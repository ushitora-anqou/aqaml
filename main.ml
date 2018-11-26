open Printf
open Scanf

let read_lines () =
  let rec aux lines =
    try
      let line = read_line () in
      aux (line :: lines)
    with End_of_file -> lines
  in
  String.concat "\n" (List.rev (aux []))

let appstr buf = ksprintf (fun str -> Buffer.add_string buf (str ^ "\n"))

let escape_string str =
  let buf = Buffer.create (String.length str) in
  let rec aux i =
    if i < String.length str then (
      ( match str.[i] with
        | '\n' -> Buffer.add_string buf "\\n"
        | '\t' -> Buffer.add_string buf "\\t"
        | '\\' -> Buffer.add_string buf "\\\\"
        | '"' -> Buffer.add_string buf "\\\""
        | ch -> Buffer.add_char buf ch ) ;
      aux (i + 1) )
  in
  aux 0 ; Buffer.contents buf

let digit x =
  match x with
  | '0' .. '9' -> int_of_char x - int_of_char '0'
  | _ -> failwith "unexpected char: not digit"

let id_counter = ref 0

let make_id base =
  id_counter := !id_counter + 1 ;
  sprintf "%s.%d" base !id_counter

type token =
  | IntLiteral of int
  | StringLiteral of string
  | Plus
  | Minus
  | Star
  | Slash
  | Ident of string
  | LParen
  | RParen
  | LRParen
  | Let
  | Equal
  | In
  | Rec
  | If
  | Then
  | Else
  | LT
  | GT
  | LTGT
  | Comma
  | LBracket
  | RBracket
  | LRBracket
  | ColonColon
  | Semicolon
  | SemicolonSemicolon

let string_of_token = function
  | IntLiteral num -> string_of_int num
  | StringLiteral str -> "\"" ^ str ^ "\""
  | Plus -> "+"
  | Minus -> "-"
  | Star -> "*"
  | Slash -> "/"
  | Ident str -> str
  | LParen -> "("
  | RParen -> ")"
  | LRParen -> "()"
  | Let -> "let"
  | Equal -> "="
  | In -> "in"
  | Rec -> "rec"
  | If -> "if"
  | Then -> "then"
  | Else -> "else"
  | LT -> "<"
  | GT -> ">"
  | LTGT -> "<>"
  | Comma -> ","
  | LBracket -> "["
  | RBracket -> "]"
  | LRBracket -> "[]"
  | ColonColon -> "::"
  | Semicolon -> ";"
  | SemicolonSemicolon -> ";;"

let rec eprint_token_list = function
  | token :: tokens ->
    eprintf "%s " (string_of_token token) ;
    eprint_token_list tokens
  | [] -> ()

exception EOF

let tokenize program =
  let rec aux i =
    let next_char i =
      if i < String.length program then (i + 1, program.[i]) else raise EOF
    in
    let rec next_int i acc =
      try
        let i, ch = next_char i in
        match ch with
        | '0' .. '9' -> next_int i ((acc * 10) + digit ch)
        | _ -> (i - 1, acc)
      with EOF -> (i, acc)
    in
    let next_ident i =
      let buf = Buffer.create 5 in
      let rec aux i =
        try
          let i, ch = next_char i in
          match ch with
          | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '\'' | '_' | '.' ->
            Buffer.add_char buf ch ; aux i
          | _ -> (i - 1, Buffer.contents buf)
        with EOF -> (i, Buffer.contents buf)
      in
      aux i
    in
    let next_string_literal i =
      let buf = Buffer.create 16 in
      let rec aux i =
        let i, ch = next_char i in
        match ch with
        | '"' -> (i, Buffer.contents buf)
        | '\\' -> (
            let i, ch = next_char i in
            match ch with
            | 'n' -> Buffer.add_char buf '\n' ; aux i
            | 't' -> Buffer.add_char buf '\t' ; aux i
            | '\\' -> Buffer.add_char buf '\\' ; aux i
            | '"' -> Buffer.add_char buf '"' ; aux i
            | ch ->
              Buffer.add_char buf '\\' ;
              aux (i - 1) )
        | _ -> Buffer.add_char buf ch ; aux i
      in
      aux i
    in
    let skip_comment i =
      let rec aux i depth =
        let i, ch = next_char i in
        match ch with
        | '(' -> (
            let i, ch = next_char i in
            match ch with '*' -> aux i (depth + 1) | _ -> aux (i - 1) depth )
        | '*' -> (
            let i, ch = next_char i in
            match ch with
            | ')' -> if depth == 1 then i else aux i (depth - 1)
            | _ -> aux (i - 1) depth )
        | _ -> aux i depth
      in
      aux i 1
    in
    try
      let i, ch = next_char i in
      match ch with
      | ' ' | '\t' | '\n' | '\r' -> aux i
      | '0' .. '9' ->
        let i, num = next_int (i - 1) 0 in
        IntLiteral num :: aux i
      | '"' ->
        let i, str = next_string_literal i in
        StringLiteral str :: aux i
      | 'a' .. 'z' | 'A' .. 'Z' ->
        let i, str = next_ident (i - 1) in
        ( match str with
          | "let" -> Let
          | "in" -> In
          | "rec" -> Rec
          | "true" -> IntLiteral 1 (* TODO: boolean type *)
          | "false" -> IntLiteral 0
          | "if" -> If
          | "then" -> Then
          | "else" -> Else
          | _ -> Ident str )
        :: aux i
      | '+' -> Plus :: aux i
      | '-' -> Minus :: aux i
      | '*' -> Star :: aux i
      | '/' -> Slash :: aux i
      | '(' -> (
          let i, ch = next_char i in
          match ch with
          | '*' ->
            let i = skip_comment i in
            aux i
          | ')' -> LRParen :: aux i
          | _ -> LParen :: aux (i - 1) )
      | ')' -> RParen :: aux i
      | '<' -> (
          let i, ch = next_char i in
          match ch with '>' -> LTGT :: aux i | _ -> LT :: aux (i - 1) )
      | '>' -> GT :: aux i
      | '=' -> Equal :: aux i
      | ',' -> Comma :: aux i
      | '[' -> (
          let i, ch = next_char i in
          match ch with
          | ']' -> LRBracket :: aux i
          | _ -> LBracket :: aux (i - 1) )
      | ']' -> RBracket :: aux i
      | ':' -> (
          let i, ch = next_char i in
          match ch with
          | ':' -> ColonColon :: aux i
          | _ -> failwith (sprintf "unexpected char: '%c'" ch) )
      | ';' -> (
          let i, ch = next_char i in
          match ch with
          | ';' -> SemicolonSemicolon :: aux i
          | _ -> Semicolon :: aux (i - 1) )
      | _ -> failwith (sprintf "unexpected char: '%c'" ch)
    with EOF -> []
  in
  aux 0

type ast =
  | IntValue of int
  | UnitValue
  | StringValue of string
  | TupleValue of ast list
  | Add of (ast * ast)
  | Sub of (ast * ast)
  | Mul of (ast * ast)
  | Div of (ast * ast)
  | StructEqual of (ast * ast)
  | StructInequal of (ast * ast)
  | LessThan of (ast * ast)
  | LessThanEqual of (ast * ast)
  | IfThenElse of (ast * ast * ast option)
  | Var of string
  | FuncCall of (ast * ast list)
  | LetVar of (pattern * ast * ast)
  | LetFunc of (bool * string * pattern list * ast * ast)
  | Cons of (ast * ast)
  | EmptyList
  | ExprSeq of ast list

and pattern = ast * string list

exception Unexpected_token

let parse tokens =
  let rec varnames_in_pattern = function
    (* TODO: much faster algorithm? *)
    | IntValue _ | UnitValue | StringValue _ | EmptyList -> []
    | Var varname -> [varname]
    | Cons (car, cdr) ->
      List.rev_append (varnames_in_pattern car) (varnames_in_pattern cdr)
    | TupleValue values ->
      List.fold_left
        (fun a b -> List.rev_append a (varnames_in_pattern b))
        [] values
    | _ -> failwith "unexpected ast"
  in
  let rec is_primary = function
    | ( IntLiteral _ | StringLiteral _ | Ident _ | LRBracket | LParen
      | LBracket | LRParen )
      :: _ ->
      true
    | _ -> false
  in
  let rec parse_primary = function
    | IntLiteral num :: tokens -> (tokens, IntValue num)
    | StringLiteral str :: tokens -> (tokens, StringValue str)
    | LRParen :: tokens -> (tokens, UnitValue)
    | Ident id :: tokens -> (tokens, Var id)
    | LRBracket :: tokens -> (tokens, EmptyList)
    | LParen :: tokens -> (
        let tokens, ast = parse_expression tokens in
        match tokens with
        | RParen :: tokens -> (tokens, ast)
        | _ -> raise Unexpected_token )
    | LBracket :: tokens ->
      let rec aux = function
        | Semicolon :: tokens ->
          let tokens, car = parse_let tokens in
          let tokens, cdr = aux tokens in
          (tokens, Cons (car, cdr))
        | RBracket :: tokens -> (tokens, EmptyList)
        | _ -> raise Unexpected_token
      in
      let tokens, car = parse_let tokens in
      let tokens, cdr = aux tokens in
      (tokens, Cons (car, cdr))
    | _ -> raise Unexpected_token
  and parse_funccall tokens =
    let rec aux tokens =
      if is_primary tokens then
        let tokens, arg = parse_primary tokens in
        let tokens, args = aux tokens in
        (tokens, arg :: args)
      else (tokens, [])
    in
    let tokens, func = parse_primary tokens in
    let tokens, args = aux tokens in
    if args = [] then (tokens, func) (* not function call *)
    else (tokens, FuncCall (func, args))
  and parse_multiplicative tokens =
    let rec aux lhs tokens =
      match tokens with
      | Star :: tokens ->
        let tokens, rhs = parse_funccall tokens in
        aux (Mul (lhs, rhs)) tokens
      | Slash :: tokens ->
        let tokens, rhs = parse_funccall tokens in
        aux (Div (lhs, rhs)) tokens
      | _ -> (tokens, lhs)
    in
    let tokens, ast = parse_funccall tokens in
    aux ast tokens
  and parse_additive tokens =
    let rec aux lhs tokens =
      match tokens with
      | Plus :: tokens ->
        let tokens, rhs = parse_multiplicative tokens in
        aux (Add (lhs, rhs)) tokens
      | Minus :: tokens ->
        let tokens, rhs = parse_multiplicative tokens in
        aux (Sub (lhs, rhs)) tokens
      | _ -> (tokens, lhs)
    in
    let tokens, ast = parse_multiplicative tokens in
    aux ast tokens
  and parse_cons tokens =
    let tokens, car = parse_additive tokens in
    match tokens with
    | ColonColon :: tokens ->
      let tokens, cdr = parse_cons tokens in
      (tokens, Cons (car, cdr))
    | _ -> (tokens, car)
  and parse_structural_equal tokens =
    let rec aux lhs tokens =
      match tokens with
      | Equal :: tokens ->
        let tokens, rhs = parse_cons tokens in
        aux (StructEqual (lhs, rhs)) tokens
      | LTGT :: tokens ->
        let tokens, rhs = parse_cons tokens in
        aux (StructInequal (lhs, rhs)) tokens
      | LT :: Equal :: tokens ->
        let tokens, rhs = parse_cons tokens in
        aux (LessThanEqual (lhs, rhs)) tokens
      | GT :: Equal :: tokens ->
        let tokens, rhs = parse_cons tokens in
        aux (LessThanEqual (rhs, lhs)) tokens
      | LT :: tokens ->
        let tokens, rhs = parse_cons tokens in
        aux (LessThan (lhs, rhs)) tokens
      | GT :: tokens ->
        let tokens, rhs = parse_cons tokens in
        aux (LessThan (rhs, lhs)) tokens
      | _ -> (tokens, lhs)
    in
    let tokens, ast = parse_cons tokens in
    aux ast tokens
  and parse_tuple tokens =
    let rec aux lhs tokens =
      match tokens with
      | Comma :: tokens ->
        let tokens, rhs = parse_structural_equal tokens in
        aux (rhs :: lhs) tokens
      | _ -> (tokens, lhs)
    in
    let tokens, ast = parse_structural_equal tokens in
    let tokens, ast_list = aux [ast] tokens in
    match ast_list with
    | [] -> raise Unexpected_token
    | [ast] -> (tokens, ast)
    | asts -> (tokens, TupleValue (List.rev asts))
  and parse_if = function
    | If :: tokens -> (
        let tokens, cond = parse_expression tokens in
        match tokens with
        | Then :: tokens -> (
            let tokens, then_body = parse_let tokens in
            match tokens with
            | Else :: tokens ->
              let tokens, else_body = parse_let tokens in
              (tokens, IfThenElse (cond, then_body, Some else_body))
            | _ -> (tokens, IfThenElse (cond, then_body, None)) )
        | _ -> raise Unexpected_token )
    | tokens -> parse_tuple tokens
  and parse_let = function
    | Let :: tokens -> (
        let tokens, recursive =
          match tokens with
          | Rec :: tokens -> (tokens, true)
          | _ -> (tokens, false)
        in
        let tokens, bind = parse_pattern tokens in
        match tokens with
        | Equal :: tokens -> (
            (* define constants *)
            let varnames = varnames_in_pattern bind in
            let tokens, lhs = parse_expression tokens in
            match tokens with
            | In :: tokens ->
              let tokens, rhs = parse_expression tokens in
              (tokens, LetVar ((bind, varnames), lhs, rhs))
            | _ -> raise Unexpected_token )
        | _ -> (
            (* define function *)
            let name =
              match bind with Var name -> name | _ -> raise Unexpected_token
            in
            let rec aux = function
              | Equal :: tokens -> (tokens, [])
              | tokens ->
                let tokens, arg = parse_pattern tokens in
                let tokens, args = aux tokens in
                (tokens, (arg, varnames_in_pattern arg) :: args)
            in
            let tokens, args = aux tokens in
            let tokens, func = parse_expression tokens in
            match tokens with
            | In :: tokens ->
              let tokens, body = parse_expression tokens in
              (tokens, LetFunc (recursive, name, args, func, body))
            | _ -> raise Unexpected_token ) )
    | tokens -> parse_if tokens
  and parse_expr_sequence tokens =
    let rec aux = function
      | Semicolon :: tokens ->
        let tokens, expr = parse_let tokens in
        let tokens, exprs = aux tokens in
        (tokens, expr :: exprs)
      | tokens -> (tokens, [])
    in
    let tokens, expr = parse_let tokens in
    let tokens, exprs = aux tokens in
    (tokens, ExprSeq (expr :: exprs))
  and parse_expression tokens = parse_expr_sequence tokens
  and parse_pattern_primary = function
    | IntLiteral num :: tokens -> (tokens, IntValue num)
    | Ident id :: tokens -> (tokens, Var id)
    | LParen :: tokens -> (
        let tokens, ast = parse_pattern tokens in
        match tokens with
        | RParen :: tokens -> (tokens, ast)
        | _ -> raise Unexpected_token )
    | LBracket :: tokens ->
      let rec aux = function
        | Semicolon :: tokens ->
          let tokens, car = parse_pattern tokens in
          let tokens, cdr = aux tokens in
          (tokens, Cons (car, cdr))
        | RBracket :: tokens -> (tokens, EmptyList)
        | _ -> raise Unexpected_token
      in
      let tokens, car = parse_pattern tokens in
      let tokens, cdr = aux tokens in
      (tokens, Cons (car, cdr))
    | _ -> raise Unexpected_token
  and parse_pattern_cons tokens =
    let tokens, car = parse_pattern_primary tokens in
    match tokens with
    | ColonColon :: tokens ->
      let tokens, cdr = parse_pattern_cons tokens in
      (tokens, Cons (car, cdr))
    | _ -> (tokens, car)
  and parse_pattern_tuple tokens =
    let rec aux lhs tokens =
      match tokens with
      | Comma :: tokens ->
        let tokens, rhs = parse_pattern_cons tokens in
        aux (rhs :: lhs) tokens
      | _ -> (tokens, lhs)
    in
    let tokens, ast = parse_pattern_cons tokens in
    let tokens, ast_list = aux [ast] tokens in
    match ast_list with
    | [] -> raise Unexpected_token
    | [ast] -> (tokens, ast)
    | asts -> (tokens, TupleValue (List.rev asts))
  and parse_pattern tokens = parse_pattern_tuple tokens in
  let parse_expressions tokens =
    (* correct? *)
    let rec aux exprs = function
      | SemicolonSemicolon :: tokens -> aux exprs tokens
      | [] -> exprs
      | tokens ->
        let tokens, expr = parse_expression tokens in
        aux (expr :: exprs) tokens
    in
    List.rev (aux [] tokens)
  in
  let exprs = parse_expressions tokens in
  (* TODO: multiple expressions *)
  List.nth exprs (List.length exprs - 1)

module HashMap = Map.Make (String)

type environment = {symbols: ast HashMap.t}

let analyze ast =
  let letfuncs = ref [] in
  let strings = ref [] in
  let rec aux env ast =
    match ast with
    | IntValue _ | UnitValue | EmptyList -> ast
    | StringValue str ->
      strings := str :: !strings ;
      ast
    | TupleValue values -> TupleValue (List.map (aux env) values)
    | Cons (car, cdr) -> Cons (aux env car, aux env cdr)
    | Add (lhs, rhs) -> Add (aux env lhs, aux env rhs)
    | Sub (lhs, rhs) -> Sub (aux env lhs, aux env rhs)
    | Mul (lhs, rhs) -> Mul (aux env lhs, aux env rhs)
    | Div (lhs, rhs) -> Div (aux env lhs, aux env rhs)
    | StructEqual (lhs, rhs) -> StructEqual (aux env lhs, aux env rhs)
    | StructInequal (lhs, rhs) -> StructInequal (aux env lhs, aux env rhs)
    | LessThan (lhs, rhs) -> LessThan (aux env lhs, aux env rhs)
    | LessThanEqual (lhs, rhs) -> LessThanEqual (aux env lhs, aux env rhs)
    | IfThenElse (cond, then_body, Some else_body) ->
      IfThenElse (aux env cond, aux env then_body, Some (aux env else_body))
    | IfThenElse (cond, then_body, None) ->
      IfThenElse (aux env cond, aux env then_body, None)
    | ExprSeq exprs -> ExprSeq (List.map (aux env) exprs)
    | Var name -> (
        try HashMap.find name env.symbols with Not_found ->
          failwith (sprintf "not found in analysis: %s" name) )
    | FuncCall (func, args) -> FuncCall (aux env func, List.map (aux env) args)
    | LetVar ((bind, varnames), lhs, rhs) ->
      let rec add_symbols symbols = function
        | varname :: varnames ->
          add_symbols (HashMap.add varname (Var varname) symbols) varnames
        | [] -> symbols
      in
      let env' = {symbols= add_symbols env.symbols varnames} in
      LetVar ((bind, varnames), aux env lhs, aux env' rhs)
    | LetFunc (recursive, funcname, args, func, body) ->
      let gen_funcname = make_id funcname in
      let funcvar = Var gen_funcname in
      let env' =
        { symbols=
            List.fold_left
              (fun symbols argname ->
                 HashMap.add argname (Var argname) symbols )
              env.symbols
              (List.fold_left
                 (fun a (_, argnames) -> List.rev_append a argnames)
                 [] args) }
      in
      (* if recursive then funcname should be in env *)
      let env' =
        { symbols=
            ( if recursive then HashMap.add funcname funcvar env'.symbols
              else env'.symbols ) }
      in
      let func = aux env' func in
      let env' = {symbols= HashMap.add funcname funcvar env.symbols} in
      let ast =
        LetFunc (recursive, gen_funcname, args, func, aux env' body)
      in
      letfuncs := ast :: !letfuncs ;
      ast
  in
  let symbols = HashMap.empty in
  let symbols = HashMap.add "String.length" (Var "String.length") symbols in
  let symbols = HashMap.add "print_string" (Var "print_string") symbols in
  let symbols = HashMap.add "exit" (Var "exit") symbols in
  let ast = aux {symbols} ast in
  let ast =
    LetFunc
      ( false
      , "aqaml_main"
      , [(Var "aqaml_main_dummy", ["aqaml_main_dummy"])]
      , ast
      , IntValue 0 )
  in
  letfuncs := ast :: !letfuncs ;
  (!letfuncs, !strings)

type gen_environment = {offset: int; varoffset: int HashMap.t}

let rec generate (letfuncs, strings) =
  let reg_of_index = function
    | 0 -> "rax"
    | 1 -> "rbx"
    | 2 -> "rdi"
    | 3 -> "rsi"
    | 4 -> "rdx"
    | 5 -> "rcx"
    | 6 -> "r8"
    | 7 -> "r9"
    | 8 -> "r12"
    | 9 -> "r13"
    | _ -> failwith "out-of-bound register"
  in
  let tag_int reg = sprintf "lea %s, [%s + %s + 1]" reg reg reg in
  let untag_int reg = sprintf "sar %s, 1" reg in
  let tagged_int num = (num lsl 1) lor 1 in
  let rec gen_alloc_block size color tag =
    (* allocated block address is in rax *)
    String.concat "\n"
      [ sprintf "mov rdi, %d" size
      ; sprintf "mov rsi, %d" color
      ; sprintf "mov rdx, %d" tag
      ; "call aqaml_alloc_block@PLT" ]
  in
  let rec gen_assign_pattern env = function
    | IntValue _ | UnitValue | StringValue _ | EmptyList -> "pop rax"
    | Var varname ->
      let offset = HashMap.find varname env.varoffset in
      String.concat "\n" ["pop rax"; sprintf "mov [rbp + %d], rax" offset]
    | Cons (car, cdr) ->
      String.concat "\n"
        [ "pop rax"
        ; "push QWORD PTR [rax]"
        ; "push QWORD PTR [rax + 8]"
        ; gen_assign_pattern env cdr
        ; gen_assign_pattern env car ]
    | TupleValue values ->
      String.concat "\n"
        [ "pop rax"
        ; String.concat "\n"
            (List.mapi
               (fun i _ -> sprintf "push QWORD PTR [rax + %d]" (i * 8))
               values)
        ; String.concat "\n"
            (List.map (gen_assign_pattern env) (List.rev values)) ]
    | _ -> failwith "unexpected ast"
  in
  let strings2id =
    let rec aux map = function
      | str :: strs -> aux (HashMap.add str (make_id "string") map) strs
      | [] -> map
    in
    aux HashMap.empty strings
  in
  let stack_size = ref 0 in
  let rec aux env = function
    | IntValue num -> sprintf "push %d" (tagged_int num)
    | UnitValue | EmptyList -> aux env (IntValue 0)
    | StringValue str ->
      sprintf "lea rax, [rip + %s]\npush rax" (HashMap.find str strings2id)
    | Cons (car, cdr) ->
      String.concat "\n"
        [ "/* Cons BEGIN */"
        ; aux env cdr
        ; aux env car
        ; gen_alloc_block 2 0 0
        ; "pop rdi" (* car *)
        ; "mov [rax], rdi"
        ; "pop rdi" (* cdr *)
        ; "mov [rax + 8], rdi"
        ; "push rax"
        ; "/* Cons END */" ]
    | TupleValue values ->
      (* +1 for header *)
      let size = List.length values in
      String.concat "\n"
        [ "/* TupleValue BEGIN */"
        ; String.concat "\n" (List.map (aux env) (List.rev values))
        ; gen_alloc_block size 0 1
        ; String.concat "\n"
            (List.mapi
               (fun i _ -> sprintf "pop rdi\nmov [rax + %d], rdi" (i * 8))
               values)
        ; "push rax"
        ; "/* TupleValue END */" ]
    | Add (lhs, rhs) ->
      String.concat "\n"
        [ aux env lhs
        ; aux env rhs
        ; "pop rdi"
        ; untag_int "rdi"
        ; "pop rax"
        ; untag_int "rax"
        ; "add rax, rdi"
        ; tag_int "rax"
        ; "push rax" ]
    | Sub (lhs, rhs) ->
      String.concat "\n"
        [ aux env lhs
        ; aux env rhs
        ; "pop rdi"
        ; untag_int "rdi"
        ; "pop rax"
        ; untag_int "rax"
        ; "sub rax, rdi"
        ; tag_int "rax"
        ; "push rax" ]
    | Mul (lhs, rhs) ->
      String.concat "\n"
        [ aux env lhs
        ; aux env rhs
        ; "pop rdi"
        ; untag_int "rdi"
        ; "pop rax"
        ; untag_int "rax"
        ; "imul rax, rdi"
        ; tag_int "rax"
        ; "push rax" ]
    | Div (lhs, rhs) ->
      String.concat "\n"
        [ aux env lhs
        ; aux env rhs
        ; "pop rdi"
        ; untag_int "rdi"
        ; "pop rax"
        ; untag_int "rax"
        ; "cqo"
        ; "idiv rdi"
        ; tag_int "rax"
        ; "push rax" ]
    | StructEqual (lhs, rhs) ->
      String.concat "\n"
        [ aux env lhs
        ; aux env rhs
        ; "pop rbx"
        ; "pop rax"
        ; "call aqaml_structural_equal"
        ; "push rax" ]
    | StructInequal (lhs, rhs) ->
      String.concat "\n"
        [ aux env lhs
        ; aux env rhs
        ; "pop rbx"
        ; "pop rax"
        ; "call aqaml_structural_inequal"
        ; "push rax" ]
    | LessThan (lhs, rhs) ->
      String.concat "\n"
        [ aux env lhs
        ; aux env rhs
        ; "pop rdi"
        ; "pop rax"
        ; "cmp rax, rdi"
        ; "setl al"
        ; "movzx rax, al"
        ; tag_int "rax"
        ; "push rax" ]
    | LessThanEqual (lhs, rhs) ->
      String.concat "\n"
        [ aux env lhs
        ; aux env rhs
        ; "pop rdi"
        ; "pop rax"
        ; "cmp rax, rdi"
        ; "setle al"
        ; "movzx rax, al"
        ; tag_int "rax"
        ; "push rax" ]
    | IfThenElse (cond, then_body, else_body) ->
      let false_label = make_id ".L" in
      let exit_label = make_id ".L" in
      String.concat "\n"
        [ aux env cond
        ; "pop rax"
        ; "cmp rax, 1" (* if rax = 0 then then_body else else_body *)
        ; sprintf "je %s" false_label
        ; aux env then_body
        ; sprintf "jmp %s" exit_label
        ; sprintf "%s:" false_label
        ; ( match else_body with
            | None -> aux env (IntValue 0) (* unit value is IntValue 0 *)
            | Some else_body -> aux env else_body )
        ; sprintf "%s:" exit_label ]
    | ExprSeq exprs -> String.concat "\npop rax\n" (List.map (aux env) exprs)
    | Var name when name = "String.length" ->
      String.concat "\n" ["lea rax, [rip + aqaml_string_length]"; "push rax"]
    | Var name when name = "print_string" ->
      String.concat "\n" ["lea rax, [rip + aqaml_print_string]"; "push rax"]
    | Var name when name = "exit" ->
      String.concat "\n" ["lea rax, [rip + aqaml_exit]"; "push rax"]
    | Var varname -> (
        try
          let offset = HashMap.find varname env.varoffset in
          String.concat "\n" [sprintf "mov rax, [rbp + %d]" offset; "push rax"]
        with Not_found -> failwith (sprintf "not found in analysis: %s" varname)
      )
    | FuncCall (func, args) ->
      String.concat "\n"
        [ aux env func
        ; String.concat "\n" (List.map (aux env) (List.rev args))
        ; String.concat "\n"
            (List.map
               (fun (_, reg) -> "pop " ^ reg)
               (List.filter
                  (fun (index, reg) -> index < List.length args)
                  [ (0, "rax")
                  ; (1, "rbx")
                  ; (2, "rdi")
                  ; (3, "rsi")
                  ; (4, "rdx")
                  ; (5, "rcx")
                  ; (6, "r8")
                  ; (7, "r9")
                  ; (8, "r12")
                  ; (9, "r13") ]))
        ; "pop r10"
        ; "call r10"
        ; "push rax" ]
    | LetVar ((bind, varnames), lhs, rhs) ->
      let offset = env.offset - (List.length varnames * 8) in
      stack_size := max !stack_size (-offset) ;
      let env' =
        { offset
        ; varoffset=
            (let rec aux i varoffset = function
                | varname :: varnames ->
                  aux (i + 1)
                    (HashMap.add varname (env.offset - (i * 8)) varoffset)
                    varnames
                | [] -> varoffset
             in
             aux 1 env.varoffset varnames) }
      in
      let assign_code = gen_assign_pattern env' bind in
      String.concat "\n" [aux env lhs; assign_code; aux env' rhs]
    | LetFunc (_, funcname, _, _, body) ->
      let offset = env.offset - 8 in
      stack_size := max !stack_size (-offset) ;
      let env =
        {offset; varoffset= HashMap.add funcname offset env.varoffset}
      in
      String.concat "\n"
        [ sprintf "lea rax, [rip + %s]" funcname
        ; sprintf "mov [rbp + %d], rax" offset
        ; aux env body ]
  in
  let strings_code =
    let buf = Buffer.create 80 in
    appstr buf ".data" ;
    List.iter
      (fun str ->
         let id = HashMap.find str strings2id in
         let size = (String.length str / 8) + 1 in
         let space = 7 - (String.length str mod 8) in
         appstr buf ".quad %d" ((size lsl 10) lor (0 lsl 8) lor 252) ;
         appstr buf "%s:" id ;
         appstr buf ".ascii \"%s\"" (escape_string str) ;
         if space <> 0 then appstr buf ".space %d" space ;
         appstr buf ".byte %d" space )
      strings ;
    appstr buf ".text\n" ;
    Buffer.contents buf
  in
  let letfuncs_code =
    String.concat "\n"
      (List.map
         (function
           | LetFunc (recursive, funcname, args, func, _) ->
             let env = {offset= 0; varoffset= HashMap.empty} in
             let env =
               List.fold_left
                 (fun env argname ->
                    let offset = env.offset - 8 in
                    let varoffset =
                      HashMap.add argname offset env.varoffset
                    in
                    {offset; varoffset} )
                 env
                 (List.fold_left
                    (fun a (_, argnames) -> List.rev_append a argnames)
                    [] args)
             in
             (* if recursive then the function itself should be in env *)
             let env =
               if recursive then
                 let offset = env.offset - 8 in
                 { offset
                 ; varoffset= HashMap.add funcname offset env.varoffset }
               else env
             in
             stack_size := -env.offset ;
             let code = aux env func in
             String.concat "\n"
               [ sprintf "/* %s(%d) */"
                   (if recursive then "recursive" else "")
                   (List.length args)
               ; funcname ^ ":"
               ; "push rbp"
               ; "mov rbp, rsp"
               ; sprintf "sub rsp, %d" !stack_size
               ; String.concat "\n"
                   (List.rev
                      (List.mapi
                         (fun i _ -> sprintf "push %s" (reg_of_index i))
                         args))
               ; String.concat "\n"
                   (List.map
                      (fun (ptn, _) -> gen_assign_pattern env ptn)
                      args)
               ; ( if recursive then
                     sprintf "lea rax, [rip + %s]\nmov [rbp + %d], rax"
                       funcname env.offset
                   else "nop" )
               ; code
               ; "pop rax"
               ; "mov rsp, rbp"
               ; "pop rbp"
               ; "ret\n\n" ]
           | _ -> failwith "LetFunc should be here")
         letfuncs)
  in
  let main_code =
    String.concat "\n"
      [ "aqaml_malloc:"
      ; untag_int "rax"
      ; "mov edi, eax"
      ; "call aqaml_malloc_detail@PLT"
      ; "ret"
      ; ""
      ; "aqaml_structural_equal:"
      ; "mov rdi, rax"
      ; "mov rsi, rbx"
      ; "call aqaml_structural_equal_detail@PLT"
      ; tag_int "rax"
      ; "ret"
      ; ""
      ; "aqaml_structural_inequal:"
      ; "mov rdi, rax"
      ; "mov rsi, rbx"
      ; "call aqaml_structural_equal_detail@PLT"
      ; "test eax, eax" (* eax == 0 *)
      ; "sete al"
      ; tag_int "rax"
      ; "ret"
      ; ""
      ; "aqaml_string_length:"
      ; "mov rdi, rax"
      ; "call aqaml_string_length_detail@PLT"
      ; tag_int "rax"
      ; "ret"
      ; ""
      ; "aqaml_print_string:"
      ; "mov rdi, rax"
      ; "call aqaml_print_string_detail@PLT"
      ; "mov rax, 1" (* return unit value *)
      ; "ret"
      ; ""
      ; "aqaml_exit:"
      ; "mov rdi, rax"
      ; "shr rdi, 1"
      ; "call exit@PLT"
      ; ""
      ; "main:"
      ; "call aqaml_main"
      ; "sar rax, 1"
      ; "ret\n\n" ]
  in
  main_code ^ letfuncs_code ^ strings_code

;;
let program = read_lines () in
let tokens = tokenize program in
(* eprint_token_list tokens ; *)
let ast = parse tokens in
let code = generate (analyze ast) in
print_string
  (String.concat "\n" [".intel_syntax noprefix"; ".global main"; code])
