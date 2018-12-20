open Printf
open Scanf
module HashMap = Map.Make (String)

let is_capital = function 'A' .. 'Z' -> true | _ -> false

let append_to_list_ref x xs = xs := x :: !xs

let string_of_list src = "[" ^ String.concat "; " src ^ "]"

let hashmap_of_list src =
  let hashmap = ref HashMap.empty in
  List.iter (fun (k, v) -> hashmap := HashMap.add k v !hashmap) src ;
  !hashmap

let integrate od nw = HashMap.union (fun _ _ r -> Some r) od nw

let read_lines () =
  let rec aux lines =
    try
      let line = read_line () in
      aux (line :: lines)
    with End_of_file -> lines
  in
  String.concat "\n" (List.rev (aux []))

let appfmt buf = ksprintf (fun str -> Buffer.add_string buf (str ^ "\n"))

let appstr buf str = Buffer.add_string buf (str ^ "\n")

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

let make_label () = make_id ".L"

type token =
  | IntLiteral of int
  | CharLiteral of char
  | StringLiteral of string * string
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
  | Match
  | With
  | Arrow
  | Bar
  | Fun
  | Function
  | As
  | When
  | Type
  | Dot
  | Of
  | KwInt
  | KwChar
  | KwString
  | Apostrophe

let string_of_token = function
  | IntLiteral num -> string_of_int num
  | CharLiteral ch -> "'" ^ String.make 1 ch ^ "'"
  | StringLiteral (_, str) -> "\"" ^ str ^ "\""
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
  | Match -> "match"
  | With -> "with"
  | Arrow -> "->"
  | Bar -> "|"
  | Fun -> "fun"
  | Function -> "function"
  | As -> "as"
  | When -> "when"
  | Type -> "type"
  | Dot -> "."
  | Of -> "of"
  | KwInt -> "int"
  | KwChar -> "char"
  | KwString -> "string"
  | Apostrophe -> "'"

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
          | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '\'' | '_' ->
              Buffer.add_char buf ch ; aux i
          | _ -> (i - 1, Buffer.contents buf)
        with EOF -> (i, Buffer.contents buf)
      in
      aux i
    in
    let next_char_literal i =
      let i, ch = next_char i in
      match ch with
      | '\\' -> (
          let i, ch = next_char i in
          ( i + 1
          , match ch with
            | 'n' -> '\n'
            | 't' -> '\t'
            | '\\' -> '\\'
            | '"' -> '"'
            | '\'' -> '\''
            | _ -> failwith "unexpected char in char literal" ) )
      | ch -> (i + 1, ch)
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
      | '\'' -> (
          let _, ch0 = next_char i in
          let _, ch1 = next_char (i + 1) in
          match (ch0, ch1) with
          | _, '\'' | '\\', _ ->
              let i, ch = next_char_literal i in
              CharLiteral ch :: aux i
          | _ -> Apostrophe :: aux i )
      | '"' ->
          let i, str = next_string_literal i in
          StringLiteral (make_id "string", str) :: aux i
      | 'a' .. 'z' | 'A' .. 'Z' | '_' ->
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
          | "match" -> Match
          | "with" -> With
          | "fun" -> Fun
          | "function" -> Function
          | "as" -> As
          | "when" -> When
          | "type" -> Type
          | "of" -> Of
          | "int" -> KwInt
          | "char" -> KwChar
          | "string" -> KwString
          | _ -> Ident str )
          :: aux i
      | '+' -> Plus :: aux i
      | '*' -> Star :: aux i
      | '/' -> Slash :: aux i
      | ')' -> RParen :: aux i
      | '>' -> GT :: aux i
      | '=' -> Equal :: aux i
      | ',' -> Comma :: aux i
      | ']' -> RBracket :: aux i
      | '|' -> Bar :: aux i
      | '.' -> Dot :: aux i
      | '-' -> (
          let i, ch = next_char i in
          match ch with '>' -> Arrow :: aux i | _ -> Minus :: aux (i - 1) )
      | '<' -> (
          let i, ch = next_char i in
          match ch with '>' -> LTGT :: aux i | _ -> LT :: aux (i - 1) )
      | '[' -> (
          let i, ch = next_char i in
          match ch with
          | ']' -> LRBracket :: aux i
          | _ -> LBracket :: aux (i - 1) )
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
      | '(' -> (
          let i, ch = next_char i in
          match ch with
          | '*' ->
              let i = skip_comment i in
              aux i
          | ')' -> LRParen :: aux i
          | _ -> LParen :: aux (i - 1) )
      | _ -> failwith (sprintf "unexpected char: '%c'" ch)
    with EOF -> []
  in
  aux 0

type typ =
  | TyInt
  | TyChar
  | TyString
  | TyTuple of typ list
  | TyCustom of string
  | TyVar of string
  | TyCtorApp of (typ * string)

type ast =
  | UnitValue
  | IntValue of int
  | CharValue of char
  | StringValue of string * string
  | TupleValue of ast list
  | Add of (ast * ast)
  | Sub of (ast * ast)
  | Mul of (ast * ast)
  | Div of (ast * ast)
  | Negate of ast
  | Positate of ast
  | StructEqual of (ast * ast)
  | StructInequal of (ast * ast)
  | LessThan of (ast * ast)
  | LessThanEqual of (ast * ast)
  | IfThenElse of (ast * ast * ast option)
  | Var of string
  | FuncVar of string * int
  | AppCls of (ast * ast list)
  | AppDir of (string * ast list)
  | LetVar of (bool * pattern * ast * ast)
  (* recursive?, funcname, args, function body, following body, free variables *)
  | LetFunc of (bool * string * pattern list * ast * ast * string list)
  | Cons of (ast * ast)
  | EmptyList
  | ExprSeq of ast list
  | MatchWith of ast * (pattern * ast option * ast) list
  | MakeCls of string * int * string list
  | Lambda of pattern list * ast
  (* TODO: module Ptn *)
  | PtnOr of pattern * pattern
  | PtnAlias of pattern * ast
  | TypeDef of typ option * string * (string * typ option) list
  | CtorApp of string option * string * ast option

and pattern = ast

let rec varnames_in_pattern = function
  (* TODO: much faster algorithm? *)
  | UnitValue | IntValue _ | CharValue _ | StringValue _ | EmptyList -> []
  | Var varname -> [varname]
  | Cons (car, cdr) ->
      List.rev_append (varnames_in_pattern car) (varnames_in_pattern cdr)
  | TupleValue values ->
      List.fold_left
        (fun a b -> List.rev_append a (varnames_in_pattern b))
        [] values
  | PtnOr (lhs, rhs) ->
      List.rev_append (varnames_in_pattern lhs) (varnames_in_pattern rhs)
  | PtnAlias (ptn, Var name) -> name :: varnames_in_pattern ptn
  | CtorApp (_, _, None) -> []
  | CtorApp (_, _, Some arg) -> varnames_in_pattern arg
  | _ -> failwith "unexpected ast"

exception Unexpected_token

let parse tokens =
  let is_primary = function
    | ( IntLiteral _ | CharLiteral _ | StringLiteral _ | Ident _ | LRBracket
      | LParen | LBracket | LRParen )
      :: _ ->
        true
    | _ -> false
  in
  let rec parse_primary = function
    | IntLiteral num :: tokens -> (tokens, IntValue num)
    | CharLiteral ch :: tokens -> (tokens, CharValue ch)
    | StringLiteral (id, str) :: tokens -> (tokens, StringValue (id, str))
    | LRParen :: tokens -> (tokens, UnitValue)
    | Ident modname :: Dot :: Ident varname :: tokens ->
        (tokens, Var (modname ^ "." ^ varname))
    | Ident id :: tokens ->
        (tokens, if is_capital id.[0] then CtorApp (None, id, None) else Var id)
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
    else (tokens, AppCls (func, args))
  and parse_unary = function
    | Minus :: tokens ->
        let tokens, ast = parse_unary tokens in
        (tokens, Negate ast)
    | Plus :: tokens ->
        let tokens, ast = parse_unary tokens in
        (tokens, Positate ast)
    | tokens -> parse_funccall tokens
  and parse_multiplicative tokens =
    let rec aux lhs tokens =
      match tokens with
      | Star :: tokens ->
          let tokens, rhs = parse_unary tokens in
          aux (Mul (lhs, rhs)) tokens
      | Slash :: tokens ->
          let tokens, rhs = parse_unary tokens in
          aux (Div (lhs, rhs)) tokens
      | _ -> (tokens, lhs)
    in
    let tokens, ast = parse_unary tokens in
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
  and parse_pattern_match tokens =
    let rec aux first cases tokens =
      let aux' tokens =
        let tokens, ptn = parse_pattern tokens in
        let tokens, whn =
          match tokens with
          | When :: tokens ->
              let tokens, expr = parse_expression tokens in
              (tokens, Some expr)
          | _ -> (tokens, None)
        in
        match tokens with
        | Arrow :: tokens ->
            let tokens, case = parse_expression tokens in
            let tokens, cases = aux false ((ptn, whn, case) :: cases) tokens in
            (tokens, cases)
        | _ -> raise Unexpected_token
      in
      match tokens with
      | Bar :: tokens -> aux' tokens
      | _ -> if first then aux' tokens else (tokens, List.rev cases)
    in
    aux true [] tokens
  and parse_let = function
    | Function :: tokens ->
        let argname = ".arg" in
        let tokens, cases = parse_pattern_match tokens in
        (tokens, Lambda ([Var argname], MatchWith (Var argname, cases)))
    | Fun :: tokens ->
        let rec aux = function
          | Arrow :: tokens -> (tokens, [])
          | tokens ->
              let tokens, arg = parse_pattern tokens in
              let tokens, args = aux tokens in
              (tokens, arg :: args)
        in
        let tokens, args = aux tokens in
        let tokens, func = parse_expression tokens in
        (tokens, Lambda (args, func))
    | Match :: tokens -> (
        let tokens, cond = parse_expression tokens in
        match tokens with
        | With :: tokens ->
            let tokens, cases = parse_pattern_match tokens in
            (tokens, MatchWith (cond, cases))
        | _ -> raise Unexpected_token )
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
            let tokens, lhs = parse_expression tokens in
            match tokens with
            | In :: tokens ->
                let tokens, rhs = parse_expression tokens in
                (tokens, LetVar (recursive, bind, lhs, rhs))
            | _ ->
                let tokens, exprs = parse_expressions tokens in
                (tokens, LetVar (recursive, bind, lhs, exprs)) )
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
                  (tokens, arg :: args)
            in
            let tokens, args = aux tokens in
            let tokens, func = parse_expression tokens in
            match tokens with
            | In :: tokens ->
                let tokens, body = parse_expression tokens in
                (tokens, LetFunc (recursive, name, args, func, body, []))
            | _ ->
                let tokens, exprs = parse_expressions tokens in
                (tokens, LetFunc (recursive, name, args, func, exprs, [])) ) )
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
    if List.length exprs = 0 then (tokens, expr)
    else (tokens, ExprSeq (expr :: exprs))
  and parse_expression tokens = parse_expr_sequence tokens
  and parse_pattern_primary = function
    | IntLiteral num :: tokens -> (tokens, IntValue num)
    | CharLiteral ch :: tokens -> (tokens, CharValue ch)
    | StringLiteral (id, str) :: tokens -> (tokens, StringValue (id, str))
    | LRParen :: tokens -> (tokens, UnitValue)
    | Ident id :: tokens ->
        (tokens, if is_capital id.[0] then CtorApp (None, id, None) else Var id)
    | LRBracket :: tokens -> (tokens, EmptyList)
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
  and parse_pattern_ctor_app tokens =
    let tokens, ctorapp = parse_pattern_primary tokens in
    match ctorapp with
    | CtorApp (None, ctorname, None) when is_primary tokens ->
        let tokens, arg = parse_pattern_primary tokens in
        (tokens, CtorApp (None, ctorname, Some arg))
    | _ -> (tokens, ctorapp)
  and parse_pattern_cons tokens =
    let tokens, car = parse_pattern_ctor_app tokens in
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
  and parse_pattern_or tokens =
    let rec aux lhs = function
      | Bar :: tokens ->
          let tokens, rhs = parse_pattern_tuple tokens in
          aux (PtnOr (lhs, rhs)) tokens
      | tokens -> (tokens, lhs)
    in
    let tokens, lhs = parse_pattern_tuple tokens in
    aux lhs tokens
  and parse_pattern_as tokens =
    let tokens, ptn = parse_pattern_or tokens in
    match tokens with
    | As :: Ident name :: tokens -> (tokens, PtnAlias (ptn, Var name))
    | _ -> (tokens, ptn)
  and parse_pattern tokens = parse_pattern_as tokens
  and parse_typexpr_primary = function
    | KwInt :: tokens -> (tokens, TyInt)
    | KwChar :: tokens -> (tokens, TyChar)
    | KwString :: tokens -> (tokens, TyString)
    | Apostrophe :: Ident id :: tokens -> (tokens, TyVar id)
    | Ident typename :: tokens -> (tokens, TyCustom typename)
    | LParen :: tokens -> (
        let tokens, typ = parse_typexpr tokens in
        match tokens with
        | RParen :: tokens -> (tokens, typ)
        | _ -> raise Unexpected_token )
    | _ -> raise Unexpected_token
  and parse_typexpr_ctor_app tokens =
    let rec aux typexpr = function
      | Ident typectorname :: tokens ->
          aux (TyCtorApp (typexpr, typectorname)) tokens
      | tokens -> (tokens, typexpr)
    in
    let tokens, typexpr = parse_typexpr_primary tokens in
    aux typexpr tokens
  and parse_typexpr_tuple tokens =
    let rec aux lhs = function
      | Star :: tokens ->
          let tokens, rhs = parse_typexpr_ctor_app tokens in
          aux (rhs :: lhs) tokens
      | tokens -> (tokens, lhs)
    in
    let tokens, typexpr = parse_typexpr_ctor_app tokens in
    let tokens, typexprs = aux [typexpr] tokens in
    match typexprs with
    | [] -> raise Unexpected_token
    | [typexpr] -> (tokens, typexpr)
    | typexprs -> (tokens, TyTuple typexprs)
  and parse_typexpr tokens = parse_typexpr_tuple tokens
  and parse_type_def tokens =
    (* token Type is already fetched *)
    let aux type_param = function
      | Ident typename :: Equal :: tokens ->
          let rec aux first ctors = function
            | Bar :: Ident ctorname :: Of :: tokens ->
                let tokens, typ = parse_typexpr tokens in
                aux false ((ctorname, Some typ) :: ctors) tokens
            | Ident ctorname :: Of :: tokens when first ->
                let tokens, typ = parse_typexpr tokens in
                aux false ((ctorname, Some typ) :: ctors) tokens
            | Bar :: Ident ctorname :: tokens ->
                aux false ((ctorname, None) :: ctors) tokens
            | Ident ctorname :: tokens when first ->
                aux false ((ctorname, None) :: ctors) tokens
            | tokens -> (tokens, ctors)
          in
          let tokens, ctors = aux true [] tokens in
          (tokens, TypeDef (type_param, typename, ctors))
      | _ -> raise Unexpected_token
    in
    let parse_type_param = function
      | Apostrophe :: Ident id :: tokens -> (tokens, TyVar id)
      | _ -> raise Unexpected_token
    in
    let parse_type_params = function
      | LParen :: tokens ->
          let rec aux type_params = function
            | Comma :: tokens ->
                let tokens, type_param = parse_type_param tokens in
                aux (type_param :: type_params) tokens
            | RParen :: tokens -> (tokens, TyTuple type_params)
            | _ -> raise Unexpected_token
          in
          let tokens, type_param = parse_type_param tokens in
          Some (aux [type_param] tokens)
      | Apostrophe :: _ as tokens -> Some (parse_type_param tokens)
      | _ -> None
    in
    match parse_type_params tokens with
    | None -> aux None tokens
    | Some (tokens, type_params) -> aux (Some type_params) tokens
  and parse_expressions tokens =
    (* Here are some tricks. All expressions split by double semicolons (;;)
     * are converted to (maybe large) one ExprSeq, and all 'let' without 'in'
     * come to have their following expressions as their 'in' part.
     * This change makes later processes such as semantic analysis easier. *)
    (* TODO: correct? *)
    let rec aux exprs = function
      | SemicolonSemicolon :: tokens -> aux exprs tokens
      | [] -> exprs
      | Type :: tokens ->
          let tokens, expr = parse_type_def tokens in
          aux (expr :: exprs) tokens
      | tokens ->
          let tokens, expr = parse_expression tokens in
          aux (expr :: exprs) tokens
    in
    ([], ExprSeq (List.rev (aux [] tokens)))
  in
  let _, expr = parse_expressions tokens in
  expr

type environment =
  { symbols: ast HashMap.t
  ; parent: environment option
  ; freevars: (string * string) list ref }

let add_symbols_in_pattern symbols ptn =
  integrate symbols @@ hashmap_of_list
  @@ List.map (fun n -> (n, Var (make_id n)))
  @@ varnames_in_pattern ptn

let add_symbols_in_patterns symbols ptns =
  integrate symbols @@ hashmap_of_list
  @@ List.map (fun n -> (n, Var (make_id n)))
  @@ List.flatten
  @@ List.map varnames_in_pattern ptns

type type_toplevel =
  { letfuncs: ast list ref
  ; strings: ast list ref
  ; ctors_type: (string, string) Hashtbl.t }

let analyze ast =
  let toplevel =
    {letfuncs= ref []; strings= ref []; ctors_type= Hashtbl.create 16}
  in
  let find_symbol env name =
    let rec aux depth env =
      try (depth, HashMap.find name env.symbols) with Not_found -> (
        match env.parent with
        | Some parent -> aux (depth + 1) parent
        | None ->
            failwith (sprintf "not found in analysis (find_symbol): %s" name) )
    in
    aux 0 env
  in
  let rec aux_ptn env ptn =
    match ptn with
    | IntValue _ | CharValue _ | UnitValue | EmptyList -> ptn
    | StringValue _ ->
        append_to_list_ref ptn toplevel.strings ;
        ptn
    | TupleValue values -> TupleValue (List.map (aux_ptn env) values)
    | Cons (car, cdr) -> Cons (aux_ptn env car, aux_ptn env cdr)
    | Var name -> (
      match find_symbol env name with
      | 0, sym -> sym
      | _ -> failwith "[FATAL] variable not found in pattern analysis" )
    | PtnAlias (ptn, (Var _ as var)) ->
        PtnAlias (aux_ptn env ptn, aux_ptn env var)
    | PtnOr (lhs, rhs) -> PtnOr (aux_ptn env lhs, aux_ptn env rhs)
    | CtorApp (None, ctorname, arg) ->
        let arg =
          match arg with Some arg -> Some (aux_ptn env arg) | _ -> None
        in
        CtorApp
          (Some (Hashtbl.find toplevel.ctors_type ctorname), ctorname, arg)
    | _ -> failwith "unexpected pattern"
  in
  let rec aux env ast =
    match ast with
    | IntValue _ | CharValue _ | UnitValue | EmptyList -> ast
    | StringValue _ ->
        append_to_list_ref ast toplevel.strings ;
        ast
    | TupleValue values -> TupleValue (List.map (aux env) values)
    | Cons (car, cdr) -> Cons (aux env car, aux env cdr)
    | Add (lhs, rhs) -> Add (aux env lhs, aux env rhs)
    | Sub (lhs, rhs) -> Sub (aux env lhs, aux env rhs)
    | Mul (lhs, rhs) -> Mul (aux env lhs, aux env rhs)
    | Div (lhs, rhs) -> Div (aux env lhs, aux env rhs)
    | Negate ast -> Negate (aux env ast)
    | Positate ast -> Positate (aux env ast)
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
      match find_symbol env name with
      | 0, (Var _ as sym) -> sym
      | 0, FuncVar (funcname, nargs) ->
          (* When FuncVar is processed here, AppDir will not be applied to this FuncVar.
         * Therefore the returned value should be closured in case
         * AppCls is applied to this value. *)
          MakeCls (funcname, nargs, [])
      | _, (Var id as sym) ->
          env.freevars := (name, id) :: !(env.freevars) ;
          sym
      | _ -> failwith @@ sprintf "not found variable in analysis: %s" name )
    | Lambda (args, body) ->
        let funcname = ".lambda" in
        aux env (LetFunc (false, funcname, args, body, Var funcname, []))
    | CtorApp (None, ctorname, None) ->
        CtorApp
          (Some (Hashtbl.find toplevel.ctors_type ctorname), ctorname, None)
    | AppCls ((CtorApp (None, ctorname, None) as ctor), args) -> (
      match aux env ctor with
      | CtorApp (typename, ctorname, None) when List.length args = 1 ->
          CtorApp (typename, ctorname, Some (aux env @@ List.hd args))
      | _ -> failwith "invalid CtorApp" )
    | AppCls ((Var funcname as var), args) -> (
      try
        match
          match find_symbol env funcname with
          (* the symbol is 'safe' when it's in the same env
           * or it can be called by its name *)
          | 0, sym | _, (FuncVar _ as sym) -> sym
          | _, (Var id as sym) ->
              env.freevars := (funcname, id) :: !(env.freevars) ;
              sym
          | _ ->
              failwith @@ sprintf "not found variable in analysis: %s" funcname
        with
        | FuncVar (gen_funcname, nargs) ->
            let args = List.map (aux env) args in
            if List.length args = nargs then AppDir (gen_funcname, args)
            else
              let rec split n lst =
                if n = 0 then ([], lst)
                else
                  match lst with
                  | x :: xs ->
                      let lhs, rhs = split (n - 1) xs in
                      (x :: lhs, rhs)
                  | [] -> failwith "n > List.length lst"
              in
              let head, tail = split nargs args in
              AppCls (AppDir (gen_funcname, head), tail)
        | Var varname -> AppCls (aux env var, List.map (aux env) args)
        | _ -> raise Not_found
      with Not_found ->
        failwith (sprintf "not found in analysis (AppCls): %s" funcname) )
    | AppCls (func, args) -> AppCls (aux env func, List.map (aux env) args)
    | LetVar (recursive, bind, lhs, rhs) ->
        if recursive then
          (* When recursive, LetVar should be LetFunc with no arguments. *)
          (* TODO:
            If the lhs doesn't have any freevars, then there is no need to convert it.
            Also, we should check whether the lhs uses itself in a correct way e.g.
                let rec length = function x :: xs -> 1 + length xs | [] -> 0;;
            is okay, but
                let rec id x = id;;
            is ng. For now, we assume that 'let rec ...' expression is written properly.
          *)
          aux env
            (LetFunc
               ( recursive
               , ( match bind with
                 | Var funcname -> funcname
                 | _ -> failwith "when recursive only Var is allowed in 'let'"
                 )
               , []
               , lhs
               , rhs
               , [] ))
        else
          let env' =
            {env with symbols= add_symbols_in_pattern env.symbols bind}
          in
          LetVar (false, aux_ptn env' bind, aux env lhs, aux env' rhs)
    | LetFunc (recursive, funcname, args, func, body, _) ->
        let toplevel_backup = toplevel in
        let gen_funcname = make_id funcname in
        let rec analyze_func first =
          let funcvar =
            (* Try FuncVar first, that is, at first
             * we try to call this function by name.
             * That means we assume that the function is not recursive,
             * or recursive and has no freevars.
             * If we find we can't,
             * then Var is used, which means it should be called as closure *)
            if first then FuncVar (gen_funcname, List.length args)
            else Var gen_funcname
          in
          let env_in =
            { symbols= add_symbols_in_patterns HashMap.empty args
            ; parent= Some env
            ; freevars= ref [] }
          in
          (* if recursive then funcname should be in env *)
          let env_in =
            { env_in with
              symbols=
                ( if recursive then HashMap.add funcname funcvar env_in.symbols
                else env_in.symbols ) }
          in
          let func = aux env_in func in
          let freevars = List.map (fun (_, a) -> a) !(env_in.freevars) in
          if first && recursive && List.length freevars <> 0 then (
            (* restore toplevel *)
            (* TODO: is there any better way? *)
            toplevel.letfuncs := !(toplevel_backup.letfuncs) ;
            toplevel.strings := !(toplevel_backup.strings) ;
            analyze_func false )
          else if not first then
            (* The function that is recursive and has freevars should be called as closure
             * IN THAT FUNCTION ITSELF. (Of course outside too, but here that's not important. *)
            ( env_in
            , LetVar
                ( false
                , funcvar
                , MakeCls (gen_funcname, List.length args, freevars)
                , func )
            , freevars )
          else
            (* The function don't have to be called as closure, AT LEAST IN THAT FUNCTION ITSELF. *)
            (env_in, func, freevars)
        in
        (* TODO: duplicated freevars *)
        let env_in, func, freevars = analyze_func true in
        (* freevars are passed to env if they are not defined in env *)
        List.iter
          (fun ((name, _) as var) ->
            let d, _ = find_symbol env name in
            if d <> 0 then env.freevars := var :: !(env.freevars) )
          !(env_in.freevars) ;
        if List.length freevars = 0 then (
          (* no freevars; no need for closure *)
          let env_out =
            { env with
              symbols=
                HashMap.add funcname
                  (FuncVar (gen_funcname, List.length args))
                  env.symbols }
          in
          let ast =
            LetFunc
              ( recursive
              , gen_funcname
              , List.map (aux_ptn env_in) args
              , func
              , aux env_out body
              , freevars )
          in
          append_to_list_ref ast toplevel.letfuncs ;
          ast )
        else
          (* closure *)
          let funcvar = Var gen_funcname in
          let env_out =
            {env with symbols= HashMap.add funcname funcvar env.symbols}
          in
          let ast =
            LetFunc
              ( recursive
              , gen_funcname
              , List.map (aux_ptn env_in) args
              , func
              , UnitValue
              , freevars )
          in
          append_to_list_ref ast toplevel.letfuncs ;
          LetVar
            ( false
            , funcvar
            , MakeCls (gen_funcname, List.length args, freevars)
            , aux env_out body )
    | MatchWith (cond, cases) ->
        MatchWith
          ( aux env cond
          , List.map
              (fun (ptn, whn, ast) ->
                let env' =
                  {env with symbols= add_symbols_in_pattern env.symbols ptn}
                in
                ( aux_ptn env' ptn
                , ( match whn with
                  | Some expr -> Some (aux env' expr)
                  | None -> None )
                , aux env' ast ) )
              cases )
    | TypeDef (type_param, typename, ctornames) ->
        let typename = make_id typename in
        List.iter
          (fun (ctorname, _) ->
            Hashtbl.add toplevel.ctors_type ctorname typename )
          ctornames ;
        TypeDef (type_param, typename, ctornames)
    | _ -> failwith "unexpected ast"
  in
  let env =
    { symbols=
        hashmap_of_list
        @@ [ ("String.length", FuncVar ("aqaml_string_length", 1))
           ; ("print_string", FuncVar ("aqaml_print_string", 1))
           ; ("exit", FuncVar ("aqaml_exit", 1)) ]
    ; parent= None
    ; freevars= ref [] }
  in
  let ast =
    LetFunc (false, "aqaml_main", [UnitValue], aux env ast, UnitValue, [])
  in
  append_to_list_ref ast toplevel.letfuncs ;
  (!(toplevel.letfuncs), !(toplevel.strings))

type gen_environment = {offset: int; varoffset: int HashMap.t}

let rec generate (letfuncs, strings) =
  let stack_size = ref 0 in
  let ctors_id = Hashtbl.create 16 in
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
  let rec gen_assign_pattern env exp_label = function
    | UnitValue | EmptyList -> gen_assign_pattern env exp_label @@ IntValue 0
    | IntValue num ->
        let buf = Buffer.create 128 in
        let exit_label = make_label () in
        appstr buf "pop rax" ;
        appfmt buf "cmp rax, %d" @@ tagged_int num ;
        appfmt buf "je %s" exit_label ;
        appfmt buf "jmp %s" exp_label ;
        appfmt buf "%s:" exit_label ;
        Buffer.contents buf
    | CharValue ch ->
        gen_assign_pattern env exp_label @@ IntValue (Char.code ch)
    | StringValue (id, str) ->
        let buf = Buffer.create 128 in
        appstr buf "pop rax" ;
        appfmt buf "lea rbx, [rip + %s]" id ;
        appstr buf "call aqaml_structural_equal" ;
        appstr buf "cmp rax, 1" ;
        appfmt buf "je %s" exp_label ;
        Buffer.contents buf
    | Var varname ->
        let buf = Buffer.create 128 in
        let offset = HashMap.find varname env.varoffset in
        appstr buf "pop rax" ;
        appfmt buf "mov [rbp + %d], rax" offset ;
        Buffer.contents buf
    | Cons (car, cdr) ->
        String.concat "\n"
          [ "pop rax"
          ; "cmp rax, 1"
          ; "je " ^ exp_label
          ; "push QWORD PTR [rax]"
          ; "push QWORD PTR [rax + 8]"
          ; gen_assign_pattern env exp_label cdr
          ; gen_assign_pattern env exp_label car ]
    | TupleValue values ->
        String.concat "\n"
          [ "pop rax"
          ; String.concat "\n"
              (List.mapi
                 (fun i _ -> sprintf "push QWORD PTR [rax + %d]" (i * 8))
                 values)
          ; String.concat "\n"
              (List.map (gen_assign_pattern env exp_label) (List.rev values))
          ]
    | PtnAlias (ptn, Var varname) ->
        let buf = Buffer.create 128 in
        let offset = HashMap.find varname env.varoffset in
        appstr buf "pop rax" ;
        appfmt buf "mov [rbp + %d], rax" offset ;
        appstr buf "push rax" ;
        appstr buf @@ gen_assign_pattern env exp_label ptn ;
        Buffer.contents buf
    | PtnOr (lhs, rhs) ->
        let next_label = make_label () in
        let exit_label = make_label () in
        let saved_rsp_offset = env.offset - 8 in
        stack_size := max !stack_size (-saved_rsp_offset) ;
        let env = {env with offset= saved_rsp_offset} in
        let buf = Buffer.create 128 in
        appfmt buf "mov [rbp + %d], rsp" saved_rsp_offset ;
        appstr buf "pop rax" ;
        appstr buf "push rax" ;
        appstr buf "push rax" ;
        appstr buf @@ gen_assign_pattern env next_label lhs ;
        appstr buf "pop rax" ;
        appfmt buf "jmp %s" exit_label ;
        appfmt buf "%s:" next_label ;
        appfmt buf "mov rsp, [rbp + %d]" saved_rsp_offset ;
        appstr buf @@ gen_assign_pattern env exp_label rhs ;
        appfmt buf "%s:" exit_label ;
        Buffer.contents buf
    | CtorApp (Some typename, ctorname, None) ->
        gen_assign_pattern env exp_label
        @@ IntValue (Hashtbl.find ctors_id (typename, ctorname))
    | CtorApp (Some typename, ctorname, Some arg) ->
        let buf = Buffer.create 128 in
        let id = Hashtbl.find ctors_id (typename, ctorname) in
        appfmt buf "pop rax" ;
        appstr buf "mov rdi, rax" ;
        appstr buf "and rax, 1" ;
        appstr buf "cmp rax, 0" ;
        appfmt buf "jne %s" exp_label ;
        appstr buf "mov rax, [rdi - 8]" ;
        appstr buf "and rax, 0xff" ;
        appfmt buf "cmp rax, %d" id ;
        appfmt buf "jne %s" exp_label ;
        appstr buf "push [rdi]" ;
        appstr buf @@ gen_assign_pattern env exp_label arg ;
        Buffer.contents buf
    | _ -> failwith "unexpected ast"
  in
  let rec gen_assign_pattern_or_raise env ptn =
    let exp_label = make_label () in
    let exit_label = make_label () in
    let assign_code = gen_assign_pattern env exp_label ptn in
    let buf = Buffer.create 256 in
    appstr buf assign_code ;
    appfmt buf "jmp %s" exit_label ;
    appfmt buf "%s:" exp_label ;
    appstr buf "mov rdi, 1" ;
    appstr buf "call exit@PLT" ;
    (* TODO: raise *)
    appfmt buf "%s:" exit_label ;
    Buffer.contents buf
  in
  let rec aux env = function
    | IntValue num -> sprintf "push %d" (tagged_int num)
    | CharValue ch -> aux env @@ IntValue (Char.code ch)
    | UnitValue | EmptyList -> aux env (IntValue 0)
    | StringValue (id, _) -> sprintf "lea rax, [rip + %s]\npush rax" id
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
    | Positate ast -> ""
    | Negate ast ->
        let buf = Buffer.create 128 in
        appstr buf @@ aux env ast ;
        appstr buf "pop rax" ;
        appstr buf @@ untag_int "rax" ;
        appstr buf "neg rax" ;
        appstr buf @@ tag_int "rax" ;
        appstr buf "push rax" ;
        Buffer.contents buf
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
        let false_label = make_label () in
        let exit_label = make_label () in
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
    | Var varname -> (
      try
        let offset = HashMap.find varname env.varoffset in
        String.concat "\n" [sprintf "mov rax, [rbp + %d]" offset; "push rax"]
      with Not_found ->
        failwith (sprintf "not found in code generation: %s" varname) )
    | CtorApp (Some typename, ctorname, None) ->
        aux env @@ IntValue (Hashtbl.find ctors_id (typename, ctorname))
    | CtorApp (Some typename, ctorname, Some arg) ->
        let buf = Buffer.create 128 in
        appstr buf @@ gen_alloc_block 1 0
        @@ Hashtbl.find ctors_id (typename, ctorname) ;
        appstr buf "push rax" ;
        appstr buf @@ aux env arg ;
        appstr buf "pop rdi" ;
        appstr buf "pop rax" ;
        appfmt buf "mov [rax], rdi" ;
        appstr buf "push rax" ;
        Buffer.contents buf
    | AppDir (funcname, args) ->
        let buf = Buffer.create 128 in
        List.iter (fun arg -> appstr buf @@ aux env arg) (List.rev args) ;
        List.iteri
          (fun index reg ->
            if index < List.length args then appfmt buf "pop %s" reg )
          ["rax"; "rbx"; "rdi"; "rsi"; "rdx"; "rcx"; "r8"; "r9"; "r12"; "r13"] ;
        appfmt buf "call %s" funcname ;
        appstr buf "push rax" ;
        Buffer.contents buf
    | AppCls (func, args) ->
        (* call aqaml_appcls *)
        (* TODO: Any better way exists? *)
        (* TODO: only 9 or less arguments are allowed *)
        if List.length args > 9 then
          failwith "only 9 or less arguments are allowed (not implemented)" ;
        let buf = Buffer.create 128 in
        appstr buf @@ aux env func ;
        List.iter (fun arg -> appstr buf @@ aux env arg) (List.rev args) ;
        List.iteri
          (fun index reg ->
            if index < List.length args then appfmt buf "pop %s" reg )
          ["rbx"; "rdi"; "rsi"; "rdx"; "rcx"; "r8"; "r9"; "r12"; "r13"] ;
        appstr buf "pop rax" ;
        appfmt buf "call aqaml_appcls%d@PLT" @@ List.length args ;
        appstr buf "push rax" ;
        Buffer.contents buf
    | LetVar (false, bind, lhs, rhs) ->
        let varnames = varnames_in_pattern bind in
        let offset = env.offset - (List.length varnames * 8) in
        stack_size := max !stack_size (-offset) ;
        let env' =
          { offset
          ; varoffset=
              integrate env.varoffset @@ hashmap_of_list
              @@ List.mapi
                   (fun i n -> (n, env.offset - ((i + 1) * 8)))
                   varnames }
        in
        let buf = Buffer.create 256 in
        appstr buf @@ aux env lhs ;
        appstr buf @@ gen_assign_pattern_or_raise env' bind ;
        appstr buf @@ aux env' rhs ;
        Buffer.contents buf
    | LetFunc (_, funcname, _, _, body, _) ->
        let offset = env.offset - 8 in
        stack_size := max !stack_size (-offset) ;
        let env =
          {offset; varoffset= HashMap.add funcname offset env.varoffset}
        in
        aux env body
    | MakeCls (funcname, nargs, freevars) ->
        let buf = Buffer.create 128 in
        appstr buf @@ gen_alloc_block (List.length freevars + 2) 0 247 ;
        appfmt buf "lea rdi, [rip + %s]" funcname ;
        appstr buf "mov [rax], rdi" ;
        appfmt buf "mov QWORD PTR [rax + 8], %d" @@ nargs ;
        List.iteri
          (fun i var ->
            let offset = HashMap.find var env.varoffset in
            appfmt buf "mov rdi, [rbp + %d]" offset ;
            appfmt buf "mov [rax + %d], rdi" ((i + 2) * 8) )
          freevars ;
        appstr buf "push rax" ;
        Buffer.contents buf
    | MatchWith (cond, cases) ->
        let buf = Buffer.create 256 in
        appstr buf "/* MatchWith BEGIN */" ;
        appstr buf @@ aux env cond ;
        let saved_rsp_offset = env.offset - 8 in
        stack_size := max !stack_size (-saved_rsp_offset) ;
        let env = {env with offset= saved_rsp_offset} in
        appfmt buf "mov [rbp + %d], rsp" saved_rsp_offset ;
        let exit_label = make_label () in
        let exp_label =
          List.fold_left
            (fun this_label (ptn, whn, case) ->
              let varnames = varnames_in_pattern ptn in
              let offset = env.offset - (List.length varnames * 8) in
              stack_size := max !stack_size (-offset) ;
              let env =
                { offset
                ; varoffset=
                    (let rec aux i varoffset = function
                       | varname :: varnames ->
                           aux (i + 1)
                             (HashMap.add varname
                                (env.offset - (i * 8))
                                varoffset)
                             varnames
                       | [] -> varoffset
                     in
                     aux 1 env.varoffset varnames) }
              in
              let next_label = make_label () in
              appfmt buf "%s:" this_label ;
              appfmt buf "mov rsp, [rbp + %d]" saved_rsp_offset ;
              appstr buf "pop rax" ;
              appstr buf "push rax" ;
              appstr buf "push rax" ;
              appstr buf @@ gen_assign_pattern env next_label ptn ;
              ( match whn with
              | None -> ()
              | Some expr ->
                  appstr buf @@ aux env expr ;
                  appstr buf "pop rax" ;
                  appfmt buf "cmp rax, %d" @@ tagged_int 0 ;
                  appfmt buf "je %s" next_label ) ;
              appstr buf "pop rax" ;
              appstr buf @@ aux env case ;
              appfmt buf "jmp %s /* exit label */" exit_label ;
              next_label )
            (make_label ()) cases
        in
        appfmt buf "%s:" exp_label ;
        appstr buf "/* Match_failure */" ;
        appstr buf "mov rdi, 1" ;
        appstr buf "call exit@PLT" ;
        appfmt buf "%s:" exit_label ;
        appstr buf "/* MatchWith END */" ;
        Buffer.contents buf
    | TypeDef (_, typename, ctornames) ->
        List.iteri
          (fun i (ctorname, _) -> Hashtbl.add ctors_id (typename, ctorname) i)
          ctornames ;
        "push 0 /* dummy */"
    | _ -> failwith "unexpected ast"
  in
  let strings_code =
    let buf = Buffer.create 80 in
    appfmt buf ".data" ;
    List.iter
      (function
        | StringValue (id, str) ->
            let size = (String.length str / 8) + 1 in
            let space = 7 - (String.length str mod 8) in
            appfmt buf ".quad %d" ((size lsl 10) lor (0 lsl 8) lor 252) ;
            appfmt buf "%s:" id ;
            appfmt buf ".ascii \"%s\"" (escape_string str) ;
            if space <> 0 then appfmt buf ".space %d" space ;
            appfmt buf ".byte %d\n" space
        | _ -> failwith "unexpected ast")
      strings ;
    appfmt buf ".text\n" ;
    Buffer.contents buf
  in
  let letfuncs_code =
    String.concat "\n"
      (List.map
         (function
           | LetFunc (recursive, funcname, args, func, _, freevars) ->
               let varnames =
                 List.flatten @@ List.map varnames_in_pattern args
               in
               let env =
                 { offset= List.length varnames * -8
                 ; varoffset=
                     integrate HashMap.empty @@ hashmap_of_list
                     @@ List.mapi (fun i arg -> (arg, -8 * (i + 1))) varnames
                 }
               in
               (* if recursive then the function itself should be in env *)
               let env =
                 if recursive then
                   let offset = env.offset - 8 in
                   { offset
                   ; varoffset= HashMap.add funcname offset env.varoffset }
                 else env
               in
               (* if closured then freevars are stored on the stack *)
               let env =
                 { offset= env.offset - (8 * List.length freevars)
                 ; varoffset=
                     integrate env.varoffset @@ hashmap_of_list
                     @@ List.mapi
                          (fun i var -> (var, env.offset - (8 * (i + 1))))
                          freevars }
               in
               stack_size := -env.offset ;
               let code = aux env func in
               let buf = Buffer.create 256 in
               appfmt buf "/* %s(%d) */"
                 (if recursive then "recursive" else "")
                 (List.length args) ;
               appstr buf @@ funcname ^ ":" ;
               appstr buf @@ "push rbp" ;
               appstr buf @@ "mov rbp, rsp" ;
               appfmt buf "sub rsp, %d" !stack_size ;
               (* push arguments in order *)
               (* first the value for closure *)
               if List.length freevars <> 0 then
                 appfmt buf "push %s" @@ reg_of_index @@ List.length args ;
               (* then real arguments *)
               appstr buf @@ String.concat "\n" @@ List.rev
               @@ List.mapi
                    (fun i _ -> sprintf "push %s" (reg_of_index i))
                    args ;
               (* process real arguments *)
               appstr buf @@ String.concat "\n"
               @@ List.map
                    (fun ptn -> gen_assign_pattern_or_raise env ptn)
                    args ;
               (* process for closure *)
               if List.length freevars > 0 then (
                 appstr buf "pop rax" ;
                 List.iteri
                   (fun i var ->
                     appfmt buf "mov rdi, [rax + %d]" (i * 8) ;
                     appfmt buf "mov [rbp + %d], rdi"
                     @@ HashMap.find var env.varoffset )
                   freevars ) ;
               appstr buf code ;
               appstr buf "pop rax" ;
               appstr buf "mov rsp, rbp" ;
               appstr buf "pop rbp" ;
               appstr buf "ret\n\n" ;
               Buffer.contents buf
           | _ -> failwith "LetFunc should be here")
         letfuncs)
  in
  let main_code =
    let buf = Buffer.create 512 in
    appstr buf "aqaml_malloc:" ;
    appstr buf @@ untag_int "rax" ;
    appstr buf "mov edi, eax" ;
    appstr buf "call aqaml_malloc_detail@PLT" ;
    appstr buf "ret" ;
    appstr buf "" ;
    appstr buf "aqaml_structural_equal:" ;
    appstr buf "mov rdi, rax" ;
    appstr buf "mov rsi, rbx" ;
    appstr buf "call aqaml_structural_equal_detail@PLT" ;
    appstr buf @@ tag_int "rax" ;
    appstr buf "ret" ;
    appstr buf "" ;
    appstr buf "aqaml_structural_inequal:" ;
    appstr buf "mov rdi, rax" ;
    appstr buf "mov rsi, rbx" ;
    appstr buf "call aqaml_structural_equal_detail@PLT" ;
    (* eax == 0 *)
    appstr buf "test eax, eax" ;
    appstr buf "sete al" ;
    appstr buf @@ tag_int "rax" ;
    appstr buf "ret" ;
    appstr buf "" ;
    appstr buf "aqaml_string_length:" ;
    appstr buf "mov rdi, rax" ;
    appstr buf "call aqaml_string_length_detail@PLT" ;
    appstr buf @@ tag_int "rax" ;
    appstr buf "ret" ;
    appstr buf "" ;
    appstr buf "aqaml_print_string:" ;
    appstr buf "mov rdi, rax" ;
    appstr buf "call aqaml_print_string_detail@PLT" ;
    (* return unit value *)
    appstr buf "mov rax, 1" ;
    appstr buf "ret" ;
    appstr buf "" ;
    appstr buf "aqaml_exit:" ;
    appstr buf "mov rdi, rax" ;
    appstr buf "shr rdi, 1" ;
    appstr buf "call exit@PLT" ;
    appstr buf "" ;
    for nargs = 1 to 9 do
      let label_loop = make_label () in
      let label_exit = make_label () in
      let label_ret = make_label () in
      appfmt buf "aqaml_appcls%d:" nargs ;
      appstr buf "push rbp" ;
      appstr buf "mov rbp, rsp" ;
      appstr buf "sub rsp, 16" ;
      for i = nargs downto 1 do
        appfmt buf "push %s" @@ reg_of_index i
      done ;
      appfmt buf "mov QWORD PTR [rbp - 8], %d" nargs ;
      appfmt buf "%s:" label_loop ;
      appstr buf "mov r10, rax" ;
      appstr buf "mov r11, [rax + 8]" ;
      appstr buf "sub [rbp - 8], r11" ;
      appstr buf "lea rax, [r10 + 16]" ;
      for i = 0 to nargs - 1 do
        appstr buf "cmp r11, 0" ;
        appfmt buf "je %s" label_exit ;
        appstr buf "dec r11" ;
        appfmt buf "pop %s" @@ reg_of_index i ;
        appfmt buf "lea %s, [r10 + 16]" @@ reg_of_index (i + 1)
      done ;
      appfmt buf "%s:" label_exit ;
      appstr buf "call [r10]" ;
      appstr buf "cmp QWORD PTR [rbp - 8], 0" ;
      appfmt buf "je %s" label_ret ;
      appfmt buf "jmp %s" label_loop ;
      appfmt buf "%s:" label_ret ;
      appstr buf "mov rsp, rbp" ;
      appstr buf "pop rbp" ;
      appstr buf "ret" ;
      appstr buf ""
    done ;
    appstr buf "main:" ;
    (* give unit value as an argument *)
    appfmt buf "mov rax, %d" @@ tagged_int 0 ;
    appstr buf "call aqaml_main" ;
    appstr buf "mov rax, 0" ;
    appstr buf "ret\n" ;
    Buffer.contents buf
  in
  main_code ^ letfuncs_code ^ strings_code

;;
let program = read_lines () in
let tokens = tokenize program in
(* eprint_token_list tokens ; *)
let asts = parse tokens in
let code = generate (analyze asts) in
print_string
  (String.concat "\n" [".intel_syntax noprefix"; ".global main"; code])
