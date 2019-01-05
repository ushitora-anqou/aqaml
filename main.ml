open Printf
open Scanf
module HashMap = Map.Make (String)

let filter_after_map f lst =
  List.map (function Some x -> x | None -> failwith "invalid op")
  @@ List.filter (function Some x -> true | None -> false)
  @@ List.map f lst

let rec list_unique lst =
  let set = Hashtbl.create @@ List.length lst in
  let rec aux res = function
    | [] -> res
    | x :: xs ->
        if Hashtbl.mem set x then aux res xs
        else (
          Hashtbl.add set x () ;
          aux (x :: res) xs )
  in
  aux [] lst

let is_capital = function 'A' .. 'Z' -> true | _ -> false

let is_lower = function 'a' .. 'z' -> true | _ -> false

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
  | CapitalIdent of string
  | LowerIdent of string
  | CapitalIdentWithModule of string
  | LowerIdentWithModule of string
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
  | DotDot
  | Of
  | KwInt
  | KwChar
  | KwString
  | Apostrophe
  | And
  | Hat
  | Naruto
  | ColonEqual
  | Exclam
  | Try
  | Exception
  | Mod
  | Lsl
  | Lsr
  | Asr
  | DotLBracket
  | Colon
  | LBrace
  | RBrace
  | Module
  | Struct
  | End
  | NarutoNaruto
  | External
  | LArrow
  | Mutable

exception Unexpected_token

let string_of_token = function
  | IntLiteral num -> string_of_int num
  | CharLiteral ch -> "'" ^ String.make 1 ch ^ "'"
  | StringLiteral (_, str) -> "\"" ^ str ^ "\""
  | Plus -> "+"
  | Minus -> "-"
  | Star -> "*"
  | Slash -> "/"
  | CapitalIdent str
   |LowerIdent str
   |CapitalIdentWithModule str
   |LowerIdentWithModule str ->
      str
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
  | DotDot -> ".."
  | Of -> "of"
  | KwInt -> "int"
  | KwChar -> "char"
  | KwString -> "string"
  | Apostrophe -> "'"
  | And -> "and"
  | Hat -> "^"
  | Naruto -> "@"
  | ColonEqual -> ":="
  | Exclam -> "!"
  | Try -> "try"
  | Exception -> "exception"
  | Mod -> "mod"
  | Lsl -> "lsl"
  | Lsr -> "lsr"
  | Asr -> "asr"
  | DotLBracket -> ".["
  | Colon -> ":"
  | LBrace -> "{"
  | RBrace -> "}"
  | Module -> "module"
  | Struct -> "struct"
  | End -> "end"
  | NarutoNaruto -> "@@"
  | External -> "external"
  | LArrow -> "<-"
  | Mutable -> "mutable"

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
      | 'a' .. 'z' | 'A' .. 'Z' | '_' -> (
          let i, str = next_ident (i - 1) in
          match str with
          | "let" -> Let :: aux i
          | "in" -> In :: aux i
          | "rec" -> Rec :: aux i
          | "true" -> IntLiteral 1 :: aux i (* TODO: boolean type *)
          | "false" -> IntLiteral 0 :: aux i
          | "if" -> If :: aux i
          | "then" -> Then :: aux i
          | "else" -> Else :: aux i
          | "match" -> Match :: aux i
          | "with" -> With :: aux i
          | "fun" -> Fun :: aux i
          | "function" -> Function :: aux i
          | "as" -> As :: aux i
          | "when" -> When :: aux i
          | "type" -> Type :: aux i
          | "of" -> Of :: aux i
          | "int" -> KwInt :: aux i
          | "char" -> KwChar :: aux i
          | "string" -> KwString :: aux i
          | "and" -> And :: aux i
          | "try" -> Try :: aux i
          | "exception" -> Exception :: aux i
          | "mod" -> Mod :: aux i
          | "lsl" -> Lsl :: aux i
          | "lsr" -> Lsr :: aux i
          | "asr" -> Asr :: aux i
          | "module" -> Module :: aux i
          | "struct" -> Struct :: aux i
          | "end" -> End :: aux i
          | "external" -> External :: aux i
          | "mutable" -> Mutable :: aux i
          | _ when is_capital str.[0] ->
              let rec aux' i cap acc =
                let i, ch = next_char i in
                match ch with
                | '.' ->
                    let i, str = next_ident i in
                    aux' i (is_capital str.[0]) (str :: acc)
                | _ -> (
                    let str = String.concat "." @@ List.rev acc in
                    ( i - 1
                    , match (cap, List.length acc > 1) with
                      | false, false -> LowerIdent str
                      | false, true -> LowerIdentWithModule str
                      | true, false -> CapitalIdent str
                      | true, true -> CapitalIdentWithModule str ) )
              in
              let i, tk = aux' i true [str] in
              tk :: aux i
          | _ -> LowerIdent str :: aux i )
      | '+' -> Plus :: aux i
      | '*' -> Star :: aux i
      | '/' -> Slash :: aux i
      | ')' -> RParen :: aux i
      | '>' -> GT :: aux i
      | '=' -> Equal :: aux i
      | ',' -> Comma :: aux i
      | ']' -> RBracket :: aux i
      | '|' -> Bar :: aux i
      | '^' -> Hat :: aux i
      | '!' -> Exclam :: aux i
      | '{' -> LBrace :: aux i
      | '}' -> RBrace :: aux i
      | '@' -> (
          let i, ch = next_char i in
          match ch with
          | '@' -> NarutoNaruto :: aux i
          | _ -> Naruto :: aux (i - 1) )
      | '.' -> (
          let i, ch = next_char i in
          match ch with
          | '.' -> DotDot :: aux i
          | '[' -> DotLBracket :: aux i
          | _ -> Dot :: aux (i - 1) )
      | '-' -> (
          let i, ch = next_char i in
          match ch with '>' -> Arrow :: aux i | _ -> Minus :: aux (i - 1) )
      | '<' -> (
          let i, ch = next_char i in
          match ch with
          | '>' -> LTGT :: aux i
          | '-' -> LArrow :: aux i
          | _ -> LT :: aux (i - 1) )
      | '[' -> (
          let i, ch = next_char i in
          match ch with
          | ']' -> LRBracket :: aux i
          | _ -> LBracket :: aux (i - 1) )
      | ':' -> (
          let i, ch = next_char i in
          match ch with
          | ':' -> ColonColon :: aux i
          | '=' -> ColonEqual :: aux i
          | _ -> Colon :: aux (i - 1) )
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
  | TyCtorApp of typ * string
  | TyArgs of typ list
  | TyFunc of typ * typ

type ast =
  | UnitValue
  | IntValue of int
  | CharValue of char
  | StringValue of string * string
  | TupleValue of ast list
  | RecordValue of string option * (string * ast) list
  | RecordValueWith of ast * (string * ast) list
  | RecordDotAccess of string option * ast * string
  | Add of ast * ast
  | Sub of ast * ast
  | Mul of ast * ast
  | Div of ast * ast
  | Rem of ast * ast
  | LogicalLeftShift of ast * ast
  | LogicalRightShift of ast * ast
  | ArithmeticRightShift of ast * ast
  | StringConcat of ast * ast
  | ListConcat of ast * ast
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
  | LetAnd of (bool * (pattern list * ast) list * ast option)
  | LetVar of (bool * pattern * ast)
  (* recursive?, funcname, args, function body, free variables *)
  | LetFunc of (bool * string * pattern list * ast * string list)
  | LetAndAnalyzed of ast list * ast
  | Cons of (ast * ast)
  | EmptyList
  | ExprSeq of ast list
  | MatchWith of ast * (pattern * ast option * ast) list
  | MakeCls of string * int * string list
  | Lambda of pattern list * ast
  | TypeAnd of typedef list
  | CtorApp of string option * string * ast option
  | RefAssign of ast * ast
  | RecordAssign of string option * ast * string * ast
  | Deref of ast
  | ExpDef of string * typ option
  | TryWith of ast * (pattern * ast option * ast) list
  | StringGet of ast * ast
  | StringSet of ast * ast * ast
  | Nope
  | ModuleDef of string * ast list
  (* for analysis *)
  | ModuleDefEnd
  | ExternalDecl of string * typ * string
  (* TODO: module Ptn *)
  | PtnOr of pattern * pattern
  | PtnAlias of pattern * ast
  | PtnRange of char * char

and pattern = ast

and typedef =
  | DefVariant of typ option * string * (string * typ option) list
  | DefTypeAlias of typ option * string * typ
  | DefRecord of string * (string * typ) list

exception Unexpected_ast

let rec varnames_in_pattern = function
  (* TODO: much faster algorithm? *)
  | UnitValue | IntValue _ | CharValue _ | StringValue _ | EmptyList
   |PtnRange _ ->
      []
  | Var varname -> [varname]
  | Cons (car, cdr) ->
      List.rev_append (varnames_in_pattern car) (varnames_in_pattern cdr)
  | TupleValue values ->
      List.fold_left
        (fun a b -> List.rev_append a (varnames_in_pattern b))
        [] values
  | CtorApp (_, _, None) -> []
  | CtorApp (_, _, Some arg) -> varnames_in_pattern arg
  | PtnOr (lhs, rhs) ->
      List.rev_append (varnames_in_pattern lhs) (varnames_in_pattern rhs)
  | PtnAlias (ptn, Var name) -> name :: varnames_in_pattern ptn
  | _ -> raise Unexpected_ast

let parse tokens =
  let is_primary = function
    | ( IntLiteral _ | CharLiteral _ | StringLiteral _ | LowerIdent _
      | LowerIdentWithModule _ | CapitalIdent _ | CapitalIdentWithModule _
      | LRBracket | NarutoNaruto | LParen | LBracket | LRParen | LBrace )
      :: _ ->
        true
    | _ -> false
  in
  let is_dot = function (Dot | DotLBracket) :: _ -> true | _ -> false in
  let is_prefix tokens =
    is_primary tokens || is_dot tokens
    || match tokens with Exclam :: _ -> true | _ -> false
  in
  let rec parse_primary = function
    | IntLiteral num :: tokens -> (tokens, IntValue num)
    | CharLiteral ch :: tokens -> (tokens, CharValue ch)
    | StringLiteral (id, str) :: tokens -> (tokens, StringValue (id, str))
    | LRParen :: tokens -> (tokens, UnitValue)
    | (LowerIdentWithModule varname | LowerIdent varname) :: tokens ->
        (tokens, Var varname)
    | (CapitalIdentWithModule ctorname | CapitalIdent ctorname) :: tokens ->
        (tokens, CtorApp (None, ctorname, None))
    | LRBracket :: tokens -> (tokens, EmptyList)
    | NarutoNaruto :: tokens -> parse_expression tokens
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
    | LBrace
      :: (LowerIdent fieldname | LowerIdentWithModule fieldname)
         :: Equal :: tokens ->
        let rec aux fields = function
          | Semicolon
            :: (LowerIdent fieldname | LowerIdentWithModule fieldname)
               :: Equal :: tokens ->
              let tokens, ast = parse_let tokens in
              aux ((fieldname, ast) :: fields) tokens
          | RBrace :: tokens -> (tokens, fields)
          | _ -> raise Unexpected_token
        in
        let tokens, ast = parse_let tokens in
        let tokens, fields = aux [(fieldname, ast)] tokens in
        (tokens, RecordValue (None, fields))
    | LBrace :: tokens -> (
        let tokens, base = parse_primary tokens in
        match tokens with
        | With
          :: (LowerIdent fieldname | LowerIdentWithModule fieldname)
             :: Equal :: tokens ->
            let rec aux fields = function
              | Semicolon
                :: (LowerIdent fieldname | LowerIdentWithModule fieldname)
                   :: Equal :: tokens ->
                  let tokens, ast = parse_let tokens in
                  aux ((fieldname, ast) :: fields) tokens
              | RBrace :: tokens -> (tokens, fields)
              | _ -> raise Unexpected_token
            in
            let tokens, ast = parse_let tokens in
            let tokens, fields = aux [(fieldname, ast)] tokens in
            (tokens, RecordValueWith (base, fields))
        | _ -> raise Unexpected_token )
    | _ -> raise Unexpected_token
  and parse_dot tokens =
    let tokens, lhs = parse_primary tokens in
    match tokens with
    | Dot :: LowerIdent fieldname :: tokens ->
        (tokens, RecordDotAccess (None, lhs, fieldname))
    | DotLBracket :: tokens -> (
        let tokens, rhs = parse_expression tokens in
        match tokens with
        | RBracket :: tokens -> (tokens, StringGet (lhs, rhs))
        | _ -> raise Unexpected_token )
    | _ -> (tokens, lhs)
  and parse_prefix = function
    | Exclam :: tokens ->
        let tokens, ast = parse_dot tokens in
        (tokens, Deref ast)
    | tokens -> parse_dot tokens
  and parse_funccall tokens =
    let rec aux tokens =
      if is_prefix tokens then
        let tokens, arg = parse_prefix tokens in
        let tokens, args = aux tokens in
        (tokens, arg :: args)
      else (tokens, [])
    in
    let tokens, func = parse_prefix tokens in
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
  and parse_shift tokens =
    let rec aux lhs = function
      | Lsl :: tokens ->
          let tokens, rhs = parse_unary tokens in
          aux (LogicalLeftShift (lhs, rhs)) tokens
      | Lsr :: tokens ->
          let tokens, rhs = parse_unary tokens in
          aux (LogicalRightShift (lhs, rhs)) tokens
      | Asr :: tokens ->
          let tokens, rhs = parse_unary tokens in
          aux (ArithmeticRightShift (lhs, rhs)) tokens
      | tokens -> (tokens, lhs)
    in
    let tokens, lhs = parse_unary tokens in
    aux lhs tokens
  and parse_multiplicative tokens =
    let rec aux lhs tokens =
      match tokens with
      | Star :: tokens ->
          let tokens, rhs = parse_shift tokens in
          aux (Mul (lhs, rhs)) tokens
      | Slash :: tokens ->
          let tokens, rhs = parse_shift tokens in
          aux (Div (lhs, rhs)) tokens
      | Mod :: tokens ->
          let tokens, rhs = parse_shift tokens in
          aux (Rem (lhs, rhs)) tokens
      | _ -> (tokens, lhs)
    in
    let tokens, ast = parse_shift tokens in
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
  and parse_string_concat tokens =
    let rec aux lhs tokens =
      match tokens with
      | Hat :: tokens ->
          let tokens, rhs = parse_cons tokens in
          aux (StringConcat (lhs, rhs)) tokens
      | Naruto :: tokens ->
          let tokens, rhs = parse_cons tokens in
          aux (ListConcat (lhs, rhs)) tokens
      | _ -> (tokens, lhs)
    in
    let tokens, ast = parse_cons tokens in
    aux ast tokens
  and parse_structural_equal tokens =
    let rec aux lhs tokens =
      match tokens with
      | Equal :: tokens ->
          let tokens, rhs = parse_string_concat tokens in
          aux (StructEqual (lhs, rhs)) tokens
      | LTGT :: tokens ->
          let tokens, rhs = parse_string_concat tokens in
          aux (StructInequal (lhs, rhs)) tokens
      | LT :: Equal :: tokens ->
          let tokens, rhs = parse_string_concat tokens in
          aux (LessThanEqual (lhs, rhs)) tokens
      | GT :: Equal :: tokens ->
          let tokens, rhs = parse_string_concat tokens in
          aux (LessThanEqual (rhs, lhs)) tokens
      | LT :: tokens ->
          let tokens, rhs = parse_string_concat tokens in
          aux (LessThan (lhs, rhs)) tokens
      | GT :: tokens ->
          let tokens, rhs = parse_string_concat tokens in
          aux (LessThan (rhs, lhs)) tokens
      | _ -> (tokens, lhs)
    in
    let tokens, ast = parse_string_concat tokens in
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
  and parse_assignment tokens =
    let tokens, lhs = parse_tuple tokens in
    match tokens with
    | ColonEqual :: tokens ->
        let tokens, rhs = parse_let tokens in
        (tokens, RefAssign (lhs, rhs))
    | LArrow :: tokens -> (
        let tokens, rhs = parse_let tokens in
        match lhs with
        | StringGet (str, idx) -> (tokens, StringSet (str, idx, rhs))
        | RecordDotAccess (None, lhs, fieldname) ->
            (tokens, RecordAssign (None, lhs, fieldname, rhs))
        | _ -> raise Unexpected_token )
    | _ -> (tokens, lhs)
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
    | tokens -> parse_assignment tokens
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
    | Try :: tokens -> (
        let tokens, cond = parse_expression tokens in
        match tokens with
        | With :: tokens ->
            let tokens, cases = parse_pattern_match tokens in
            (tokens, TryWith (cond, cases))
        | _ -> raise Unexpected_token )
    | Let :: tokens -> (
        let parse_let_binding tokens =
          let tokens, bind = parse_pattern tokens in
          match tokens with
          | Equal :: tokens ->
              (* define constants *)
              let tokens, lhs = parse_expression tokens in
              (tokens, ([bind], lhs))
          | _ ->
              (* define function *)
              let rec aux = function
                | Equal :: tokens -> (tokens, [])
                | tokens ->
                    let tokens, arg = parse_pattern tokens in
                    let tokens, args = aux tokens in
                    (tokens, arg :: args)
              in
              let tokens, args = aux tokens in
              let tokens, func = parse_expression tokens in
              (tokens, (bind :: args, func))
        in
        let tokens, recursive =
          match tokens with
          | Rec :: tokens -> (tokens, true)
          | _ -> (tokens, false)
        in
        let rec aux' lets = function
          | And :: tokens ->
              let tokens, le = parse_let_binding tokens in
              aux' (le :: lets) tokens
          | tokens -> (tokens, lets)
        in
        let tokens, le = parse_let_binding tokens in
        let tokens, lets = aux' [le] tokens in
        match tokens with
        | In :: tokens ->
            let tokens, rhs_of_in = parse_expression tokens in
            (tokens, LetAnd (recursive, lets, Some rhs_of_in))
        | _ -> (tokens, LetAnd (recursive, lets, None)) )
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
    | (LowerIdent id | LowerIdentWithModule id) :: tokens -> (tokens, Var id)
    | (CapitalIdent id | CapitalIdentWithModule id) :: tokens ->
        (tokens, CtorApp (None, id, None))
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
  and parse_pattern_range = function
    | CharLiteral st :: DotDot :: CharLiteral ed :: tokens ->
        (tokens, PtnRange (st, ed))
    | tokens -> parse_pattern_primary tokens
  and parse_pattern_ctor_app tokens =
    let tokens, ctorapp = parse_pattern_range tokens in
    match ctorapp with
    | CtorApp (None, ctorname, None) when is_primary tokens ->
        let tokens, arg = parse_pattern_range tokens in
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
    | As :: LowerIdent name :: tokens -> (tokens, PtnAlias (ptn, Var name))
    | _ -> (tokens, ptn)
  and parse_pattern tokens = parse_pattern_as tokens
  and parse_typexpr_primary = function
    | KwInt :: tokens -> (tokens, TyInt)
    | KwChar :: tokens -> (tokens, TyChar)
    | KwString :: tokens -> (tokens, TyString)
    | Apostrophe :: LowerIdent id :: tokens -> (tokens, TyVar id)
    | (LowerIdent typename | LowerIdentWithModule typename) :: tokens ->
        (tokens, TyCustom typename)
    | LParen :: _ ->
        failwith "Any token LParen should be handled in parse_typexpr_ctor_app"
    | _ -> raise Unexpected_token
  and parse_typexpr_ctor_app tokens =
    let tokens, lhs =
      match tokens with
      | LParen :: tokens ->
          let tokens, typexpr = parse_typexpr tokens in
          let rec aux types = function
            | Comma :: tokens ->
                let tokens, typexpr = parse_typexpr tokens in
                aux (typexpr :: types) tokens
            | RParen :: tokens -> (tokens, types)
            | _ -> raise Unexpected_token
          in
          let tokens, types = aux [typexpr] tokens in
          let types = List.rev types in
          ( tokens
          , if List.length types = 1 then List.hd types else TyArgs types )
      | _ -> parse_typexpr_primary tokens
    in
    let rec aux lhs = function
      | (LowerIdent typectorname | LowerIdentWithModule typectorname) :: tokens
        ->
          aux (TyCtorApp (lhs, typectorname)) tokens
      | tokens -> (tokens, lhs)
    in
    aux lhs tokens
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
  and parse_typexpr_func tokens =
    let tokens, lhs = parse_typexpr_tuple tokens in
    match tokens with
    | Arrow :: tokens ->
        let tokens, rhs = parse_typexpr_func tokens in
        (tokens, TyFunc (lhs, rhs))
    | _ -> (tokens, lhs)
  and parse_typexpr tokens = parse_typexpr_func tokens
  and parse_type_def tokens =
    let parse_type_param = function
      | Apostrophe :: LowerIdent id :: tokens -> (tokens, TyVar id)
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
    let parse_type_def_entry tokens =
      let tokens, type_param =
        match parse_type_params tokens with
        | None -> (tokens, None)
        | Some (tokens, type_params) -> (tokens, Some type_params)
      in
      match tokens with
      | LowerIdent typename :: Equal :: tokens -> (
          let parse_variant tokens =
            let rec aux first ctors = function
              | Bar :: CapitalIdent ctorname :: Of :: tokens ->
                  let tokens, typ = parse_typexpr tokens in
                  aux false ((ctorname, Some typ) :: ctors) tokens
              | CapitalIdent ctorname :: Of :: tokens when first ->
                  let tokens, typ = parse_typexpr tokens in
                  aux false ((ctorname, Some typ) :: ctors) tokens
              | Bar :: CapitalIdent ctorname :: tokens ->
                  aux false ((ctorname, None) :: ctors) tokens
              | CapitalIdent ctorname :: tokens when first ->
                  aux false ((ctorname, None) :: ctors) tokens
              | tokens -> (tokens, ctors)
            in
            let tokens, ctors = aux true [] tokens in
            (tokens, DefVariant (type_param, typename, ctors))
          in
          match tokens with
          | CapitalIdent str :: _ -> parse_variant tokens
          | Bar :: _ -> parse_variant tokens
          (* TODO: skip mutable *)
          | LBrace :: LowerIdent fieldname :: Colon :: tokens
           |LBrace :: Mutable :: LowerIdent fieldname :: Colon :: tokens ->
              let rec aux fields = function
                | Semicolon :: LowerIdent fieldname :: Colon :: tokens
                 |Semicolon
                  :: Mutable :: LowerIdent fieldname :: Colon :: tokens ->
                    let tokens, typexpr = parse_typexpr tokens in
                    aux ((fieldname, typexpr) :: fields) tokens
                | RBrace :: tokens -> (tokens, fields)
                | _ -> raise Unexpected_token
              in
              let tokens, typexpr = parse_typexpr tokens in
              let tokens, fields = aux [(fieldname, typexpr)] tokens in
              (tokens, DefRecord (typename, fields))
          | tokens ->
              let tokens, typ = parse_typexpr tokens in
              (tokens, DefTypeAlias (type_param, typename, typ)) )
      | _ -> raise Unexpected_token
    in
    let rec aux entries = function
      | And :: tokens ->
          let tokens, entry = parse_type_def_entry tokens in
          aux (entry :: entries) tokens
      | tokens -> (tokens, TypeAnd entries)
    in
    (* token Type is already fetched *)
    let tokens, entry = parse_type_def_entry tokens in
    aux [entry] tokens
  and parse_exp_def = function
    (* token Exception is already fetched *)
    | CapitalIdent expname :: Of :: tokens ->
        let tokens, typ = parse_typexpr tokens in
        (tokens, ExpDef (expname, Some typ))
    | CapitalIdent expname :: tokens -> (tokens, ExpDef (expname, None))
    | _ -> raise Unexpected_token
  in
  let parse_expressions_and_definitions tokens =
    (* Here are some tricks. All expressions split by double semicolons (;;)
     * are converted to (maybe large) one ExprSeq, and all 'let' without 'in'
     * come to have their following expressions as their 'in' part.
     * This change makes later processes such as semantic analysis easier. *)
    (* TODO: correct? *)
    (* TODO: not correct. definitions and expressions should be completely separated. *)
    let rec aux exprs = function
      | SemicolonSemicolon :: tokens -> aux exprs tokens
      | [] -> ([], List.rev exprs)
      | Type :: tokens ->
          let tokens, expr = parse_type_def tokens in
          aux (expr :: exprs) tokens
      | Exception :: tokens ->
          let tokens, expr = parse_exp_def tokens in
          aux (expr :: exprs) tokens
      | External :: LowerIdent id :: Colon :: tokens -> (
          let tokens, typexpr = parse_typexpr tokens in
          match tokens with
          | Equal :: StringLiteral (_, str) :: tokens ->
              let ast = ExternalDecl (id, typexpr, str) in
              aux (ast :: exprs) tokens
          | _ -> raise Unexpected_token )
      | Module :: CapitalIdent modulename :: Equal :: Struct :: tokens ->
          let tokens, asts = aux [] tokens in
          let ast = ModuleDef (modulename, asts) in
          aux (ast :: exprs) tokens
      | End :: tokens -> (* module end *)
                         (tokens, List.rev exprs)
      | tokens ->
          let tokens, expr = parse_expression tokens in
          aux (expr :: exprs) tokens
    in
    let _, exprs = aux [] tokens in
    exprs
  in
  parse_expressions_and_definitions tokens

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
  ; typedefs: typedef list ref
  ; exps_list: string list ref
  ; ctors_type: (string, string) Hashtbl.t
  ; exps: (string, string) Hashtbl.t
  ; records: (string, string) Hashtbl.t
  ; records_fields: (string, string list) Hashtbl.t
  ; modulename: string list ref }

(* Used in analysis of LetAnd *)
exception Should_be_closure

exception LetDef of ast list * environment

let analyze asts =
  let toplevel =
    { letfuncs= ref []
    ; strings= ref []
    ; typedefs= ref []
    ; exps_list= ref []
    ; ctors_type= Hashtbl.create 16
    ; exps= Hashtbl.create 16
    ; records= Hashtbl.create 16
    ; records_fields= Hashtbl.create 16
    ; modulename= ref [] }
  in
  let name_with_modulename name =
    String.concat "." @@ List.rev @@ (name :: !(toplevel.modulename))
  in
  let exprs2expr = function
    | [] -> Nope
    | [expr] -> expr
    | exprs -> ExprSeq exprs
  in
  let hashmap_find_with_modulename name hashmap =
    try HashMap.find name hashmap with Not_found ->
      HashMap.find (name_with_modulename name) hashmap
  in
  let find_symbol env name =
    let rec aux depth env =
      try (depth, hashmap_find_with_modulename name env.symbols)
      with Not_found -> (
        match env.parent with
        | Some parent -> aux (depth + 1) parent
        | None ->
            failwith (sprintf "not found in analysis (find_symbol): %s" name) )
    in
    aux 0 env
  in
  let rec aux_ptn env ptn =
    match ptn with
    | IntValue _ | CharValue _ | UnitValue | EmptyList | PtnRange _ -> ptn
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
          ( Some
              ( try Hashtbl.find toplevel.ctors_type ctorname
                with Not_found -> Hashtbl.find toplevel.exps ctorname )
          , ctorname
          , arg )
    | _ -> failwith "unexpected pattern"
  in
  let rec analyze_pattern_match_cases env cases =
    List.map
      (fun (ptn, whn, ast) ->
        let env' =
          {env with symbols= add_symbols_in_pattern env.symbols ptn}
        in
        ( aux_ptn env' ptn
        , (match whn with Some expr -> Some (aux env' expr) | None -> None)
        , aux env' ast ) )
      cases
  and aux env ast =
    match ast with
    | IntValue _ | CharValue _ | UnitValue | EmptyList -> ast
    | StringValue _ ->
        append_to_list_ref ast toplevel.strings ;
        ast
    | TupleValue values -> TupleValue (List.map (aux env) values)
    | RecordValue (None, fields) ->
        let key_fieldname, _ = List.hd fields in
        let typename = Hashtbl.find toplevel.records key_fieldname in
        RecordValue
          ( Some typename
          , List.map (fun (name, ast) -> (name, aux env ast)) fields )
    | RecordValueWith (base, fields) ->
        let key_fieldname, _ = List.hd fields in
        let typename = Hashtbl.find toplevel.records key_fieldname in
        let fieldnames = Hashtbl.find toplevel.records_fields typename in
        let fields = hashmap_of_list fields in
        let new_base = Var (make_id "var") in
        aux env
        @@ LetAnd
             ( false
             , [([new_base], base)]
             , Some
                 (RecordValue
                    ( None
                    , List.map
                        (fun fieldname ->
                          try (fieldname, HashMap.find fieldname fields)
                          with Not_found ->
                            ( fieldname
                            , RecordDotAccess (None, new_base, fieldname) ) )
                        fieldnames )) )
    | RecordDotAccess (None, ast, fieldname) ->
        let typename = Hashtbl.find toplevel.records fieldname in
        RecordDotAccess (Some typename, aux env ast, fieldname)
    | Cons (car, cdr) -> Cons (aux env car, aux env cdr)
    | Add (lhs, rhs) -> Add (aux env lhs, aux env rhs)
    | Sub (lhs, rhs) -> Sub (aux env lhs, aux env rhs)
    | Mul (lhs, rhs) -> Mul (aux env lhs, aux env rhs)
    | Div (lhs, rhs) -> Div (aux env lhs, aux env rhs)
    | Rem (lhs, rhs) -> Rem (aux env lhs, aux env rhs)
    | LogicalLeftShift (lhs, rhs) -> LogicalLeftShift (lhs, rhs)
    | LogicalRightShift (lhs, rhs) -> LogicalRightShift (lhs, rhs)
    | ArithmeticRightShift (lhs, rhs) -> ArithmeticRightShift (lhs, rhs)
    | StringConcat (lhs, rhs) -> StringConcat (aux env lhs, aux env rhs)
    | ListConcat (lhs, rhs) -> ListConcat (aux env lhs, aux env rhs)
    | RefAssign (lhs, rhs) -> RefAssign (aux env lhs, aux env rhs)
    | RecordAssign (None, lhs, fieldname, rhs) ->
        let typename = Hashtbl.find toplevel.records fieldname in
        RecordAssign (Some typename, aux env lhs, fieldname, aux env rhs)
    | Deref ast -> Deref (aux env ast)
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
    | Lambda (args, body) ->
        let funcname = ".lambda" in
        aux env
        @@ LetAnd (false, [(Var funcname :: args, body)], Some (Var funcname))
    | StringGet (str, idx) ->
        (* a.[b] returns a b-th character of a string a.
         * Therefore, convert it to String.get call *)
        aux env @@ AppCls (Var "String.get", [str; idx])
    | StringSet (str, idx, ast) ->
        aux env @@ AppCls (Var "String.set", [str; idx; ast])
    | TryWith (cond, cases) ->
        TryWith (aux env cond, analyze_pattern_match_cases env cases)
    | MatchWith (cond, cases) ->
        MatchWith (aux env cond, analyze_pattern_match_cases env cases)
    | Var name -> (
      match find_symbol env name with
      | 0, (Var _ as sym) -> sym
      | _, FuncVar (gen_funcname, 0) -> AppDir (gen_funcname, [])
      | 0, FuncVar (funcname, nargs) ->
          (* When FuncVar is processed here, AppDir will not be applied to this FuncVar.
           * Therefore the returned value should be closured in case
           * AppCls is applied to this value. *)
          MakeCls (funcname, nargs, [])
      | _, (Var id as sym) ->
          env.freevars := (name, id) :: !(env.freevars) ;
          sym
      | _ -> failwith @@ sprintf "not found variable in analysis: %s" name )
    | CtorApp (None, ctorname, None) ->
        CtorApp
          ( Some
              ( try Hashtbl.find toplevel.ctors_type ctorname
                with Not_found -> Hashtbl.find toplevel.exps ctorname )
          , ctorname
          , None )
    | TypeAnd entries ->
        toplevel.typedefs :=
          List.rev_append !(toplevel.typedefs)
          @@ List.map
               (function
                 | DefTypeAlias (type_param, typename, typ) ->
                     let typename = name_with_modulename typename in
                     DefTypeAlias (type_param, typename, typ)
                 | DefVariant (type_param, typename, ctornames) ->
                     let typename = name_with_modulename typename in
                     let ctornames =
                       List.map
                         (fun (ctorname, typexpr) ->
                           (name_with_modulename ctorname, typexpr) )
                         ctornames
                     in
                     List.iter
                       (fun (ctorname, _) ->
                         Hashtbl.add toplevel.ctors_type ctorname typename )
                       ctornames ;
                     DefVariant (type_param, typename, ctornames)
                 | DefRecord (typename, fields) ->
                     let typename = name_with_modulename typename in
                     let fields =
                       List.map
                         (fun (fieldname, typexpr) ->
                           (name_with_modulename fieldname, typexpr) )
                         fields
                     in
                     List.iter
                       (fun (fieldname, _) ->
                         Hashtbl.add toplevel.records fieldname typename )
                       fields ;
                     Hashtbl.add toplevel.records_fields typename
                     @@ List.map (fun (fieldname, _) -> fieldname) fields ;
                     DefRecord (typename, fields))
               entries ;
        Nope
    | ExpDef (expname, components) ->
        Hashtbl.add toplevel.exps expname expname ;
        toplevel.exps_list := expname :: !(toplevel.exps_list) ;
        Nope
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
    | LetAnd (recursive, lhs_of_in, rhs_of_in) ->
        (* Split rhs_of_eq into LetVar and LetFunc. At the same time,
         * make a conversion table for function names *)
        let funcnames2gen = Hashtbl.create 2 in
        let src =
          List.map
            (function
              | [Var funcname], rhs_of_eq when recursive ->
                  (* When recursive, LetVar should be LetFunc with no arguments. *)
                  (* TODO:
                    If the lhs doesn't have any freevars, then there is no need to convert it.
                    Also, we should check whether the lhs uses itself in a correct way e.g.
                        let rec length = function x :: xs -> 1 + length xs | [] -> 0;;
                    is okay, but
                        let rec id x = id;;
                    is ng. For now, we assume that 'let rec ...' expression is written properly.
                  *)
                  let funcname =
                    match rhs_of_in with
                    | Some _ -> funcname
                    | None -> name_with_modulename funcname
                  in
                  Hashtbl.add funcnames2gen funcname (make_id funcname) ;
                  LetFunc (true, funcname, [], rhs_of_eq, [])
              | [bind], rhs_of_eq -> LetVar (recursive, bind, rhs_of_eq)
              | Var funcname :: args, rhs_of_eq ->
                  let funcname =
                    match rhs_of_in with
                    | Some _ -> funcname
                    | None -> name_with_modulename funcname
                  in
                  Hashtbl.add funcnames2gen funcname (make_id funcname) ;
                  LetFunc (recursive, funcname, args, rhs_of_eq, [])
              | _ -> failwith "unexpected ast")
            lhs_of_in
        in
        (* Now, analyze all LetVar/LetFunc.
         * When we analyze *recursive* LetFunc, we must decide whether
         * we should call this function by name or as closure in itself.
         * Therefore, first, we assume that we can call them by name i.e. we use FuncVar.
         * Next, if we find we can't do so (i.e. there are any freevars), we decide to call them as closure,
         * that is, use Var, and analyze it again.
         * I (ushitora-anqou) 'pakutta' or borrowed this idea from MinCaml.
         * TODO: is there better way?*)
        let let_closures_freevars = ref [] in
        let should_be_closure = ref false in
        let rec analyze_lets first =
          let toplevel_letfuncs_backup = !(toplevel.letfuncs) in
          let toplevel_strings_backup = !(toplevel.strings) in
          let funcvars =
            hashmap_of_list
            @@ filter_after_map
                 (function
                   | LetFunc (_, funcname, args, _, _) ->
                       let gen_funcname =
                         Hashtbl.find funcnames2gen funcname
                       in
                       Some
                         ( if first then
                           (funcname, FuncVar (gen_funcname, List.length args))
                         else (funcname, Var gen_funcname) )
                   | _ -> None)
                 src
          in
          let rec aux' env' = function
            | LetVar (false, bind, lhs) ->
                let env' =
                  {env' with symbols= add_symbols_in_pattern env'.symbols bind}
                in
                (env', LetVar (false, aux_ptn env' bind, aux env lhs))
            | LetFunc (recursive, funcname, args, func, _) ->
                let gen_funcname = Hashtbl.find funcnames2gen funcname in
                let env_in =
                  { symbols= add_symbols_in_patterns HashMap.empty args
                  ; parent= Some env
                  ; freevars= ref [] }
                in
                (* if recursive then funcname(s) should be in env *)
                let env_in =
                  if not recursive then env_in
                  else {env_in with symbols= integrate env_in.symbols funcvars}
                in
                let func = aux env_in func in
                (* Delete duplicate freevars *)
                env_in.freevars := list_unique !(env_in.freevars) ;
                let freevars =
                  ref (List.map (fun (_, a) -> a) !(env_in.freevars))
                in
                if first then (
                  (* Save data for the possible second loop *)
                  let_closures_freevars := !freevars @ !let_closures_freevars ;
                  (* If the function is recursive and should call itself as a closure,
                   * then Var should be used rather than FuncVar *)
                  if recursive && List.length !freevars <> 0 then
                    should_be_closure := true ;
                  if !should_be_closure then raise Should_be_closure ) ;
                let func =
                  if first then func
                  else (
                    (* In the target function, all functions chained with keyword 'and' should be available.
                     * This means that they should be defined as closures at the head of the target function.
                     * Note that these closures should have *all* freevars in chained functions. *)
                    (* TODO: only functions appeared in freevars need to be available. *)
                    freevars := !let_closures_freevars ;
                    LetAndAnalyzed
                      ( filter_after_map
                          (function
                            | LetFunc (_, funcname, args, _, _) ->
                                let gen_funcname =
                                  Hashtbl.find funcnames2gen funcname
                                in
                                Some
                                  (LetVar
                                     ( false
                                     , Var gen_funcname
                                     , MakeCls
                                         ( gen_funcname
                                         , List.length args
                                         , !let_closures_freevars ) ))
                            | _ -> None)
                          src
                      , func ) )
                in
                (* freevars are passed to env if they are not defined in env *)
                List.iter
                  (fun ((name, _) as var) ->
                    let d, _ = find_symbol env name in
                    if d <> 0 then env.freevars := var :: !(env.freevars) )
                  !(env_in.freevars) ;
                if List.length !freevars = 0 then (
                  (* no freevars; no need for closure *)
                  let env_out =
                    { env' with
                      symbols=
                        HashMap.add funcname
                          (FuncVar (gen_funcname, List.length args))
                          env'.symbols }
                  in
                  let ast =
                    LetFunc
                      ( recursive
                      , gen_funcname
                      , List.map (aux_ptn env_in) args
                      , func
                      , [] )
                  in
                  append_to_list_ref ast toplevel.letfuncs ;
                  (env_out, ast) )
                else
                  (* closure *)
                  let funcvar = Var gen_funcname in
                  let env_out =
                    { env' with
                      symbols= HashMap.add funcname funcvar env'.symbols }
                  in
                  let ast =
                    LetFunc
                      ( recursive
                      , gen_funcname
                      , List.map (aux_ptn env_in) args
                      , func
                      , !freevars )
                  in
                  append_to_list_ref ast toplevel.letfuncs ;
                  ( env_out
                  , LetVar
                      ( false
                      , funcvar
                      , MakeCls (gen_funcname, List.length args, !freevars) )
                  )
            | _ -> raise Unexpected_ast
          in
          let env', lets =
            List.fold_left
              (fun (env', lets) le ->
                try
                  match le with
                  | LetVar _ ->
                      let env', le_analyzed = aux' env' le in
                      (env', le_analyzed :: lets)
                  | LetFunc _ ->
                      let env', le_analyzed = aux' env' le in
                      (env', le_analyzed :: lets)
                  | _ -> failwith "unexpected ast"
                with Should_be_closure when first -> (env', lets) )
              (env, []) src
          in
          if first && !should_be_closure then (
            toplevel.letfuncs := toplevel_letfuncs_backup ;
            toplevel.strings := toplevel_strings_backup ;
            let_closures_freevars := list_unique !let_closures_freevars ;
            analyze_lets false )
          else
            match rhs_of_in with
            | None -> raise (LetDef (lets, env'))
            | Some rhs -> LetAndAnalyzed (lets, aux env' rhs)
        in
        analyze_lets true
    | _ -> raise Unexpected_ast
  and analyze_module env exprs =
    let toplevel_env = ref env in
    let rec aux' exprs = function
      | ModuleDef (this_modulename, body) :: asts ->
          toplevel.modulename := this_modulename :: !(toplevel.modulename) ;
          (* TODO: is there any better way? *)
          aux' exprs @@ body @ (ModuleDefEnd :: asts)
      | ModuleDefEnd :: asts ->
          toplevel.modulename := List.tl !(toplevel.modulename) ;
          aux' exprs asts
      | ExternalDecl (id, typexpr, decl) :: asts ->
          let id = name_with_modulename id in
          let nargs =
            let rec aux cnt = function
              | TyFunc (lhs, rhs) -> aux (cnt + 1) rhs
              | _ -> cnt
            in
            aux 0 typexpr
          in
          toplevel_env :=
            { !toplevel_env with
              symbols=
                HashMap.add id (FuncVar (decl, nargs)) !toplevel_env.symbols } ;
          aux' exprs asts
      | ast :: asts -> (
        try aux' (aux !toplevel_env ast :: exprs) asts
        with LetDef (lets, env) ->
          toplevel_env := env ;
          exprs2expr @@ List.rev
          @@ (LetAndAnalyzed (lets, aux' [] asts) :: exprs) )
      | [] -> exprs2expr @@ List.rev exprs
    in
    let ast = aux' [] exprs in
    (!toplevel_env, ast)
  in
  let env = {symbols= HashMap.empty; parent= None; freevars= ref []} in
  let _, ast = analyze_module env asts in
  let ast = LetFunc (false, "aqaml_main", [UnitValue], ast, []) in
  append_to_list_ref ast toplevel.letfuncs ;
  ( !(toplevel.letfuncs)
  , !(toplevel.strings)
  , !(toplevel.typedefs)
  , !(toplevel.exps_list) )

type gen_environment = {offset: int; varoffset: int HashMap.t}

let rec generate (letfuncs, strings, typedefs, exps) =
  let stack_size = ref 0 in
  let records_idx = Hashtbl.create 16 in
  let ctors_id = Hashtbl.create 16 in
  List.iter
    (function
      | DefTypeAlias _ -> ()
      | DefVariant (_, typename, ctornames) ->
          List.iteri
            (fun i (ctorname, _) -> Hashtbl.add ctors_id (typename, ctorname) i)
            ctornames
      | DefRecord (typename, fields) ->
          List.iteri
            (fun i (fieldname, _) ->
              Hashtbl.add records_idx (typename, fieldname) i )
            fields)
    typedefs ;
  let exps_id = Hashtbl.create 16 in
  List.iter
    (fun expname -> Hashtbl.add exps_id expname @@ Hashtbl.length exps_id)
    exps ;
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
    | CtorApp (Some typename, ctorname, None) ->
        gen_assign_pattern env exp_label
        @@ IntValue
             ( try Hashtbl.find ctors_id (typename, ctorname)
               with Not_found -> Hashtbl.find exps_id typename )
    | CtorApp (Some typename, ctorname, Some arg) ->
        let buf = Buffer.create 128 in
        let id =
          try Hashtbl.find ctors_id (typename, ctorname) with Not_found ->
            Hashtbl.find exps_id typename
        in
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
    | PtnRange (bg, ed) ->
        let buf = Buffer.create 128 in
        let exit0_label = make_label () in
        let exit1_label = make_label () in
        appstr buf "pop rax" ;
        appfmt buf "cmp rax, %d" @@ tagged_int @@ Char.code bg ;
        appfmt buf "jge %s" exit0_label ;
        appfmt buf "jmp %s" exp_label ;
        appfmt buf "%s:" exit0_label ;
        appfmt buf "cmp rax, %d" @@ tagged_int @@ Char.code ed ;
        appfmt buf "jle %s" exit1_label ;
        appfmt buf "jmp %s" exp_label ;
        appfmt buf "%s:" exit1_label ;
        Buffer.contents buf
    | _ -> raise Unexpected_ast
  in
  let rec gen_assign_pattern_or_raise env ptn =
    let exp_label = make_label () in
    let exit_label = make_label () in
    let assign_code = gen_assign_pattern env exp_label ptn in
    let buf = Buffer.create 256 in
    appstr buf assign_code ;
    appfmt buf "jmp %s" exit_label ;
    appfmt buf "%s:" exp_label ;
    (* TODO: arguments of Match_failure *)
    appstr buf "mov rax, 1" ;
    appstr buf @@ gen_raise_exp_of "Match_failure" true ;
    appfmt buf "%s:" exit_label ;
    Buffer.contents buf
  and gen_raise () =
    let buf = Buffer.create 128 in
    (* Raise. Thanks to:
     * https://github.com/ocamllabs/ocaml-multicore/wiki/Native-code-notes *)
    appstr buf "mov rsp, r14" ;
    appstr buf "pop r14" ;
    appstr buf "ret" ;
    Buffer.contents buf
  and gen_raise_exp_of expname has_arguments =
    let buf = Buffer.create 128 in
    appstr buf "/* raise */" ;
    if not has_arguments then
      appfmt buf "mov rax, %d" @@ tagged_int @@ Hashtbl.find exps_id expname
    else (
      (* Assume that the argument is stored in rax *)
      appstr buf "mov rbx, rax" ;
      appstr buf @@ gen_alloc_block 1 0 @@ Hashtbl.find exps_id expname ;
      appstr buf "mov [rax], rbx" ) ;
    appstr buf @@ gen_raise () ;
    Buffer.contents buf
  and gen_pattern_match_cases env cases exp_body =
    (* Assume that the target value is in stack top *)
    let buf = Buffer.create 128 in
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
                         (HashMap.add varname (env.offset - (i * 8)) varoffset)
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
    appstr buf "/* match failed */" ;
    appstr buf exp_body ;
    appfmt buf "%s:" exit_label ;
    Buffer.contents buf
  and aux env = function
    | Nope -> "push 0 /* dummy */"
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
    | RecordValue (Some typename, fields) ->
        let offset = env.offset - 8 in
        stack_size := max !stack_size (-offset) ;
        let buf = Buffer.create 128 in
        appfmt buf "/* RecordValue %s BEGIN */" typename ;
        appstr buf @@ gen_alloc_block (List.length fields) 0 1 ;
        appfmt buf "mov [rbp + %d], rax" offset ;
        List.iter
          (fun (fieldname, ast) ->
            appstr buf @@ aux env ast ;
            appstr buf "pop rax" ;
            appfmt buf "mov rdi, [rbp + %d]" offset ;
            let idx = Hashtbl.find records_idx (typename, fieldname) in
            appfmt buf "mov [rdi + %d], rax" (idx * 8) )
          fields ;
        appfmt buf "push [rbp + %d]" offset ;
        appfmt buf "/* RecordValue %s END */" typename ;
        Buffer.contents buf
    | RecordDotAccess (Some typename, ast, fieldname) ->
        let idx = Hashtbl.find records_idx (typename, fieldname) in
        let buf = Buffer.create 128 in
        appfmt buf "/* RecordDotAccess %s %s */" typename fieldname ;
        appstr buf @@ aux env ast ;
        appstr buf "pop rax" ;
        appfmt buf "push [rax + %d]" (idx * 8) ;
        Buffer.contents buf
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
        let buf = Buffer.create 128 in
        appstr buf @@ aux env lhs ;
        appstr buf @@ aux env rhs ;
        appstr buf "pop rdi" ;
        appstr buf @@ untag_int "rdi" ;
        appstr buf "pop rax" ;
        appstr buf @@ untag_int "rax" ;
        appstr buf "cqo" ;
        appstr buf "idiv rdi" ;
        appstr buf @@ tag_int "rax" ;
        appstr buf "push rax" ;
        Buffer.contents buf
    | Rem (lhs, rhs) ->
        let buf = Buffer.create 128 in
        appstr buf @@ aux env lhs ;
        appstr buf @@ aux env rhs ;
        appstr buf "pop rdi" ;
        appstr buf @@ untag_int "rdi" ;
        appstr buf "pop rax" ;
        appstr buf @@ untag_int "rax" ;
        appstr buf "cqo" ;
        appstr buf "idiv rdi" ;
        appstr buf @@ tag_int "rdx" ;
        appstr buf "push rdx" ;
        Buffer.contents buf
    | LogicalLeftShift (lhs, rhs) ->
        let buf = Buffer.create 128 in
        appstr buf @@ aux env lhs ;
        appstr buf @@ aux env rhs ;
        appstr buf "pop rcx" ;
        appstr buf @@ untag_int "rcx" ;
        appstr buf "pop rax" ;
        appstr buf @@ untag_int "rax" ;
        appstr buf "shl rax, cl" ;
        appstr buf @@ tag_int "rax" ;
        appstr buf "push rax" ;
        Buffer.contents buf
    | LogicalRightShift (lhs, rhs) ->
        (* Note that the size of int is 63bit, not 64bit. *)
        let buf = Buffer.create 128 in
        appstr buf @@ aux env lhs ;
        appstr buf @@ aux env rhs ;
        appstr buf "pop rcx" ;
        appstr buf @@ untag_int "rcx" ;
        appstr buf "pop rax" ;
        appstr buf "shr rax, cl" ;
        appstr buf "or rax, 1" ;
        appstr buf "push rax" ;
        Buffer.contents buf
    | ArithmeticRightShift (lhs, rhs) ->
        let buf = Buffer.create 128 in
        appstr buf @@ aux env lhs ;
        appstr buf @@ aux env rhs ;
        appstr buf "pop rcx" ;
        appstr buf @@ untag_int "rcx" ;
        appstr buf "pop rax" ;
        appstr buf @@ untag_int "rax" ;
        appstr buf "sar rax, cl" ;
        appstr buf @@ tag_int "rax" ;
        appstr buf "push rax" ;
        Buffer.contents buf
    | StringConcat (lhs, rhs) ->
        let buf = Buffer.create 128 in
        appstr buf @@ aux env lhs ;
        appstr buf @@ aux env rhs ;
        appstr buf "pop rbx" ;
        appstr buf "pop rax" ;
        appstr buf "call aqaml_concat_string" ;
        appstr buf "push rax" ;
        Buffer.contents buf
    | ListConcat (lhs, rhs) ->
        let buf = Buffer.create 128 in
        appstr buf @@ aux env lhs ;
        appstr buf @@ aux env rhs ;
        appstr buf "pop rbx" ;
        appstr buf "pop rax" ;
        appstr buf "call aqaml_concat_list" ;
        appstr buf "push rax" ;
        Buffer.contents buf
    | RefAssign (lhs, rhs) ->
        let buf = Buffer.create 128 in
        appstr buf @@ aux env lhs ;
        appstr buf @@ aux env rhs ;
        appstr buf "pop rbx" ;
        appstr buf "pop rax" ;
        appstr buf "mov [rax], rbx" ;
        (* push unit value *)
        appstr buf "push 1" ;
        Buffer.contents buf
    | RecordAssign (Some typename, lhs, fieldname, rhs) ->
        let idx = Hashtbl.find records_idx (typename, fieldname) in
        let buf = Buffer.create 128 in
        appstr buf @@ aux env lhs ;
        appstr buf @@ aux env rhs ;
        appstr buf "pop rbx" ;
        appstr buf "pop rax" ;
        appfmt buf "mov [rax + %d], rbx" (idx * 8) ;
        (* push unit value *)
        appstr buf "push 1" ;
        Buffer.contents buf
    | Deref ast ->
        let buf = Buffer.create 128 in
        appstr buf @@ aux env ast ;
        appstr buf "pop rax" ;
        appstr buf "push [rax]" ;
        Buffer.contents buf
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
        aux env
        @@ IntValue
             ( try Hashtbl.find ctors_id (typename, ctorname)
               with Not_found -> Hashtbl.find exps_id typename )
    | CtorApp (Some typename, ctorname, Some arg) ->
        let buf = Buffer.create 128 in
        appstr buf
        @@ gen_alloc_block 1 0
             ( try Hashtbl.find ctors_id (typename, ctorname)
               with Not_found -> Hashtbl.find exps_id typename ) ;
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
    | LetAndAnalyzed (lets, rhs_of_in) ->
        let buf = Buffer.create 256 in
        let aux' env' = function
          | LetVar (false, bind, lhs) ->
              let varnames = varnames_in_pattern bind in
              let offset = env'.offset - (List.length varnames * 8) in
              stack_size := max !stack_size (-offset) ;
              let env' =
                { offset
                ; varoffset=
                    integrate env'.varoffset @@ hashmap_of_list
                    @@ List.mapi
                         (fun i n -> (n, env'.offset - ((i + 1) * 8)))
                         varnames }
              in
              appstr buf @@ aux env lhs ;
              appstr buf @@ gen_assign_pattern_or_raise env' bind ;
              env'
          | LetFunc (_, funcname, _, _, _) ->
              let offset = env'.offset - 8 in
              stack_size := max !stack_size (-offset) ;
              let env' =
                {offset; varoffset= HashMap.add funcname offset env'.varoffset}
              in
              env'
          | _ -> raise Unexpected_ast
        in
        let env' = List.fold_left (fun env le -> aux' env le) env lets in
        appstr buf @@ aux env' rhs_of_in ;
        Buffer.contents buf
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
        appstr buf
        @@ gen_pattern_match_cases env cases
             (let buf = Buffer.create 128 in
              appstr buf "mov rax, 1" ;
              (* TODO: arguments for Match_failure *)
              appstr buf @@ gen_raise_exp_of "Match_failure" true ;
              Buffer.contents buf) ;
        appstr buf "/* MatchWith END */" ;
        Buffer.contents buf
    | TryWith (cond, cases) ->
        let offset = env.offset - 8 in
        stack_size := max !stack_size (-offset) ;
        let env = {env with offset} in
        let exp_label = make_label () in
        let exit_label = make_label () in
        let buf = Buffer.create 256 in
        appstr buf "/* TryWith BEGIN */" ;
        (* set an exception handler *)
        appfmt buf "lea r13, [rip + %s]" exp_label ;
        appstr buf "push rbp" ;
        appstr buf "push r13" ;
        appstr buf "push r14" ;
        appstr buf "mov r14, rsp" ;
        appstr buf @@ aux env cond ;
        appstr buf "pop rax" ;
        appstr buf "pop r14 /* pop for r14 */" ;
        appstr buf "pop rbx /* pop for r13 */" ;
        appstr buf "pop rbx /* pop for rbp */" ;
        appstr buf "push rax" ;
        appfmt buf "jmp %s" exit_label ;
        appfmt buf "%s:" exp_label ;
        appstr buf "pop rbp" ;
        appfmt buf "mov [rbp + %d], rax" offset ;
        appstr buf "push rax" ;
        appstr buf
        @@ gen_pattern_match_cases env cases
             (let buf = Buffer.create 128 in
              appfmt buf "mov rax, [rbp + %d]" offset ;
              appstr buf @@ gen_raise () ;
              Buffer.contents buf) ;
        appfmt buf "%s:" exit_label ;
        appstr buf "/* TryWith END */" ;
        Buffer.contents buf
    | _ -> raise Unexpected_ast
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
        | _ -> raise Unexpected_ast)
      strings ;
    appfmt buf ".text\n" ;
    Buffer.contents buf
  in
  let letfuncs_code =
    String.concat "\n"
      (List.map
         (function
           | LetFunc (recursive, funcname, args, func, freevars) ->
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
    appstr buf "aqaml_concat_string:" ;
    appstr buf "mov rdi, rax" ;
    appstr buf "mov rsi, rbx" ;
    appstr buf "call aqaml_concat_string_detail@PLT" ;
    appstr buf "ret" ;
    appstr buf "" ;
    appstr buf "aqaml_concat_list:" ;
    appstr buf "mov rdi, rax" ;
    appstr buf "mov rsi, rbx" ;
    appstr buf "call aqaml_concat_list_detail@PLT" ;
    appstr buf "ret" ;
    appstr buf "" ;
    appstr buf "aqaml_string_length:" ;
    appstr buf "mov rdi, rax" ;
    appstr buf "call aqaml_string_length_detail@PLT" ;
    appstr buf @@ tag_int "rax" ;
    appstr buf "ret" ;
    appstr buf "" ;
    appstr buf "aqaml_string_get:" ;
    appstr buf "mov rdi, rax" ;
    appstr buf @@ untag_int "rbx" ;
    appstr buf "mov rsi, rbx" ;
    appstr buf "call aqaml_string_get_detail@PLT" ;
    appstr buf @@ tag_int "rax" ;
    appstr buf "ret" ;
    appstr buf "" ;
    appstr buf "aqaml_string_set:" ;
    appstr buf @@ untag_int "rbx" ;
    appstr buf @@ untag_int "rdi" ;
    appstr buf "mov rdx, rdi" ;
    appstr buf "mov rsi, rbx" ;
    appstr buf "mov rdi, rax" ;
    appstr buf "call aqaml_string_set_detail@PLT" ;
    appfmt buf "mov rax, %d" @@ tagged_int 0 ;
    appstr buf "ret" ;
    appstr buf "" ;
    appstr buf "aqaml_string_create:" ;
    appstr buf @@ untag_int "rax" ;
    appstr buf "mov rdi, rax" ;
    appstr buf "call aqaml_string_create_detail@PLT" ;
    appstr buf "ret" ;
    appstr buf "" ;
    appstr buf "aqaml_string_blit:" ;
    appstr buf @@ untag_int "rbx" ;
    appstr buf @@ untag_int "rsi" ;
    appstr buf @@ untag_int "rdx" ;
    appstr buf "mov r8, rdx" ;
    appstr buf "mov rcx, rsi" ;
    appstr buf "mov rdx, rdi" ;
    appstr buf "mov rsi, rbx" ;
    appstr buf "mov rdi, rax" ;
    appstr buf "call aqaml_string_blit_detail@PLT" ;
    appfmt buf "mov rax, %d" @@ tagged_int 0 ;
    appstr buf "ret" ;
    appstr buf "" ;
    appstr buf "aqaml_char_code:" ;
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
    appstr buf "aqaml_ref:" ;
    appstr buf "mov rbx, rax" ;
    appstr buf @@ gen_alloc_block 1 0 0 ;
    appstr buf "mov [rax], rbx" ;
    appstr buf "ret" ;
    appstr buf "" ;
    appstr buf "aqaml_raise:" ;
    appstr buf @@ gen_raise () ;
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
    appstr buf "push rbp" ;
    appstr buf "mov rbp, rsp" ;
    (* default exception handler *)
    appstr buf "lea r13, [rip + aqaml_default_exception_handler]" ;
    appstr buf "push r13" ;
    appstr buf "push r14" ;
    appstr buf "mov r14, rsp" ;
    (* give unit value as an argument *)
    appfmt buf "mov rax, %d" @@ tagged_int 0 ;
    appstr buf "call aqaml_main" ;
    appstr buf "pop rax" ;
    appstr buf "pop rax" ;
    appstr buf "mov rax, 0" ;
    appstr buf "pop rbp" ;
    appstr buf "ret" ;
    appstr buf "aqaml_default_exception_handler:" ;
    appfmt buf "mov rax, %d" @@ tagged_int 1 ;
    appstr buf "call aqaml_exit" ;
    appstr buf "" ;
    Buffer.contents buf
  in
  main_code ^ letfuncs_code ^ strings_code

;;
let program = read_lines () in
let tokens = tokenize program in
(* eprint_token_list tokens ; *)
let asts = parse tokens in
let analyzed_data = analyze asts in
let code = generate analyzed_data in
print_string
  (String.concat "\n" [".intel_syntax noprefix"; ".global main"; code])
