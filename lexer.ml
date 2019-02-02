open Printf
open Helper

let is_capital = function 'A' .. 'Z' -> true | _ -> false

let is_digit = function '0' .. '9' -> true | _ -> false

let is_ident_char = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '\'' | '_' -> true
  | _ -> false

let digit x =
  match x with
  | '0' .. '9' -> int_of_char x - int_of_char '0'
  | _ -> failwith "unexpected char: not digit"

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
  | Pipe
  | Fun
  | Function
  | As
  | When
  | Type
  | Dot
  | DotDot
  | Of
  | Int
  | Char
  | Unit
  | Bool
  | String
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
  | DotLParen
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
  | Open
  | PipePipe
  | AndAnd
  | Ampersand
  | Lor
  | Land
  | For
  | To
  | Downto
  | Do
  | Done
  | PipeGT
  | LBracketBar
  | BarRBracket

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
  | Pipe -> "|"
  | Fun -> "fun"
  | Function -> "function"
  | As -> "as"
  | When -> "when"
  | Type -> "type"
  | Dot -> "."
  | DotDot -> ".."
  | Of -> "of"
  | Int -> "int"
  | Char -> "char"
  | Unit -> "unit"
  | Bool -> "bool"
  | String -> "string"
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
  | DotLParen -> ".("
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
  | Open -> "open"
  | PipePipe -> "||"
  | AndAnd -> "&&"
  | Ampersand -> "&"
  | Lor -> "lor"
  | Land -> "land"
  | For -> "for"
  | To -> "to"
  | Downto -> "downto"
  | Do -> "do"
  | Done -> "done"
  | PipeGT -> "|>"
  | LBracketBar -> "[|"
  | BarRBracket -> "|]"

let raise_unexpected_token = function
  | x :: _ ->
      raise @@ failwith @@ sprintf "Unexpected token: %s" @@ string_of_token x
  | [] -> failwith "Unexpected EOF"

exception EOF

let tokenize program =
  let rec aux acc i =
    let next_char i =
      if i < String.length program then (i + 1, program.[i]) else raise EOF
    in
    let maybe_next_char i =
      try
        let i, ch = next_char i in
        (i, Some ch)
      with EOF -> (i + 1, None)
    in
    let rec next_int i acc =
      match maybe_next_char i with
      | _, None -> (i, acc)
      | i, Some ch when is_digit ch -> next_int i ((acc * 10) + digit ch)
      | i, Some _ -> (i - 1, acc)
    in
    let next_ident i =
      let buf = Buffer.create 5 in
      let rec aux i =
        match maybe_next_char i with
        | _, None -> (i, Buffer.contents buf)
        | i, Some ch when is_ident_char ch -> Buffer.add_char buf ch ; aux i
        | i, Some _ -> (i - 1, Buffer.contents buf)
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
            | 'r' -> '\r'
            | 't' -> '\t'
            | '\\' -> '\\'
            | '"' -> '"'
            | '\'' -> '\''
            | _ -> failwith @@ sprintf "unexpected char in char literal: %c" ch
          ) )
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
            | '\n' ->
                (* string chained with backslash *)
                let rec skip_space_and_tab i =
                  let i, ch = next_char i in
                  match ch with
                  | ' ' | '\t' -> skip_space_and_tab i
                  | _ -> i - 1
                in
                aux @@ skip_space_and_tab i
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
            | ')' -> if depth = 1 then i else aux i (depth - 1)
            | _ -> aux (i - 1) depth )
        | _ -> aux i depth
      in
      aux i 1
    in
    let switch_char i default tbl =
      match maybe_next_char i with
      | _, None -> aux (default :: acc) i
      | i, Some ch ->
          let i, token =
            try
              let _, token = List.find (fun (x, _) -> x = ch) tbl in
              (i, token)
            with Not_found -> (i - 1, default)
          in
          aux (token :: acc) i
    in
    match maybe_next_char i with
    | _, None -> List.rev acc
    | i, Some ch -> (
      match ch with
      | ' ' | '\t' | '\n' | '\r' -> aux acc i
      | '0' .. '9' ->
          let i, num = next_int (i - 1) 0 in
          aux (IntLiteral num :: acc) i
      | '\'' -> (
          let _, ch0 = next_char i in
          let _, ch1 = next_char (i + 1) in
          match (ch0, ch1) with
          | _, '\'' | '\\', _ ->
              let i, ch = next_char_literal i in
              aux (CharLiteral ch :: acc) i
          | _ -> aux (Apostrophe :: acc) i )
      | '"' ->
          let i, str = next_string_literal i in
          aux (StringLiteral (make_id "string", str) :: acc) i
      | 'a' .. 'z' | 'A' .. 'Z' | '_' -> (
          let i, str = next_ident (i - 1) in
          match str with
          | "let" -> aux (Let :: acc) i
          | "in" -> aux (In :: acc) i
          | "rec" -> aux (Rec :: acc) i
          | "true" -> aux (IntLiteral 1 :: acc) i (* TODO: boolean type *)
          | "false" -> aux (IntLiteral 0 :: acc) i
          | "if" -> aux (If :: acc) i
          | "then" -> aux (Then :: acc) i
          | "else" -> aux (Else :: acc) i
          | "match" -> aux (Match :: acc) i
          | "with" -> aux (With :: acc) i
          | "fun" -> aux (Fun :: acc) i
          | "function" -> aux (Function :: acc) i
          | "as" -> aux (As :: acc) i
          | "when" -> aux (When :: acc) i
          | "type" -> aux (Type :: acc) i
          | "of" -> aux (Of :: acc) i
          | "int" -> aux (Int :: acc) i
          | "char" -> aux (Char :: acc) i
          | "unit" -> aux (Unit :: acc) i
          | "bool" -> aux (Bool :: acc) i
          | "string" -> aux (String :: acc) i
          | "and" -> aux (And :: acc) i
          | "try" -> aux (Try :: acc) i
          | "exception" -> aux (Exception :: acc) i
          | "mod" -> aux (Mod :: acc) i
          | "lsl" -> aux (Lsl :: acc) i
          | "lsr" -> aux (Lsr :: acc) i
          | "asr" -> aux (Asr :: acc) i
          | "module" -> aux (Module :: acc) i
          | "struct" -> aux (Struct :: acc) i
          | "end" -> aux (End :: acc) i
          | "external" -> aux (External :: acc) i
          | "mutable" -> aux (Mutable :: acc) i
          | "open" -> aux (Open :: acc) i
          | "land" -> aux (Land :: acc) i
          | "lor" -> aux (Lor :: acc) i
          | "for" -> aux (For :: acc) i
          | "to" -> aux (To :: acc) i
          | "downto" -> aux (Downto :: acc) i
          | "do" -> aux (Do :: acc) i
          | "done" -> aux (Done :: acc) i
          | _ when is_capital str.[0] ->
              let rec aux' i cap acc =
                let is_dot_connected =
                  match maybe_next_char i with
                  | _, Some '.' -> (
                    match maybe_next_char (i + 1) with
                    | _, Some ('a' .. 'z' | 'A' .. 'Z' | '_') -> true
                    | _ -> false )
                  | _ -> false
                in
                if is_dot_connected then
                  let i, str = next_ident (i + 1) in
                  aux' i (is_capital str.[0]) (str :: acc)
                else
                  let str = String.concat "." @@ List.rev acc in
                  ( i
                  , match (cap, List.length acc > 1) with
                    | false, false -> LowerIdent str
                    | false, true -> LowerIdentWithModule str
                    | true, false -> CapitalIdent str
                    | true, true -> CapitalIdentWithModule str )
              in
              let i, tk = aux' i true [str] in
              aux (tk :: acc) i
          | _ -> aux (LowerIdent str :: acc) i )
      | '+' -> aux (Plus :: acc) i
      | '*' -> aux (Star :: acc) i
      | '/' -> aux (Slash :: acc) i
      | ')' -> aux (RParen :: acc) i
      | '>' -> aux (GT :: acc) i
      | '=' -> aux (Equal :: acc) i
      | ',' -> aux (Comma :: acc) i
      | ']' -> aux (RBracket :: acc) i
      | '^' -> aux (Hat :: acc) i
      | '!' -> aux (Exclam :: acc) i
      | '{' -> aux (LBrace :: acc) i
      | '}' -> aux (RBrace :: acc) i
      | '|' ->
          switch_char i Pipe
            [('|', PipePipe); ('>', PipeGT); (']', BarRBracket)]
      | '&' -> switch_char i Ampersand [('&', AndAnd)]
      | '@' -> switch_char i Naruto [('@', NarutoNaruto)]
      | '.' ->
          switch_char i Dot
            [('.', DotDot); ('[', DotLBracket); ('(', DotLParen)]
      | '-' -> switch_char i Minus [('>', Arrow)]
      | '<' -> switch_char i LT [('>', LTGT); ('-', LArrow)]
      | '[' -> switch_char i LBracket [(']', LRBracket); ('|', LBracketBar)]
      | ':' -> switch_char i Colon [(':', ColonColon); ('=', ColonEqual)]
      | ';' -> switch_char i Semicolon [(';', SemicolonSemicolon)]
      | '(' -> (
        match maybe_next_char i with
        | i, Some '*' ->
            let i = skip_comment i in
            aux acc i
        | i, Some ')' -> aux (LRParen :: acc) i
        | _, (Some _ | None) -> aux (LParen :: acc) i )
      | _ -> failwith (sprintf "unexpected char: '%c'" ch) )
  in
  aux [] 0
