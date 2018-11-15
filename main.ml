open Printf;;
open Scanf;;

let digit x = match x with
  | '0'..'9' -> (int_of_char x) - (int_of_char '0')
  | _ -> failwith "unexpected char: not digit";;

let program = read_line ();;

type token =
  | IntLiteral of int
  | Plus
  | Minus
  | Star
  | Slash
  | Ident of string
  | LParen
  | RParen
;;

exception EOF;;

let rec tokenize i =
  let next_char i =
    if i < String.length program then
      (i + 1, String.get program i)
    else raise EOF in
  let rec next_int i acc =
    try
      let (i, ch) = next_char i in
      match ch with
      | '0'..'9' -> next_int i (acc * 10 + digit ch)
      | _ -> (i - 1, acc)
    with
      EOF -> (i, acc) in
  let next_ident i =
    let buf = Buffer.create 5 in
    let rec aux i =
      try
        let (i, ch) = next_char i in
        match ch with
        | 'a'..'z' | 'A'..'Z' | '0'..'9' | '\'' -> Buffer.add_char buf ch; aux i
        | _ -> (i - 1, Buffer.contents buf)
      with
        EOF -> (i, Buffer.contents buf) in
    aux i in

  try
    let (i, ch) = next_char i in
    match ch with
    | ' ' | '\t' | '\n' -> tokenize i
    | '0'..'9' ->
      let (i, num) = next_int (i - 1) 0 in (IntLiteral num)::tokenize i
    | 'a'..'z' | 'A'..'Z' ->
      let (i, str) = next_ident (i - 1) in (Ident str)::tokenize i
    | '+' -> Plus::tokenize i
    | '-' -> Minus::tokenize i
    | '*' -> Star::tokenize i
    | '/' -> Slash::tokenize i
    | '(' -> LParen::tokenize i
    | ')' -> RParen::tokenize i
    | _ -> failwith (sprintf "unexpected char: '%c'" ch)
  with
    EOF -> [];;

type ast =
  | Int of int
  | Add of (ast * ast)
  | Sub of (ast * ast)
  | Mul of (ast * ast)
  | Div of (ast * ast)
  | Var of (string * int option)
  | FuncCall of (ast * ast list)
;;

module HashMap = Map.Make(String);;
type environment = {symbols: ast HashMap.t; funcdefs: (string * ast) list };;
let parse tokens =
  let rec
    parse_primary env = function
    | (IntLiteral num)::tokens -> (tokens, Int num)
    | (Ident id)::tokens -> (tokens, HashMap.find id env.symbols)
    | LParen::tokens ->
      let (tokens, ast) = parse_expression env tokens in
      begin match tokens with
        | RParen::tokens -> (tokens, ast)
        | _ -> failwith "unexpected token"
      end
    | _ -> failwith "unexpected token"
  and
    parse_funccall env tokens =
    let rec aux tokens = match tokens with
      | (IntLiteral _ | Ident _ | LParen)::_ ->    (* if primary *)
        let (tokens, arg) = parse_primary env tokens in
        let (tokens, args) = aux tokens in
        (tokens, arg::args)
      | _ -> (tokens, []) in

    let (tokens, func) = parse_primary env tokens in
    let (tokens, args) = aux tokens in
    if args = [] then (tokens, func)    (* not function call *)
    else (tokens, FuncCall (func, args))
  and
    parse_multiplicative env tokens =
    let rec aux lhs tokens = match tokens with
      | Star::tokens ->
        let (tokens, rhs) = parse_funccall env tokens in
        aux (Mul (lhs, rhs)) tokens
      | Slash::tokens ->
        let (tokens, rhs) = parse_funccall env tokens in
        aux (Div (lhs, rhs)) tokens
      | _ -> (tokens, lhs) in
    let (tokens, ast) = parse_funccall env tokens in
    aux ast tokens
  and
    parse_additive env tokens =
    let rec aux lhs tokens = match tokens with
      | Plus::tokens ->
        let (tokens, rhs) = parse_multiplicative env tokens in
        aux (Add (lhs, rhs)) tokens
      | Minus::tokens ->
        let (tokens, rhs) = parse_multiplicative env tokens in
        aux (Sub (lhs, rhs)) tokens
      | _ -> (tokens, lhs) in
    let (tokens, ast) = parse_multiplicative env tokens in
    aux ast tokens
  and
    parse_expression env tokens = parse_additive env tokens in

  let symbols = HashMap.empty in
  let symbols = HashMap.add "pi" (Var ("pi", Some (-8))) symbols in
  let symbols = HashMap.add "id" (Var ("id", Some (-16))) symbols in
  let symbols = HashMap.add "add1" (Var ("add1", Some (-24))) symbols in
  let symbols = HashMap.add "add" (Var ("add", Some (-32))) symbols in
  let env = {symbols; funcdefs = []} in
  let (tokens, ast) = parse_expression env tokens in
  if tokens <> [] then failwith "invalid token sequence"
  else
    ("main", ast)::env.funcdefs
;;

let rec generate funcdefs =
  let tag_int reg = sprintf "sal %s, 1\nor %s, 1" reg reg in
  let untag_int reg = sprintf "sar %s, 1" reg in
  let rec aux = function
    | Int num -> sprintf "mov rax, %d\n%s\npush rax" num (tag_int "rax")
    | Add (lhs, rhs) -> String.concat "\n" [
        aux lhs;
        aux rhs;
        "pop rdi";
        untag_int "rdi";
        "pop rax";
        untag_int "rax";
        "add rax, rdi";
        tag_int "rax";
        "push rax" ]
    | Sub (lhs, rhs) -> String.concat "\n" [
        aux lhs;
        aux rhs;
        "pop rdi";
        untag_int "rdi";
        "pop rax";
        untag_int "rax";
        "sub rax, rdi";
        tag_int "rax";
        "push rax" ]
    | Mul (lhs, rhs) -> String.concat "\n" [
        aux lhs;
        aux rhs;
        "pop rdi";
        untag_int "rdi";
        "pop rax";
        untag_int "rax";
        "imul rax, rdi";
        tag_int "rax";
        "push rax" ]
    | Div (lhs, rhs) -> String.concat "\n" [
        aux lhs;
        aux rhs;
        "pop rdi";
        untag_int "rdi";
        "pop rax";
        untag_int "rax";
        "cqo";
        "idiv rdi";
        tag_int "rax";
        "push rax" ]
    | Var (_, Some offset) -> String.concat "\n" [
        sprintf "mov rax, [rbp + %d]" offset;
        "push rax" ]
    | FuncCall (func, args) -> String.concat "\n" [
        aux func;
        String.concat "\n" (List.map aux args);
        String.concat "\n" (
          List.map (fun (_, reg) -> "pop " ^ reg) (
            List.filter (fun (index, reg) -> index < List.length args) [
              (0, "rax"); (1, "rbx"); (2, "rdi"); (3, "rsi"); (4, "rdx");
              (5, "rcx"); (6, "r8"); (7, "r9"); (8, "r12"); (9, "r13") ]));
        "pop r10";
        "call r10";
        "push rax" ]
    | _ -> failwith "unexpected ast" in
  let rec generate = function
    | (genname, func)::funcdefs -> String.concat "\n" [
        genname ^ ":";
        "mov rbp, rsp";
        if genname <> "main" then ""
        else
          String.concat "\n" [
            "sub rsp, 64";
            "mov rax, 7";
            "mov [rbp - 8], rax";
            "lea rax, [rip + id]";
            "mov [rbp - 16], rax";
            "lea rax, [rip + add1]";
            "mov [rbp - 24], rax";
            "lea rax, [rip + add]";
            "mov [rbp - 32], rax" ];
        aux func;
        "pop rax";
        "sar rax, 1";
        "mov rsp, rbp";
        "ret" ]
    | [] -> ""
  in

  generate funcdefs
;;

let code = generate (parse (tokenize 0)) in
print_string (String.concat "\n" [
    ".intel_syntax noprefix";
    "id:";
    "ret";
    "add1:";
    "sar rax, 1";
    "add rax, 1";
    "sal rax";
    "or rax, 1";
    "ret";
    "add:";
    "sar rax, 1";
    "sar rbx, 1";
    "add rax, rbx";
    "sal rax";
    "or rax, 1";
    "sal rbx";
    "or rbx, 1";
    "ret";
    ".global main";
    code;
    "\n"
  ]);;
