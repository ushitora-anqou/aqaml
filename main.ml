open Printf;;
open Scanf;;

let digit x = match x with
  | '0'..'9' -> (int_of_char x) - (int_of_char '0')
  | _ -> failwith "unexpected char: not digit"
;;

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
  | Let
  | Equal
  | In
  | Rec
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
      let (i, str) = next_ident (i - 1) in
      begin match str with
        | "let" -> Let
        | "in" -> In
        | "rec" -> Rec
        | _ -> Ident str
      end::tokenize i
    | '+' -> Plus::tokenize i
    | '-' -> Minus::tokenize i
    | '*' -> Star::tokenize i
    | '/' -> Slash::tokenize i
    | '(' -> LParen::tokenize i
    | ')' -> RParen::tokenize i
    | '=' -> Equal::tokenize i
    | _ -> failwith (sprintf "unexpected char: '%c'" ch)
  with
    EOF -> [];;

type ast =
  | Int of int
  | Add of (ast * ast)
  | Sub of (ast * ast)
  | Mul of (ast * ast)
  | Div of (ast * ast)
  | Var of (string)
  | FuncCall of (ast * ast list)
  | LetVar of (string * ast * ast)
  | LetRec of (string * string * ast * ast)
;;

let parse tokens =
  let rec
    parse_primary = function
    | (IntLiteral num)::tokens -> (tokens, Int num)
    | (Ident id)::tokens -> (tokens, Var id)
    | LParen::tokens ->
      let (tokens, ast) = parse_expression tokens in
      begin match tokens with
        | RParen::tokens -> (tokens, ast)
        | _ -> failwith "unexpected token"
      end
    | _ -> failwith "unexpected token"
  and
    parse_funccall tokens =
    let rec aux tokens = match tokens with
      | (IntLiteral _ | Ident _ | LParen)::_ ->    (* if primary *)
        let (tokens, arg) = parse_primary tokens in
        let (tokens, args) = aux tokens in
        (tokens, arg::args)
      | _ -> (tokens, []) in
    let (tokens, func) = parse_primary tokens in
    let (tokens, args) = aux tokens in
    if args = [] then (tokens, func)    (* not function call *)
    else (tokens, FuncCall (func, args))
  and
    parse_multiplicative tokens =
    let rec aux lhs tokens = match tokens with
      | Star::tokens ->
        let (tokens, rhs) = parse_funccall tokens in
        aux (Mul (lhs, rhs)) tokens
      | Slash::tokens ->
        let (tokens, rhs) = parse_funccall tokens in
        aux (Div (lhs, rhs)) tokens
      | _ -> (tokens, lhs) in
    let (tokens, ast) = parse_funccall tokens in
    aux ast tokens
  and
    parse_additive tokens =
    let rec aux lhs tokens = match tokens with
      | Plus::tokens ->
        let (tokens, rhs) = parse_multiplicative tokens in
        aux (Add (lhs, rhs)) tokens
      | Minus::tokens ->
        let (tokens, rhs) = parse_multiplicative tokens in
        aux (Sub (lhs, rhs)) tokens
      | _ -> (tokens, lhs) in
    let (tokens, ast) = parse_multiplicative tokens in
    aux ast tokens
  and
    parse_let tokens = match tokens with
    | Let::(Ident varname)::Equal::tokens ->
      let (tokens, lhs) = parse_expression tokens in
      begin match tokens with
        | In::tokens ->
          let (tokens, rhs) = parse_expression tokens in
          (tokens, LetVar (varname, lhs, rhs))
        | _ -> failwith "unexpected token"
      end
    | Let::Rec::(Ident funcname)::(Ident argname)::Equal::tokens ->
      let (tokens, func) = parse_expression tokens in
      begin match tokens with
        | In::tokens ->
          let (tokens, body) = parse_expression tokens in
          (tokens, LetRec (funcname, argname, func, body))
        | _ -> failwith "unexpected token"
      end
    | _ -> parse_additive tokens
  and
    parse_expression tokens = parse_let tokens in

  let (tokens, ast) = parse_expression tokens in
  if tokens = [] then ast else failwith "invalid token sequence"
;;

module HashMap = Map.Make(String);;
type environment = {symbols: ast HashMap.t};;
let analyze ast =
  let letrecs = ref [] in
  let rec aux env ast =
    match ast with
    | Int _ -> ast
    | Add (lhs, rhs) -> Add (aux env lhs, aux env rhs)
    | Sub (lhs, rhs) -> Sub (aux env lhs, aux env rhs)
    | Mul (lhs, rhs) -> Mul (aux env lhs, aux env rhs)
    | Div (lhs, rhs) -> Div (aux env lhs, aux env rhs)
    | Var (name) -> HashMap.find name env.symbols
    | FuncCall (func, args) -> FuncCall (aux env func, List.map (aux env) args)
    | LetVar (varname, lhs, rhs) ->
      let env' = {env with symbols = HashMap.add varname (Var varname) env.symbols} in
      LetVar (varname, (aux env lhs), (aux env' rhs))
    | LetRec (funcname, argname, func, body) ->   (* TODO: allow recursion *)
      let env' = {env with symbols = HashMap.add argname (Var argname) env.symbols} in
      let func = aux env' func in
      let env' = {env with symbols = HashMap.add funcname (Var funcname) env.symbols} in
      let ast = LetRec (funcname, argname, func, aux env' body) in
      letrecs := ast::(!letrecs);
      ast
  in

  let symbols = HashMap.empty in
  let ast = aux {symbols} ast in
  (ast, !letrecs)
;;

type gen_environment = {offset: int; varoffset: int HashMap.t};;
let rec generate (ast, letrecs) =
  let tag_int reg = sprintf "sal %s, 1\nor %s, 1" reg reg in
  let untag_int reg = sprintf "sar %s, 1" reg in
  let stack_size = ref 0 in

  let rec aux env ast = match ast with
    | Int num -> sprintf "mov rax, %d\n%s\npush rax" num (tag_int "rax")
    | Add (lhs, rhs) -> String.concat "\n" [
        aux env lhs;
        aux env rhs;
        "pop rdi";
        untag_int "rdi";
        "pop rax";
        untag_int "rax";
        "add rax, rdi";
        tag_int "rax";
        "push rax" ]
    | Sub (lhs, rhs) -> String.concat "\n" [
        aux env lhs;
        aux env rhs;
        "pop rdi";
        untag_int "rdi";
        "pop rax";
        untag_int "rax";
        "sub rax, rdi";
        tag_int "rax";
        "push rax" ]
    | Mul (lhs, rhs) -> String.concat "\n" [
        aux env lhs;
        aux env rhs;
        "pop rdi";
        untag_int "rdi";
        "pop rax";
        untag_int "rax";
        "imul rax, rdi";
        tag_int "rax";
        "push rax" ]
    | Div (lhs, rhs) -> String.concat "\n" [
        aux env lhs;
        aux env rhs;
        "pop rdi";
        untag_int "rdi";
        "pop rax";
        untag_int "rax";
        "cqo";
        "idiv rdi";
        tag_int "rax";
        "push rax" ]
    | Var varname ->
      let offset = HashMap.find varname env.varoffset in
      String.concat "\n" [
        sprintf "mov rax, [rbp + %d]" offset;
        "push rax" ]
    | FuncCall (func, args) -> String.concat "\n" [
        aux env func;
        String.concat "\n" (List.map (aux env) args);
        String.concat "\n" (
          List.map (fun (_, reg) -> "pop " ^ reg) (
            List.filter (fun (index, reg) -> index < List.length args) [
              (0, "rax"); (1, "rbx"); (2, "rdi"); (3, "rsi"); (4, "rdx");
              (5, "rcx"); (6, "r8"); (7, "r9"); (8, "r12"); (9, "r13") ]));
        "pop r10";
        "call r10";
        "push rax" ]
    | LetVar (varname, lhs, rhs) ->
      let lhs_code = aux env lhs in
      let offset = env.offset - 8 in
      stack_size := max !stack_size (-offset);
      let env = {offset; varoffset = HashMap.add varname offset env.varoffset} in
      String.concat "\n" [
        lhs_code;
        "pop rax";
        sprintf "mov [rbp + %d], rax" offset;
        aux env rhs ]
    | LetRec (funcname, _, _, body) ->
      let offset = env.offset - 8 in
      stack_size := max !stack_size (-offset);
      let env = {offset; varoffset = HashMap.add funcname offset env.varoffset} in
      String.concat "\n" [
        sprintf "lea rax, [rip + %s]" funcname;
        sprintf "mov [rbp + %d], rax" offset;
        aux env body ]
    | _ -> failwith "unexpected ast" in

  let env = {offset = 0; varoffset = HashMap.empty} in
  stack_size := 0;
  let code = aux env ast in
  let main_code = String.concat "\n" [
      "main:";
      "push rbp";
      "mov rbp, rsp";
      sprintf "sub rsp, %d" !stack_size;
      code;
      "pop rax";
      "sar rax, 1";
      "mov rsp, rbp";
      "pop rbp";
      "ret\n" ] in
  let letrecs_code = String.concat "\n" (List.map (fun (LetRec (funcname, argname, func, _)) ->
      let arg_offset = -8 in
      let env = {offset = arg_offset; varoffset = HashMap.singleton argname arg_offset} in
      stack_size := -arg_offset;
      let code = aux env func in
      String.concat "\n" [
        funcname ^ ":";
        "push rbp";
        "mov rbp, rsp";
        sprintf "sub rsp, %d" !stack_size;
        sprintf "mov [rbp + %d], rax" arg_offset;
        code;
        "pop rax";
        "mov rsp, rbp";
        "pop rbp";
        "ret\n" ]) letrecs) in
  main_code ^ letrecs_code
;;

let ast = parse (tokenize 0) in
let code = generate (analyze ast) in
print_string (String.concat "\n" [
    ".intel_syntax noprefix";
    ".global main";
    code ])
;;
