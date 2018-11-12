open Printf;;
open Scanf;;

let digit x = match x with
  | '0'..'9' -> (int_of_char x) - (int_of_char '0')
  | _ -> failwith "unexpected char: not digit";;

let program = read_line ();;

exception EOF;;
let next_char i =
  if i < String.length program then i + 1, String.get program i else raise EOF;;

let rec next_int i acc =
  try
    let (i, ch) = next_char i in
    match ch with
    | '0'..'9' -> next_int i (acc * 10 + digit ch)
    | _ -> (i - 1, acc)
  with
    EOF -> (i, acc);;

type token =
  | IntLiteral of int
  | Plus;;

let rec tokenize i =
  try
    let (i, ch) = next_char i in
    match ch with
    | '0'..'9' ->
      let (i, num) = next_int (i - 1) 0 in (IntLiteral num)::tokenize i
    | '+' -> Plus::tokenize i
    | _ -> failwith "unexpected char"
  with
    EOF -> [];;

type ast =
  | Int of int
  | Add of (ast * ast);;

let rec parse = function
  | (IntLiteral num)::Plus::tokens -> Add (Int num, parse tokens)
  | (IntLiteral num)::[] -> Int num
  | _ -> failwith "unexpected token";;

let rec generate ast =
  let
    tag_int reg = sprintf "sal %s, 1\nor %s, 1" reg reg and
  untag_int reg = sprintf "sar %s, 1" reg
  in
  match ast with
  | Int num -> sprintf "mov rax, %d\n%s\npush rax" num (tag_int "rax")
  | Add (lhs, rhs) -> String.concat "\n" [
      generate lhs;
      generate rhs;
      "pop rax";
      untag_int "rax";
      "pop rdi";
      untag_int "rdi";
      "add rax, rdi";
      tag_int "rax";
      "push rax" ]
  | _ -> failwith "unexpected ast";;

let code = generate (parse (tokenize 0)) in
printf ".intel_syntax noprefix\n.global main\nmain:\n%s\npop rax\nsar rax, 1\nret\n" code;;
