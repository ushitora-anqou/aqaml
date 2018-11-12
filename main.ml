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
  | IntLiteral of int;;

let rec tokenize i =
  try
    let (i, ch) = next_char i in
    match ch with
    | '0'..'9' ->
      let (i, num) = next_int (i - 1) 0 in (IntLiteral num)::tokenize i
    | _ -> failwith "unexpected char"
  with
    EOF -> [];;

type ast =
  | Int of int;;

let rec parse = function
  | token::tokens ->
    begin
      match token with
      | IntLiteral num -> Int num
    end::parse tokens
  | [] -> [];;

let rec generate = function
  | ast::asts ->
    begin
      match ast with
      | Int num -> sprintf "mov $%d, %%rax\nsal $1, %%rax\nor $1, %%rax\npush %%rax" num
    end::generate asts
  | [] -> [];;

let code = generate (parse (tokenize 0)) in
printf ".global main\nmain:\n%s\npop %%rax\nsar $1, %%rax\nret\n" (String.concat "\n" code);;
