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

let tokens = tokenize 0 in
match List.nth tokens 0 with
| IntLiteral num ->
  printf ".global main\nmain:\n\tmov $%d, %%eax\n\tret\n" num;;
