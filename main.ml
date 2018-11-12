open Printf;;
open Scanf;;

let next_int () = Scanf.scanf "%d" (fun i -> i);;

let num = next_int () in
printf ".global main\nmain:\n\tmov $%d, %%eax\n\tret\n" num;;
