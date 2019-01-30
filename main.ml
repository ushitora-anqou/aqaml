open Printf
open Helper
open Lexer
open Parser
open Analyzer
open Generator

let input_lines inch =
  let rec aux lines =
    try
      let line = input_line inch in
      aux (line :: lines)
    with End_of_file -> lines
  in
  String.concat "\n" (List.rev (aux []))

let read_file path =
  let inch = open_in path in
  let str = input_lines inch in
  close_in inch ; str

let get_modulename_from_path path =
  let rec aux i fi ti =
    if i < 0 then
      String.init
        (ti - fi + 1)
        (fun i ->
          let idx = i + fi in
          if i = 0 && path.[idx] >= 'a' then
            Char.chr @@ (Char.code path.[idx] - 32)
          else path.[idx] )
    else
      match path.[i] with
      | '.' -> aux (i - 1) (i - 1) (i - 1)
      | '/' -> aux (-1) fi ti
      | _ -> aux (i - 1) i ti
  in
  let i = String.length path - 1 in
  aux i i i

;;
try
  let buf = Buffer.create 128 in
  Array.iteri
    (fun i path ->
      if i >= 1 then
        appfmt buf ";;module %s = struct %s end;;"
          (get_modulename_from_path path)
          (read_file path) )
    Sys.argv ;
  Buffer.contents buf |> tokenize |> parse |> analyze |> generate
  |> printf ".intel_syntax noprefix\n%s"
with Failure str -> eprintf "[AQaml Error] %s\n" @@ str
