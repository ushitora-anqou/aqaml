external ref : 'a -> 'a ref = "aqaml_ref"

external raise : exn -> 'a = "aqaml_raise"

external exit : int -> 'a = "aqaml_exit"

external print_string : string -> unit = "aqaml_print_string"

external prerr_string : string -> unit = "aqaml_prerr_string"

external string_of_int : int -> string = "aqaml_string_of_int"

external int_of_char : char -> int = "aqaml_char_code"

exception Match_failure of string * int * int

exception End_of_file

exception Not_found

exception Failure of string

exception Sys_error of string

type in_channel = {descriptor: int}

external _aqaml_get_stdin : unit -> in_channel = "aqaml_get_stdin"

let stdin = _aqaml_get_stdin ()

external input_char : in_channel -> char = "aqaml_input_char"

external open_in : string -> in_channel = "aqaml_open_in"

external close_in : string -> in_channel = "aqaml_close_in"

type 'a option = Some of 'a | None

let ignore _ = ()

let failwith str = raise (Failure str)

let max a b = if a < b then b else a

module Char = struct
  external code : char -> int = "aqaml_char_code"
end

type bytes = string

module Bytes = struct
  external length : bytes -> int = "aqaml_string_length"

  external get : bytes -> int -> char = "aqaml_string_get"

  external set : bytes -> int -> char -> unit = "aqaml_string_set"

  external create : int -> bytes = "aqaml_string_create"

  external sub : bytes -> int -> int -> bytes = "aqaml_string_sub"

  external sub_string : bytes -> int -> int -> string = "aqaml_string_sub"

  external blit :
    bytes -> int -> bytes -> int -> int -> unit
    = "aqaml_string_blit"

  external blit_string :
    string -> int -> bytes -> int -> int -> unit
    = "aqaml_string_blit"

  let of_string str = str

  let to_string bytes = bytes
end

module List = struct
  let rec length = function _ :: xs -> 1 + length xs | _ -> 0

  let rec fold_left f a bs =
    match bs with b :: bs -> fold_left f (f a b) bs | _ -> a

  let rev lst =
    let rec aux acc = function x :: xs -> aux (x :: acc) xs | [] -> acc in
    aux [] lst

  let rec iter f = function x :: xs -> f x ; iter f xs | [] -> ()

  let rec iteri f =
    let rec aux i = function
      | x :: xs ->
          f i x ;
          aux (i + 1) xs
      | [] -> ()
    in
    aux 0

  let map f lst =
    let rec aux acc = function x :: xs -> aux (f x :: acc) xs | [] -> acc in
    List.rev (aux [] lst)

  let mapi f lst =
    let rec aux i acc = function
      | x :: xs -> aux (i + 1) (f i x :: acc) xs
      | [] -> acc
    in
    List.rev (aux 0 [] lst)

  let rec rev_append l1 l2 =
    match l1 with x :: xs -> rev_append xs (x :: l2) | [] -> l2

  let hd = function x :: xs -> x | [] -> failwith "hd"

  let tl = function x :: xs -> xs | [] -> failwith "tl"

  let rec concat = function x :: xs -> x @ concat xs | [] -> []

  let flatten lst = concat lst

  let filter f lst =
    let rec aux acc = function
      | x :: xs -> aux (if f x then x :: acc else acc) xs
      | [] -> acc
    in
    List.rev (aux [] lst)

  let rec find p = function
    | x :: xs -> if p x then x else find p xs
    | [] -> raise Not_found
end

module String = struct
  external length : string -> int = "aqaml_string_length"

  external get : string -> int -> char = "aqaml_string_get"

  external set : string -> int -> char -> unit = "aqaml_string_set"

  external create : int -> bytes = "aqaml_string_create"

  external sub : string -> int -> int -> string = "aqaml_string_sub"

  external make : int -> char -> string = "aqaml_string_make"

  external blit :
    string -> int -> bytes -> int -> int -> unit
    = "aqaml_string_blit"

  let concat sep = function
    | [] -> ""
    | lst ->
        let seplen = length sep in
        let sum_length = List.fold_left (fun acc s -> acc + length s) 0 lst in
        let buf =
          Bytes.create @@ (sum_length + (seplen * (List.length lst - 1)))
        in
        let rec aux pos = function
          | [] -> ()
          | [hd] -> Bytes.blit_string hd 0 buf pos @@ length hd
          | hd :: tl ->
              let hdlen = length hd in
              Bytes.blit_string hd 0 buf pos hdlen ;
              Bytes.blit_string sep 0 buf (pos + hdlen) seplen ;
              aux (pos + hdlen + seplen) tl
        in
        aux 0 lst ; Bytes.to_string buf
end

module Buffer = struct
  type t = {mutable buf: bytes; mutable len: int}

  let create size = {buf= Bytes.create size; len= 0}

  let contents src = Bytes.sub_string src.buf 0 src.len

  let length buf = buf.len

  let rec resize src size =
    if Bytes.length src.buf < size then (
      let new_buf = Bytes.create @@ (Bytes.length src.buf * 2) in
      Bytes.blit src.buf 0 new_buf 0 src.len ;
      src.buf <- new_buf ;
      resize src size )

  let add_char src ch =
    resize src (src.len + 1) ;
    Bytes.set src.buf src.len ch ;
    src.len <- src.len + 1

  let add_string src str =
    let str_len = String.length str in
    resize src (src.len + str_len) ;
    Bytes.blit_string str 0 src.buf src.len str_len ;
    src.len <- src.len + str_len
end

module Printf = struct
  external ksprintf :
    (string -> 'd) -> ('a, unit, string, 'd) format4 -> 'a
    = "aqaml_printf_ksprintf"

  let sprintf fmt = ksprintf (fun str -> str) fmt

  let printf fmt = ksprintf (fun str -> print_string str) fmt

  let eprintf fmt = ksprintf (fun str -> prerr_string str) fmt
end

module Array = struct
  external get : 'a array -> int -> 'a = "aqaml_array_get"
end

let read_line () =
  let buf = Buffer.create 65 in
  let rec aux () =
    let ch = try Some (input_char stdin) with End_of_file -> None in
    match ch with
    | Some '\n' -> ()
    | None -> if Buffer.length buf = 0 then raise End_of_file else ()
    | Some ch -> Buffer.add_char buf ch ; aux ()
  in
  aux () ; Buffer.contents buf

let not x = if x then false else true
