external ref : 'a -> 'a ref = "aqaml_ref"

external raise : exn -> 'a = "aqaml_raise"

external exit : int -> 'a = "aqaml_exit"

external print_string : string -> unit = "aqaml_print_string"

module Char = struct
  external code : char -> int = "aqaml_char_code"
end

type bytes = string

module Bytes = struct
  external length : bytes -> int = "aqaml_string_length"

  external get : bytes -> int -> char = "aqaml_string_get"

  external set : bytes -> int -> char -> unit = "aqaml_string_set"

  external create : int -> bytes = "aqaml_string_create"

  external blit :
    bytes -> int -> bytes -> int -> int -> unit
    = "aqaml_string_blit"

  external blit_string :
    string -> int -> bytes -> int -> int -> unit
    = "aqaml_string_blit"

  let of_string str = str

  let to_string bytes = bytes
end

module String = struct
  external length : string -> int = "aqaml_string_length"

  external get : string -> int -> char = "aqaml_string_get"

  external set : string -> int -> char -> unit = "aqaml_string_set"

  external create : int -> bytes = "aqaml_string_create"

  external blit :
    string -> int -> bytes -> int -> int -> unit
    = "aqaml_string_blit"
end

exception Failure of string

let ignore _ = ()

let failwith str = raise (Failure str)

let int_of_char ch = Char.code ch

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

  (* TODO: this 'rec' is needed due to missing implementation *)
  let rec hd = function x :: xs -> x | [] -> failwith "hd"

  (* TODO: this 'rec' is needed due to missing implementation *)
  let rec tl = function x :: xs -> xs | [] -> failwith "tl"

  let rec concat = function x :: xs -> x @ concat xs | [] -> []

  let flatten lst = concat lst

  let filter f lst =
    let rec aux acc = function
      | x :: xs -> aux (if f x then x :: acc else acc) xs
      | [] -> acc
    in
    List.rev (aux [] lst)
end

exception Match_failure of string * int * int

exception Not_found
