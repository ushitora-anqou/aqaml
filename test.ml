let id x = x

let pi = 3

let helloworld = "Hello, world!\n"

;;
print_string "Test starts.\n"

let test got expect = if got <> expect then ( print_string "ERROR\n" ; exit 1 )

;;
test 42 42

;;
test (1 + 2 + 3) 6

;;
test (1 + 2) 3

;;
test (2 - 1) 1

;;
test (3 - 2 - 1) 0

;;
test (2 * 3) 6

;;
test (4 / 2) 2

;;
test (1 + (2 * 3)) 7

;;
test ((2 * 3) + 1) 7

;;
test (1 + (2 * 3)) 7

;;
test ((4 / 2 * 3) + 1 - (10 / 2) + (4 * 2 * 1)) 10

;;
test ((1 + 2) * 3) 9

;;
test ((1 + 2) * (3 + 4)) 21

;;
test (33 * (1 + 2) / 3) 33

;;
test
  (let pi = 3 in
   pi)
  3

;;
test
  (let pi = 3 in
   pi * 2)
  6

;;
test
  (let pi = 3 in
   pi + 2)
  5

;;
test
  (let pi = 3 in
   10 - pi)
  7

;;
test
  (let x = 1 in
   let y = 2 in
   x + y)
  3

;;
test
  (let x = 1 in
   let y = 2 in
   let z = 3 in
   let w = x + (y * z) in
   w * 2)
  14

;;
test
  (let x = 1 in
   let x = x + 1 in
   x)
  2

;;
test
  (let x =
     let y = 2 in
     y + 2
   in
   x + 2)
  6

;;
test
  (let id x = x in
   id 10)
  10

;;
test
  (let id x = x in
   id (id 10))
  10

;;
test
  (let id x = x in
   1 + id 2 + 3)
  6

;;
test
  (let add1 x = x + 1 in
   add1 2 * 3)
  9

;;
test
  (let add1 x = x + 1 in
   add1 (2 * 3))
  7

;;
test
  (let add1 x = x + 1 in
   let add1 x = x + 2 in
   add1 2)
  4

;;
test
  (let add x y = x + y in
   add 2 3)
  5

;;
test
  (let add x y = x + y in
   add (add 2 3) (add 3 4))
  12

;;
test
  (let add x y = x + y in
   2 * add (add 2 3) (add 3 4))
  24

;;
test (if 1 = 1 then 1 else 0) 1

;;
test (if 2 = 1 then 1 else 0) 0

;;
test
  (let add x y = x + y in
   if add 2 3 = add 3 2 then 1 else 0)
  1

;;
test
  (let sub x y = x - y in
   sub 3 2)
  1

;;
test
  (let calc x y z = (y * z) - (x * x) in
   calc 2 3 4)
  8

;;
test
  (let rec pow n m = if m = 0 then 1 else pow n (m - 1) * n in
   pow 2 6)
  64

;;
test
  (let rec fib n = if n <= 1 then n else fib (n - 1) + fib (n - 2) in
   fib 13)
  233

;;
test
  (let rec trib n =
     if n <= 1 then 0
     else if n = 2 then 1
     else trib (n - 1) + trib (n - 2) + trib (n - 3)
   in
   trib 11)
  149

;;
test
  (let rec lucas n =
     if n = 0 then 2 else if n = 1 then 1 else lucas (n - 1) + lucas (n - 2)
   in
   lucas 11)
  199

;;
test (if 1 <> 2 then 1 else 0) 1

;;
test (if 1 <> 1 then 1 else 0) 0

;;
test (if 1 < 2 then 1 else 0) 1

;;
test (if 2 < 1 then 1 else 0) 0

;;
test (if 1 > 2 then 1 else 0) 0

;;
test (if 2 > 1 then 1 else 0) 1

;;
test (if 1 <= 2 then 1 else 0) 1

;;
test (if 2 <= 1 then 1 else 0) 0

;;
test (if 1 <= 1 then 1 else 0) 1

;;
test (if 1 >= 2 then 1 else 0) 0

;;
test (if 1 >= 1 then 1 else 0) 1

;;
test
  (let not x = if x then false else true in
   if not true then 1 else 0)
  0

;;
test
  (let a, b, c = (10, 20, 30) in
   a + b - c)
  0

;;
test
  (let x = (4, 2, 3) in
   let a, b, c = x in
   (a * b) - c)
  5

;;
test
  (let (a, b), c = ((10, 40), 3) in
   (b / a) - c)
  1

;;
test
  (let 1 = 1 in
   let (10, b), c = ((10, 40), 3) in
   (b / 10) - c)
  1

;;
test
  (let f x y = x - y in
   let t = (f 10 2, f 5 4) in
   let a, b = t in
   a - b)
  7

;;
test
  (let f (x, y) = x - y in
   let t = (f (10, 2), f (5, 4)) in
   let a, b = t in
   a - b)
  7

;;
test
  (let f (x, y, z, w) (a, b, c) = ((x / y) - z + w) * (a - (b / c)) in
   f (10, 2, 3, 4) (20, 14, 7))
  108

;;
test (if (1, 2, 3) = (1, 2, 3) then 1 else 0) 1

;;
test (if (1, 2, 4) = (1, 2, 3) then 1 else 0) 0

;;
test (if (1, 2, 3) <> (1, 2, 3) then 1 else 0) 0

;;
test (if (1, 2, 4) <> (1, 2, 3) then 1 else 0) 1

;;
test
  (let x = (1, 2, 3) in
   let y = (1, 2, 3) in
   if x = y then 1 else 0)
  1

;;
test (if [] = [] then 1 else 0) 1

;;
test
  (let (x :: xs) = [1; 2; 3] in
   if x = 1 then 1 else 0)
  1

;;
test
  (let (x :: xs) = [1; 2; 3] in
   let (y :: ys) = xs in
   let (z :: zs) = ys in
   if z = 3 then 1 else 0)
  1

;;
test
  (let f (x :: xs, y) z = x - (y * z) in
   f ([10; 20], 2) 3)
  4

;;
test
  (let f ([x; y], z) w = x - (y * z) + w in
   let add x y = x + y in
   let sub x y = x - y in
   let div x y = x / y in
   f ([add 10 2; sub 5 1], div 6 4) 10)
  18

;;
test
  (let f ([a; b; c], [d; e; f], [g; h; i]) = f in
   f ([1; 2; 3], [4; 5; 6], [7; 8; 9]))
  6

;;
test (if [1; 2; 3] = [1; 2; 3] then 1 else 0) 1

;;
test (if [1; 2; 4] = [1; 2; 3] then 1 else 0) 0

;;
test (if [1; 2; 3] <> [1; 2; 3] then 1 else 0) 0

;;
test (if [1; 2; 4] <> [1; 2; 3] then 1 else 0) 1

;;
test
  (let str = "aqaml_\t\ndebug_str" in
   let str2 = "aqaml_debug\\\"_str2" in
   String.length str + String.length str2)
  35

;;
test
  (let [x] =
     [ (let x = 10 in
        x ; 2) ]
   in
   let y, z = 1 ; 2 ; (3, 4) in
   (x * y) - z)
  2

;;
test
  (let x = 1 ; 2 ; 3 in
   x)
  3

;;
test
  (let f x = x + 1 in
   f 1 ; 3)
  3

;;
test
  ( print_string "Hello, world!\n" ;
    10 )
  10

;;
test
  ( if true then print_string "Happy, Halloween\n" ;
    20 )
  20

;;
test (if () = () then 1 else 0) 1

;;
test (if (if false then print_string "Never shown") = () then 1 else 0) 1

;;
test (id 10) 10

;;
test pi 3

;;
test (if "abc" = "abc" then 1 else 0) 1

;;
test (if "abc" = "def" then 1 else 0) 0

;;
test (if "abcdef" = "abc" then 1 else 0) 0

;;
test (if "abc" = "abcdef" then 1 else 0) 0

;;
test (if "abc" <> "abc" then 1 else 0) 0

;;
test (if "abc" <> "def" then 1 else 0) 1

;;
test (if "abcdef" <> "abc" then 1 else 0) 1

;;
test (if "abc" <> "abcdef" then 1 else 0) 1

;;
test (match 10 with 10 -> 12) 12

;;
test (match 10 with x -> x + 2) 12

;;
test
  ( match [(1, 2); (3, 4); (5, 6)] with [(1, x); (3, y); (z, 6)] -> (x * y) - z
  )
  3

;;
test (match 10 with 1 -> 2 | 3 -> 4 | 10 -> 12 | x -> x + 3) 12

;;
let f x = match x with [10; 20; x], 1 -> x / 2 | [20; x; 30], y -> x - y in
test (f ([10; 20; 30], 1)) 15 ;
test (f ([20; 10; 30], 3)) 7

;;
let f x =
  match x with
  | [10; 20; x], (1, y) -> x / y
  | [20; x; 30], (y, 2) -> x - y
  | [x; y; z], (w, v) -> (x * y) - z + (w / v)
  | _, (x, y) -> (x * 2) - y
in
test (f ([10; 20; 30], (1, 3))) 10 ;
test (f ([20; 10; 30], (3, 2))) 7 ;
test (f ([5; 4; 3], (2, 1))) 19 ;
test (f ([], (4, 3))) 5

;;
let length xs =
  let rec aux n xs = match xs with x :: xs -> aux (n + 1) xs | [] -> n in
  aux 0 xs
in
test (length [1; 2; 3]) 3 ;
test (length [1]) 1 ;
test (length []) 0

;;
let f x y =
  match x with 10 -> ( match y with 10 -> 100 | _ -> y + 1 ) | _ -> x * 2
in
test (f 10 10) 100 ;
test (f 10 20) 21 ;
test (f 15 3) 30

let rec fold_left f a bs =
  match bs with b :: bs -> fold_left f (f a b) bs | _ -> a

;;
test (fold_left (fun a b -> (a - b) * 2) 100 [1; 2; 3]) 778 ;
test
  (fold_left (fun a (b, c) -> b :: c :: a) [] [(1, 2); (2, 3); (4, 5)])
  [4; 5; 2; 3; 1; 2]

;;
test ((function 10 -> 12 | x -> x * 2) 3) 6 ;
test ((function 10 -> 12 | x -> x * 2) 10) 12

;;
test
  (let a = 3 in
   let f () = a in
   f ())
  3

let f () = pi

;;
test (f ()) 3

;;
test
  ((let f x = x - 2 in
    f)
     10)
  8

let rec f n = if n = 0 then 1 else f (n - 1) * pi

;;
test (f 3) 27

let f () =
  let rec g n = if n = 0 then 1 else g (n - 1) * pi in
  g

;;
test ((f ()) 3) 27

let rec f () =
  let g () =
    let pi, _ = f () in
    pi
  in
  (pi, g)

;;
test
  (let _, g = f () in
   g ())
  3

let a = 10

let b = 20

let c = 30

let g x y = (x * b) + c

let f x y z = (a * x) + (b * y) + (c * z) + g y z

;;
test (f 2 3 4) 290

;;
let pos x = +x in
let neg x = -x in
test (pos 10) 10 ;
test (pos (-10)) (-10) ;
test (neg 10) (-10) ;
test (neg (-10)) 10

let rec length = function _ :: xs -> 1 + length xs | _ -> 0

;;
test (length [1; 2; 3]) 3

;;
let rec length n = function _ :: xs -> length (n + 1) xs | _ -> n in
test (length 0 [1; 2; 3]) 3

;;
let rec mul n = function x -> n * x * pi in
test (mul 2 3) 18

;;
let f () =
  let g x y = function z -> (x * y) + z in
  g
in
test (f () 4 2 3) 11

;;
let f =
  let g x y = function z -> (x * y) + z in
  g
in
test (f 4 2 3) 11

;;
let f =
  let g x y z = (x * y) + z in
  g
in
test (f 4 2 3) 11

;;
let f =
  let g x = function y -> ( function z -> (x * y) + z ) in
  g
in
test (f 4 2 3) 11

;;
let f =
  let g x y z = function
    | w ->
        let h v a b c d = x + y + z + w + v + a + b + c + d in
        h
  in
  g
in
test (f 0 1 2 3 4 5 6 7 8) 36

let not x = if x then false else true

;;
test (not true) false ; test (not false) true

let abs x = if x > 0 then x else -x

;;
test (abs 10) 10 ;
test (abs (-10)) 10

let alli f lst =
  let rec aux i = function
    | x :: xs -> if not (f i x) then false else aux (i + 1) xs
    | _ -> true
  in
  aux 0 lst

;;
test (alli (fun i x -> x) []) true ;
test (alli (fun i x -> x) [true; false; true]) false ;
test (alli (fun i x -> x) [true; true; true]) true

let rec mem item = function
  | x :: xs -> if x = item then true else mem item xs
  | _ -> false

;;
test (mem 10 [1; 2; 10; 9]) true ;
test (mem 4 [1; 2; 3]) false

let rec length lst =
  let rec aux n = function _ :: xs -> aux (n + 1) xs | _ -> n in
  aux 0 lst

;;
test (length [1; 2; 3]) 3 ;
test (length []) 0

;;
let nqueen n =
  let rec safe q qs =
    if mem q qs then false
    else
      alli
        (fun r' c ->
          let r = r' + n - length qs in
          let y = n - length qs - 1 in
          abs (y - r) - abs (q - c) <> 0 )
        qs
  in
  let rec aux qs =
    if length qs = n then 1
    else
      let rec aux' total c =
        if c = n then total
        else aux' (if safe c qs then total + aux (c :: qs) else total) (c + 1)
      in
      aux' 0 0
  in
  aux []
in
test (nqueen 1) 1 ;
test (nqueen 2) 0 ;
test (nqueen 3) 0 ;
test (nqueen 4) 2 ;
test (nqueen 5) 10 ;
test (nqueen 6) 4 ;
test (nqueen 7) 40 ;
test (nqueen 8) 92

;;
test (match "abc" with "def" -> 0 | "abc" -> 1 | _ -> 2) 1

;;
test
  ( match ("abc", ["def"; "ghi"]) with
  | "def", _ -> 0
  | _, ["def"; _] -> 1
  | _ -> 2 )
  1

;;
test ('a' = 'a') true ;
test ('b' = 'c') false ;
test ('\n' = '\n') true ;
test ('\'' = '\'') true ;
let ch = 'd' in
test (ch = 'd') true ;
test (ch = 'e') false ;
test (match ch with 'a' -> 1 | 'e' -> 0 | 'd' -> 2 | _ -> 3) 2

;;
test
  (match 10 with 1 | 99 | 2 -> -99 | 0 | 3 | 4 -> 1 | 52 | 278 | 10 | 9 -> 2)
  2

;;
let a = "abc" in
let b = 10 in
test
  ( match (a, b) with
  | "def", _ -> 0
  | _, 3 -> 1
  | "abc", x -> ( match x with 35 -> 2 | 9 | 2 -> 3 | x -> x - 2 )
  | _ -> -1 )
  8

;;
test
  (let ((1, 2) as var) = (1, 2) in
   var)
  (1, 2)

;;
test
  ( match (10, 20) with
  | (10, 20) as t -> ( match t with 10, 20 -> true | _ -> false )
  | _ -> false )
  true

;;
let flag = false in
test (match 10 with 10 when flag -> 0 | 20 -> 1 | x when x = 10 -> 3) 3 ;
test
  ( match (1, 2) with
  | x, y when x + y = 2 -> 0
  | 2, y when y = 3 -> 1
  | x, y when x - y = -1 -> 2
  | _ -> 3 )
  2

;;
test
  ( match (1, (2, [3; 4], 5), 6) with
  | 1, (2, ([6; 1] | [_; 5]), _), 99 -> 0
  | _, (2, (([x; 3] | [2; 100; x] | [3; x] | [27; x]) as y), 5), _ -> (
    match y with [a; b] -> a + (b * x) | _ -> 2 )
  | _ -> 1 )
  19

type furikake2 =
  | Noritama
  | Okaka
  | Syake
  | Powder of int
  | PowderSpecial of string

;;
let f = Okaka in
let Noritama = Noritama in
test (match f with Noritama -> 0 | Okaka -> 1 | Syake -> 2) 1

;;
let rec map f = function x :: xs -> f x :: map f xs | _ -> [] in
let fs = [Syake; Okaka; Syake; Powder 20; PowderSpecial "abc"] in
test
  (map
     (function
       | Noritama -> "n"
       | Okaka -> "o"
       | Syake -> "s"
       | Powder _ -> "p"
       | PowderSpecial _ -> "ps")
     fs)
  ["s"; "o"; "s"; "p"; "ps"]

;;
let a = Powder 19 in
let b = Noritama in
test (a = b) false ;
test (a <> b) true

;;
let fu = Powder 19 in
test
  ( match fu with
  | Noritama -> "n"
  | Okaka -> "o"
  | Syake -> "s"
  | PowderSpecial "abc" -> "psABC"
  | PowderSpecial _ -> "ps???"
  | Powder 10 -> "p10"
  | Powder n -> "p??" )
  "p??"

;;
test
  ( match
      [ (Noritama, [Powder 10; Okaka])
      ; (Powder 20, [Noritama; Okaka])
      ; (Syake, [Powder 3; PowderSpecial "abc"]) ]
    with
  | [(_, [Powder 10; Okaka]); (Okaka, [Noritama; Okaka]); x] -> 0
  | [(Noritama, [x; Okaka]); (Powder d, [Noritama; Okaka]); (Syake, _)] ->
      let (Powder e) = x in
      e + d
  | _ -> 2 )
  30

type adder =
  | Adder2 of int * int
  | Adder3 of int * int * int
  | Adder4 of int * int * int * int
  | AdderSpecial of int * (string * int)

;;
let add = function
  | Adder2 (a, b) -> a + b
  | Adder3 (a, b, c) -> a + b + c
  | Adder4 (a, b, c, d) -> a + b + c + d
  | AdderSpecial (a, (str, b)) -> (a * String.length str) + b
in
test (add (Adder2 (10, 20))) 30 ;
test (add (AdderSpecial (23, ("abc", -4)))) 65

(* thanks to http://www.fos.kuis.kyoto-u.ac.jp/~igarashi/class/pl/03-ocaml.html *)
type furikake = Shake | Katsuo | Nori

let isVeggie = function Shake | Katsuo -> false | Nori -> true

;;
test (isVeggie Shake) false

type miso = Aka | Shiro | Awase

type gu = Wakame | Tofu | Radish

type dish = PorkCutlet | Soup of miso * gu | Rice of furikake

;;
test PorkCutlet PorkCutlet ;
test (Soup (Aka, Tofu)) (Soup (Aka, Tofu)) ;
test (Rice Shake) (Rice Shake)

let isSolid = function PorkCutlet | Rice _ -> true | Soup _ -> false

;;
test (isSolid (Rice Shake)) true

let price_of_dish = function
  | PorkCutlet -> 350
  | Soup _ -> 90
  | Rice (Shake | Katsuo) -> 90
  | Rice Nori -> 80

;;
test (price_of_dish (Rice Shake)) 90

;;
let isVeggieDish = function
  | PorkCutlet -> false
  | Soup _ -> true
  | Rice f -> isVeggie f
in
test (isVeggieDish (Rice Katsuo)) false

type 'a option = Some of 'a | None

;;
let div x y = if y = 0 then None else Some (x / y) in
test (div 12 3) (Some 4) ;
test (div 5 0) None

;;
let s = Soup (Aka, Tofu) in
let (Soup (m, g)) = s in
test Aka Aka ; test g Tofu

;;
let find lst t =
  let rec aux = function
    | x :: xs -> if x = t then true else aux xs
    | [] -> false
  in
  aux lst
in
test (find ["1"; "2"; "3"] "2") true

type ('a, 'b) hashmap2 = Data of ('a * 'b) list

;;
let create () = Data [] in
let add (Data m) k v = Data ((k, v) :: m) in
let find (Data m) k =
  let rec aux = function
    | (x, y) :: xs -> if x = k then Some y else aux xs
    | [] -> None
  in
  aux m
in
let m = create () in
let m = add m "kutimane" 3 in
let m = add m "busu" 2 in
let m = add m "kouji" 1 in
let m = add m "sibiri" 1 in
test (find m "busu") (Some 2)

;;
let a = 3 and b = 4 and c = 5 in
test ((a * b) - c) 7 ;
let a = c - a and b = c * a and c = a - (b * c) in
test ((a * b) - c) 47 ;
let f x = (a * x) - 4 and a = 5 in
test (f a) 6

;;
let rec mem item = function
  | x :: xs -> if x = item then true else mem item xs
  | _ -> false
and length = function _ :: xs -> 1 + length xs | _ -> 0 in
test (mem 3 [1; 2; 4]) false ;
test (length ["abc"; "def"]) 2

;;
test ("abc" ^ "") "abc" ;
test ("abc" ^ "def") "abcdef" ;
test ("abc" ^ "defghi") "abcdefghi" ;
test ("abcghi" ^ "def") "abcghidef" ;
test ("abc" ^ "def" ^ "ghi") "abcdefghi" ;
let str = "abcdef" in
test ("<" ^ str ^ ">") "<abcdef>"

;;
let rec even = function 0 -> true | x -> odd (x - 1)
and odd = function 0 -> false | x -> even (x - 1) in
test (even 2) true ;
test (even 3) false ;
test (odd 2) false ;
test (odd 3) true

;;
let t = true and f = false in
(*let rec even = function 0 -> t | x -> odd (x - 1)
and odd = function 0 -> f | x -> even (x - 1) in*)
let rec odd = function 0 -> f | x -> even (x - 1)
and even = function 0 -> t | x -> odd (x - 1) in
test (even 2) true ;
test (even 3) false ;
test (odd 2) false ;
test (odd 3) true

;;
let is_capital = function 'A' .. 'Z' -> true | _ -> false in
test (is_capital 'B') true ;
test (is_capital 'd') false ;
test (is_capital '\n') false

;;
test ([] @ []) [] ;
test ([1; 2] @ []) [1; 2] ;
test ([] @ [1; 2]) [1; 2] ;
test ([1; 2; 3] @ [4; 5; 6]) [1; 2; 3; 4; 5; 6] ;
let f () = [1; 2; 3] in
let g () = [4; 5; 6] in
test (f () @ g ()) [1; 2; 3; 4; 5; 6]

;;
let x = ref 0 in
test !x 0 ;
x := 5 ;
test !x 5 ;
test (!x * 2) 10

;;
let x = ref [1; 2; 3] in
test !x [1; 2; 3] ;
x := 4 :: !x ;
test !x [4; 1; 2; 3]

;;
test
  ( try
      let [] = [1] in
      3
    with Match_failure _ -> 5 )
  5

;;
test
  ( try
      let 1, 2 = (3, 4) in
      10
    with
  | Not_found -> 5
  | Match_failure _ -> 3 )
  3

;;
test
  ( ( try
        try
          let [] = [1] in
          10
        with Not_found -> 5
      with Match_failure _ -> 3 )
  + 5 )
  8

;;
test (try raise Not_found with Not_found -> 10) 10

;;
test
  ( try raise (Match_failure ("//toplevel//", 5, 13)) with Match_failure _ -> 2
  )
  2

exception TestExc

;;
test (try raise TestExc with TestExc -> 10) 10

exception TestExc2 of string

;;
test (try raise (TestExc2 "debug") with TestExc2 str -> str) "debug" ;
test
  ( try (try 10 with Not_found -> 1) + raise (TestExc2 "str")
    with TestExc2 _ -> 5 )
  5

;;
test (5 mod 3) 2 ;
test (-5 mod 3) (-2)

;;
test (5 lsl 2) 20 ;
test (-1 lsl 2) (-4) ;
test (20 lsr 2) 5 ;
test (19 lsr 2) 4 ;
test (-1 lsr 61) 3 ;
test (19 asr 2) 4 ;
test (-1 asr 61) (-1)

type integer = int

and character = char

and mixed = integer * character

and variant = Integer of integer | Character of character | Mixed of mixed

;;
test "abc".[1] 'b' ;
let str = "abcde fghi" in
test str.[5] ' '

type ('a, 'b) hashmap = ('a * 'b) list

let hashmap_empty = []

let hashmap_add k v m = (k, v) :: m

let rec hashmap_find k = function
  | (k', v') :: xs -> if k = k' then v' else hashmap_find k xs
  | [] -> raise Not_found

let hashmap_mem k m =
  try
    ignore (hashmap_find k m) ;
    true
  with Not_found -> false

let hashmap_merge f m1 m2 =
  let src = ref hashmap_empty in
  let rec iter_m1 = function
    | (k, v) :: xs ->
        ( try src := hashmap_add k (Some v, Some (hashmap_find k m2)) !src
          with Not_found -> src := hashmap_add k (Some v, None) !src ) ;
        iter_m1 xs
    | [] -> ()
  in
  let rec iter_m2 = function
    | (k, v) :: xs ->
        if not (hashmap_mem k m1) then src := hashmap_add k (None, Some v) !src ;
        iter_m2 xs
    | [] -> ()
  in
  iter_m1 m1 ;
  iter_m2 m2 ;
  fold_left
    (fun m (k, (l, r)) ->
      match f k l r with None -> m | Some v -> hashmap_add k v m )
    hashmap_empty !src

let hashmap_union f m1 m2 =
  hashmap_merge
    (fun k l r ->
      match (l, r) with
      | None, None -> None
      | Some v, None -> l
      | None, Some v -> r
      | Some v1, Some v2 -> f k v1 v2 )
    m1 m2

let hashmap_cardinal m = length m

;;
let m = hashmap_empty in
(* Thanks to: https://github.com/stereobooster/programming-languages-genealogical-tree *)
let m = hashmap_add "ML" 1977 m in
let m = hashmap_add "SML" 1984 m in
let m = hashmap_add "Caml" 1987 m in
let m = hashmap_add "OCaml" 1996 m in
test (hashmap_find "SML" m) 1984 ;
test (hashmap_find "OCaml" m) 1996 ;
test (try hashmap_find "C" m with Not_found -> -1) (-1) ;
test (try hashmap_find "ML" m with Not_found -> -1) 1977 ;
test (hashmap_mem "ML" m) true ;
test (hashmap_mem "C" m) false ;
let m' = hashmap_empty in
let m' = hashmap_add "ANSI C" 1989 m' in
let m' = hashmap_add "SML" 1990 m' in
let m' = hashmap_add "Caml" 1987 m' in
let m = hashmap_union (fun k l r -> Some r) m m' in
test (hashmap_find "SML" m) 1990 ;
test (hashmap_find "OCaml" m) 1996 ;
test (hashmap_find "Caml" m) 1987 ;
test (hashmap_find "ANSI C" m) 1989

type ('a, 'b) hashtbl = ('a, 'b) hashmap ref

let hashtbl_create size_hint = ref hashmap_empty

let hashtbl_add tbl k v = tbl := hashmap_add k v !tbl

let hashtbl_mem tbl k = hashmap_mem k !tbl

let hashtbl_find tbl k = hashmap_find k !tbl

let hashtbl_length tbl = hashmap_cardinal !tbl

;;
let m = hashtbl_create 16 in
hashtbl_add m "ML" 1977 ;
hashtbl_add m "SML" 1984 ;
hashtbl_add m "Caml" 1987 ;
hashtbl_add m "OCaml" 1996 ;
test (hashtbl_find m "SML") 1984 ;
test (hashtbl_find m "OCaml") 1996 ;
test (try hashtbl_find m "C" with Not_found -> -1) (-1) ;
test (try hashtbl_find m "ML" with Not_found -> -1) 1977 ;
test (hashtbl_mem m "ML") true ;
test (hashtbl_mem m "C") false ;
test (hashtbl_length m) 4

type testrecord1 = {testrecord1_int: int; testrecord1_str: string}

;;
test
  ( {testrecord1_int= 10; testrecord1_str= "abc"}
  = {testrecord1_str= "abc"; testrecord1_int= 10} )
  true ;
test
  ( {testrecord1_int= 10; testrecord1_str= "abc"}
  = {testrecord1_str= "adc"; testrecord1_int= 10} )
  false

;;
test {testrecord1_int= 10; testrecord1_str= "abc"}.testrecord1_int 10 ;
let h = {testrecord1_int= 10; testrecord1_str= "abc"} in
test h.testrecord1_str "abc" ;
let f a b = {testrecord1_int= a; testrecord1_str= b} in
test (f 50 "kekeke").testrecord1_str "kekeke"

;;
let h = {testrecord1_int= 10; testrecord1_str= "abc"} in
test
  {h with testrecord1_str= "def"}
  {testrecord1_int= 10; testrecord1_str= "def"} ;
test {{testrecord1_int= 20; testrecord1_str= "abc"} with testrecord1_int= 10} h

module TestModule1 = struct
  let hoge n = n + 2

  type furikake = Shake | Katsuo | Norikamo

  let f () = hoge 10
end

;;
test (TestModule1.hoge 10) 12 ;
test TestModule1.Katsuo TestModule1.Katsuo ;
test (TestModule1.f ()) 12

(* for self-hosting *)

;;
test (int_of_char 'A') 65 ;
test (int_of_char 'a') 97

;;
test (List.length [1; 2; 3]) 3 ;
test (List.fold_left (fun a b -> (a - b) * 2) 100 [1; 2; 3]) 778 ;
test (List.map (fun x -> x * 10) [1; 2; 3]) [10; 20; 30] ;
test (List.mapi (fun i x -> (x * 10) + i) [1; 2; 3]) [10; 21; 32] ;
let sum = ref 0 in
List.iter (fun x -> sum := !sum + x) [1; 2; 3] ;
test !sum 6 ;
let sum = ref 0 in
List.iteri (fun i x -> sum := !sum + (x * i)) [1; 2; 3] ;
test !sum 8 ;
test (List.rev [1; 2; 3]) [3; 2; 1] ;
let l1 = [2; 3; 4; 5] in
let l2 = [1; -1; 0] in
test (List.rev_append l1 l2) (List.rev l1 @ l2) ;
test (List.hd [-1; 0; 2]) (-1) ;
test (try List.hd [] with Failure _ -> 1) 1 ;
test (List.concat [[1]; [2; 3]; [4; 5; 6]]) [1; 2; 3; 4; 5; 6] ;
test (List.flatten [[1]; [2; 3]; [4; 5; 6]]) [1; 2; 3; 4; 5; 6] ;
test (List.filter (fun x -> x mod 2 = 0) [1; 2; 3; 4; 5; 6; 7]) [2; 4; 6] ;
test (List.find (fun x -> x mod 2 = 0) [1; 2; 3; 4; 5]) 2

module HashMap = struct
  type ('a, 'b) t = ('a * 'b) list

  (* TODO: this 'rec' is needed due to missing implementation *)
  let rec empty = []

  let add k v m = (k, v) :: m

  let rec find k = function
    | (k', v') :: xs -> if k = k' then v' else find k xs
    | [] -> raise Not_found

  let mem k m =
    try
      ignore (find k m) ;
      true
    with Not_found -> false

  let merge f m1 m2 =
    let src = ref empty in
    let rec iter_m1 = function
      | (k, v) :: xs ->
          ( try src := add k (Some v, Some (find k m2)) !src
            with Not_found -> src := add k (Some v, None) !src ) ;
          iter_m1 xs
      | [] -> ()
    in
    let rec iter_m2 = function
      | (k, v) :: xs ->
          if not (mem k m1) then src := add k (None, Some v) !src ;
          iter_m2 xs
      | [] -> ()
    in
    iter_m1 m1 ;
    iter_m2 m2 ;
    fold_left
      (fun m (k, (l, r)) ->
        match f k l r with None -> m | Some v -> add k v m )
      empty !src

  let union f m1 m2 =
    merge
      (fun k l r ->
        match (l, r) with
        | None, None -> None
        | Some v, None -> l
        | None, Some v -> r
        | Some v1, Some v2 -> f k v1 v2 )
      m1 m2

  let cardinal m = length m
end

;;
let m = HashMap.empty in
(* Thanks to: https://github.com/stereobooster/programming-languages-genealogical-tree *)
let m = HashMap.add "ML" 1977 m in
let m = HashMap.add "SML" 1984 m in
let m = HashMap.add "Caml" 1987 m in
let m = HashMap.add "OCaml" 1996 m in
test (HashMap.find "SML" m) 1984 ;
test (HashMap.find "OCaml" m) 1996 ;
test (try HashMap.find "C" m with Not_found -> -1) (-1) ;
test (try HashMap.find "ML" m with Not_found -> -1) 1977 ;
test (HashMap.mem "ML" m) true ;
test (HashMap.mem "C" m) false ;
let m' = HashMap.empty in
let m' = HashMap.add "ANSI C" 1989 m' in
let m' = HashMap.add "SML" 1990 m' in
let m' = HashMap.add "Caml" 1987 m' in
let m = HashMap.union (fun k l r -> Some r) m m' in
test (HashMap.find "SML" m) 1990 ;
test (HashMap.find "OCaml" m) 1996 ;
test (HashMap.find "Caml" m) 1987 ;
test (HashMap.find "ANSI C" m) 1989

module Hashtbl = struct
  type ('a, 'b) t = ('a, 'b) HashMap.t ref

  let create size_hint = ref HashMap.empty

  let add tbl k v = tbl := HashMap.add k v !tbl

  let mem tbl k = HashMap.mem k !tbl

  let find tbl k = HashMap.find k !tbl

  let length tbl = HashMap.cardinal !tbl
end

;;
let m = Hashtbl.create 16 in
Hashtbl.add m "ML" 1977 ;
Hashtbl.add m "SML" 1984 ;
Hashtbl.add m "Caml" 1987 ;
Hashtbl.add m "OCaml" 1996 ;
test (Hashtbl.find m "SML") 1984 ;
test (Hashtbl.find m "OCaml") 1996 ;
test (try Hashtbl.find m "C" with Not_found -> -1) (-1) ;
test (try Hashtbl.find m "ML" with Not_found -> -1) 1977 ;
test (Hashtbl.mem m "ML") true ;
test (Hashtbl.mem m "C") false ;
test (Hashtbl.length m) 4

let filter_after_map f lst =
  List.map (function Some x -> x | None -> failwith "invalid op")
  @@ List.filter (function Some x -> true | None -> false)
  @@ List.map f lst

;;
test
  (filter_after_map
     (function 10 -> Some "ten" | 20 -> Some "twenty" | _ -> None)
     [5; 3; 10; 20; 10])
  ["ten"; "twenty"; "ten"]

let rec list_unique lst =
  let set = Hashtbl.create @@ List.length lst in
  let rec aux res = function
    | [] -> res
    | x :: xs ->
        if Hashtbl.mem set x then aux res xs
        else (
          Hashtbl.add set x () ;
          aux (x :: res) xs )
  in
  aux [] lst

;;
test (list_unique [52; 36; 2; 0; 10; 36; 0; 52; 10; 72]) [72; 10; 0; 2; 36; 52]

let is_lower = function 'a' .. 'z' -> true | _ -> false

;;
test (is_lower 'd') true ;
test (is_lower 'e') true ;
test (is_lower 'Q') false ;
test (is_lower '\n') false

let append_to_list_ref x xs = xs := x :: !xs

;;
let lst = ref [] in
append_to_list_ref 10 lst ;
append_to_list_ref 3 lst ;
test !lst [3; 10]

let hashmap_of_list src =
  let hashmap = ref HashMap.empty in
  List.iter (fun (k, v) -> hashmap := HashMap.add k v !hashmap) src ;
  !hashmap

;;
let m = hashmap_of_list [("key1", "value1"); ("key2", "value2")] in
test (HashMap.find "key1" m) "value1" ;
test (HashMap.find "key2" m) "value2"

let integrate od nw = HashMap.union (fun _ _ r -> Some r) od nw

;;
let m1 = hashmap_of_list [("key1", "value1"); ("key2", "value2")] in
let m2 = hashmap_of_list [("key1", "value1"); ("key2", "value3")] in
let m = integrate m1 m2 in
test (HashMap.find "key1" m) "value1" ;
test (HashMap.find "key2" m) "value3"

let digit x =
  match x with
  | '0' .. '9' -> int_of_char x - int_of_char '0'
  | _ -> failwith "unexpected char: not digit"

;;
test (digit '5') 5 ;
test (digit '7') 7 ;
test (try digit '\n' with Failure _ -> -1) (-1)

type test_for_func_type = Value of int | Func of (int -> int)

;;
let str = Bytes.of_string "debug" in
test str @@ Bytes.of_string "debug" ;
str.[2] <- 'a' ;
test str @@ Bytes.of_string "deaug"

;;
let src = "abcd" in
let dst = Bytes.of_string "def  " in
String.blit src 1 dst 2 3 ;
test src "abcd" ;
test dst @@ Bytes.of_string "debcd"

;;
let src = "abcd" in
let dst = Bytes.create 5 in
dst.[0] <- 'd' ;
dst.[1] <- 'e' ;
String.blit src 1 dst 2 3 ;
test dst @@ Bytes.of_string "debcd"

type testrecord2 =
  {mutable testrecord2_int: int; mutable testrecord2_str: string}

;;
let r = {testrecord2_int= 12; testrecord2_str= "abc"} in
test r.testrecord2_str "abc" ;
r.testrecord2_str <- "def" ;
test r.testrecord2_str "def" ;
r.testrecord2_int <- 35 ;
test r.testrecord2_int 35

;;
let str = "abcdefghi" in
test (String.sub str 2 3) "cde"

;;
let buf = Buffer.create 3 in
test (Buffer.contents buf) "" ;
Buffer.add_char buf 'a' ;
test (Buffer.contents buf) "a" ;
Buffer.add_char buf 'b' ;
Buffer.add_char buf 'c' ;
Buffer.add_char buf 'd' ;
test (Buffer.contents buf) "abcd" ;
Buffer.add_string buf "efghi" ;
test (Buffer.contents buf) "abcdefghi" ;
Buffer.add_string buf "jk" ;
Buffer.add_string buf "lm" ;
test (Buffer.contents buf) "abcdefghijklm"

;;
test (String.concat ";" ["abc"; "def"; "ghi"]) "abc;def;ghi" ;
test (String.concat "." ["abc"; "def"; "ghi"]) "abc.def.ghi" ;
test (String.concat "." ["abc"]) "abc" ;
test (String.concat "." []) "" ;
let string_of_list src = "[" ^ String.concat "; " src ^ "]" in
test (string_of_list ["a"; "b"; "c"]) "[a; b; c]"

;;
let escape_string str =
  let buf = Buffer.create (String.length str) in
  let rec aux i =
    if i < String.length str then (
      ( match str.[i] with
      | '\n' -> Buffer.add_string buf "\\n"
      | '\t' -> Buffer.add_string buf "\\t"
      | '\\' -> Buffer.add_string buf "\\\\"
      | '"' -> Buffer.add_string buf "\\\""
      | ch -> Buffer.add_char buf ch ) ;
      aux (i + 1) )
  in
  aux 0 ; Buffer.contents buf
in
test (escape_string "\n\t\\\"") "\\n\\t\\\\\\\""

;;
test (string_of_int 20) "20" ;
test (string_of_int 0) "0" ;
test (string_of_int (-12)) "-12"

;;
test (String.make 3 'b') "bbb"

;;
test (Printf.sprintf "abc") "abc" ;
test (Printf.sprintf "abc%d" 1) "abc1" ;
test (Printf.sprintf "abc%d%d" 1 2) "abc12" ;
test (Printf.sprintf "abc%d%d%d" 1 2 3) "abc123" ;
test (Printf.sprintf "abc%d%d%d%d" 1 2 3 4) "abc1234" ;
test (Printf.sprintf "abc%d%d%d%d%d" 1 2 3 4 5) "abc12345" ;
test (Printf.sprintf "abc%d%d%d%d%d" 1 (-2) 3 4 5) "abc1-2345" ;
test (Printf.sprintf "abc%s" "def") "abcdef" ;
test (Printf.sprintf "abc%s%s" "def" "ghi") "abcdefghi" ;
test (Printf.sprintf "abc%s%d%s" "def" 10 "ghi") "abcdef10ghi" ;
test (Printf.sprintf "abc%c" 'd') "abcd" ;
test (Printf.sprintf "abc%c%s%d%c" 'd' "efg" 10 'i') "abcdefg10i" ;
test (Printf.ksprintf (fun str -> String.length str) "%s%s%d" "abc" "cde" 5) 7 ;
test (Printf.ksprintf (fun str -> String.length str) "abc") 3

open Printf

;;
test (sprintf "abc") "abc" ;
test (sprintf "abc%d" 1) "abc1" ;
test (sprintf "abc%d%d" 1 2) "abc12" ;
test (sprintf "abc%d%d%d" 1 2 3) "abc123" ;
test (sprintf "abc%d%d%d%d" 1 2 3 4) "abc1234" ;
test (sprintf "abc%d%d%d%d%d" 1 2 3 4 5) "abc12345" ;
test (sprintf "abc%d%d%d%d%d" 1 (-2) 3 4 5) "abc1-2345" ;
test (sprintf "abc%s" "def") "abcdef" ;
test (sprintf "abc%s%s" "def" "ghi") "abcdefghi" ;
test (sprintf "abc%s%d%s" "def" 10 "ghi") "abcdef10ghi" ;
test (sprintf "abc%c" 'd') "abcd" ;
test (sprintf "abc%c%s%d%c" 'd' "efg" 10 'i') "abcdefg10i" ;
test (ksprintf (fun str -> String.length str) "%s%s%d" "abc" "cde" 5) 7 ;
test (ksprintf (fun str -> String.length str) "abc") 3

;;
test (false && false) false ;
test (false && true) false ;
test (true && false) false ;
test (true && true) true ;
test (false || false) false ;
test (false || true) true ;
test (true || false) true ;
test (true || true) true

;;
let r = ref 10 in
let f () = r := !r + 1 in
false && (f () ; true) ;
test !r 10 ;
true || (f () ; true) ;
test !r 10 ;
true && (f () ; true) ;
test !r 11 ;
false || (f () ; true) ;
test !r 12

;;
let s = "debugdebugdebugdebugdebugdebugdebugdebugdebug" in
test
  "debugdebugdebugdebugdebugdebugdebugdebugdebug \
   debugdebugdebugdebugdebugdebugdebugdebugdebug"
  (s ^ " " ^ s)

;;
let testrecord1_int = 3 in
test
  {testrecord1_int; testrecord1_str= "abc"}
  {testrecord1_int= 3; testrecord1_str= "abc"} ;
test
  {testrecord1_str= "abc"; testrecord1_int}
  {testrecord1_int= 3; testrecord1_str= "abc"} ;
let testrecord1_str = "abc" in
test
  {testrecord1_str; testrecord1_int}
  {testrecord1_int= 3; testrecord1_str= "abc"}

;;
test (0 land 0) 0 ;
test (1 land 0) 0 ;
test (0 land 1) 0 ;
test (1 land 1) 1 ;
test (31 land 2) 2 ;
test (0 lor 0) 0 ;
test (1 lor 0) 1 ;
test (0 lor 1) 1 ;
test (1 lor 1) 1 ;
test (31 lor 2) 31

;;
test (max 1 2) 2 ;
test (max 5 3) 5 ;
test (max 4 4) 4

let is_capital = function 'A' .. 'Z' -> true | _ -> false

let id_counter = ref 0

let make_id base =
  id_counter := !id_counter + 1 ;
  sprintf "%s.%d" base !id_counter

let make_label () = make_id ".L"

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
  | Bar
  | Fun
  | Function
  | As
  | When
  | Type
  | Dot
  | DotDot
  | Of
  | KwInt
  | KwChar
  | KwUnit
  | KwBool
  | KwString
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
  | BarBar
  | AndAnd

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
  | Bar -> "|"
  | Fun -> "fun"
  | Function -> "function"
  | As -> "as"
  | When -> "when"
  | Type -> "type"
  | Dot -> "."
  | DotDot -> ".."
  | Of -> "of"
  | KwInt -> "int"
  | KwChar -> "char"
  | KwUnit -> "unit"
  | KwBool -> "bool"
  | KwString -> "string"
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
  | BarBar -> "||"
  | AndAnd -> "&&"

let raise_unexpected_token = function
  | x :: _ ->
      raise @@ failwith @@ sprintf "Unexpected token: %s" @@ string_of_token x
  | [] -> failwith "Unexpected EOF"

exception EOF

let tokenize program =
  let rec aux i =
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
      try
        let i, ch = next_char i in
        match ch with
        | '0' .. '9' -> next_int i ((acc * 10) + digit ch)
        | _ -> (i - 1, acc)
      with EOF -> (i, acc)
    in
    let next_ident i =
      let buf = Buffer.create 5 in
      let rec aux i =
        try
          let i, ch = next_char i in
          match ch with
          | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '\'' | '_' ->
              Buffer.add_char buf ch ; aux i
          | _ -> (i - 1, Buffer.contents buf)
        with EOF -> (i, Buffer.contents buf)
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
    try
      let i, ch = next_char i in
      match ch with
      | ' ' | '\t' | '\n' | '\r' -> aux i
      | '0' .. '9' ->
          let i, num = next_int (i - 1) 0 in
          IntLiteral num :: aux i
      | '\'' -> (
          let _, ch0 = next_char i in
          let _, ch1 = next_char (i + 1) in
          match (ch0, ch1) with
          | _, '\'' | '\\', _ ->
              let i, ch = next_char_literal i in
              CharLiteral ch :: aux i
          | _ -> Apostrophe :: aux i )
      | '"' ->
          let i, str = next_string_literal i in
          StringLiteral (make_id "string", str) :: aux i
      | 'a' .. 'z' | 'A' .. 'Z' | '_' -> (
          let i, str = next_ident (i - 1) in
          match str with
          | "let" -> Let :: aux i
          | "in" -> In :: aux i
          | "rec" -> Rec :: aux i
          | "true" -> IntLiteral 1 :: aux i (* TODO: boolean type *)
          | "false" -> IntLiteral 0 :: aux i
          | "if" -> If :: aux i
          | "then" -> Then :: aux i
          | "else" -> Else :: aux i
          | "match" -> Match :: aux i
          | "with" -> With :: aux i
          | "fun" -> Fun :: aux i
          | "function" -> Function :: aux i
          | "as" -> As :: aux i
          | "when" -> When :: aux i
          | "type" -> Type :: aux i
          | "of" -> Of :: aux i
          | "int" -> KwInt :: aux i
          | "char" -> KwChar :: aux i
          | "unit" -> KwUnit :: aux i
          | "bool" -> KwBool :: aux i
          | "string" -> KwString :: aux i
          | "and" -> And :: aux i
          | "try" -> Try :: aux i
          | "exception" -> Exception :: aux i
          | "mod" -> Mod :: aux i
          | "lsl" -> Lsl :: aux i
          | "lsr" -> Lsr :: aux i
          | "asr" -> Asr :: aux i
          | "module" -> Module :: aux i
          | "struct" -> Struct :: aux i
          | "end" -> End :: aux i
          | "external" -> External :: aux i
          | "mutable" -> Mutable :: aux i
          | "open" -> Open :: aux i
          | _ when is_capital str.[0] ->
              let rec aux' i cap acc =
                let i, ch = maybe_next_char i in
                match ch with
                | Some '.' ->
                    let i, str = next_ident i in
                    aux' i (is_capital str.[0]) (str :: acc)
                | _ -> (
                    let str = String.concat "." @@ List.rev acc in
                    ( i - 1
                    , match (cap, List.length acc > 1) with
                      | false, false -> LowerIdent str
                      | false, true -> LowerIdentWithModule str
                      | true, false -> CapitalIdent str
                      | true, true -> CapitalIdentWithModule str ) )
              in
              let i, tk = aux' i true [str] in
              tk :: aux i
          | _ -> LowerIdent str :: aux i )
      | '+' -> Plus :: aux i
      | '*' -> Star :: aux i
      | '/' -> Slash :: aux i
      | ')' -> RParen :: aux i
      | '>' -> GT :: aux i
      | '=' -> Equal :: aux i
      | ',' -> Comma :: aux i
      | ']' -> RBracket :: aux i
      | '^' -> Hat :: aux i
      | '!' -> Exclam :: aux i
      | '{' -> LBrace :: aux i
      | '}' -> RBrace :: aux i
      | '|' -> (
          let i, ch = next_char i in
          match ch with '|' -> BarBar :: aux i | _ -> Bar :: aux (i - 1) )
      | '&' -> (
          let i, ch = next_char i in
          match ch with
          | '&' -> AndAnd :: aux i
          | _ -> failwith "unexpected char" )
      | '@' -> (
          let i, ch = next_char i in
          match ch with
          | '@' -> NarutoNaruto :: aux i
          | _ -> Naruto :: aux (i - 1) )
      | '.' -> (
          let i, ch = next_char i in
          match ch with
          | '.' -> DotDot :: aux i
          | '[' -> DotLBracket :: aux i
          | _ -> Dot :: aux (i - 1) )
      | '-' -> (
          let i, ch = next_char i in
          match ch with '>' -> Arrow :: aux i | _ -> Minus :: aux (i - 1) )
      | '<' -> (
          let i, ch = next_char i in
          match ch with
          | '>' -> LTGT :: aux i
          | '-' -> LArrow :: aux i
          | _ -> LT :: aux (i - 1) )
      | '[' -> (
          let i, ch = next_char i in
          match ch with
          | ']' -> LRBracket :: aux i
          | _ -> LBracket :: aux (i - 1) )
      | ':' -> (
          let i, ch = next_char i in
          match ch with
          | ':' -> ColonColon :: aux i
          | '=' -> ColonEqual :: aux i
          | _ -> Colon :: aux (i - 1) )
      | ';' -> (
          let i, ch = next_char i in
          match ch with
          | ';' -> SemicolonSemicolon :: aux i
          | _ -> Semicolon :: aux (i - 1) )
      | '(' -> (
          let i, ch = next_char i in
          match ch with
          | '*' ->
              let i = skip_comment i in
              aux i
          | ')' -> LRParen :: aux i
          | _ -> LParen :: aux (i - 1) )
      | _ -> failwith (sprintf "unexpected char: '%c'" ch)
    with EOF -> []
  in
  aux 0

type typ =
  | TyInt
  | TyChar
  | TyUnit
  | TyBool
  | TyString
  | TyTuple of typ list
  | TyCustom of string
  | TyVar of string
  | TyCtorApp of typ * string
  | TyArgs of typ list
  | TyFunc of typ * typ

type ast =
  | UnitValue
  | IntValue of int
  | CharValue of char
  | StringValue of string * string
  | TupleValue of ast list
  | RecordValue of string option * (string * ast) list
  | RecordValueWith of ast * (string * ast) list
  | RecordDotAccess of string option * ast * string
  | Add of ast * ast
  | Sub of ast * ast
  | Mul of ast * ast
  | Div of ast * ast
  | Rem of ast * ast
  | LogicalLeftShift of ast * ast
  | LogicalRightShift of ast * ast
  | ArithmeticRightShift of ast * ast
  | LogicalAnd of ast * ast
  | LogicalOr of ast * ast
  | StringConcat of ast * ast
  | ListConcat of ast * ast
  | Negate of ast
  | Positate of ast
  | StructEqual of (ast * ast)
  | StructInequal of (ast * ast)
  | LessThan of (ast * ast)
  | LessThanEqual of (ast * ast)
  | IfThenElse of (ast * ast * ast option)
  | Var of string
  | FuncVar of string * int
  | AppCls of (ast * ast list)
  | AppDir of (string * ast list)
  | LetAnd of (bool * (pattern list * ast) list * ast option)
  | LetVar of (bool * pattern * ast)
  (* recursive?, funcname, args, function body, free variables *)
  | LetFunc of (bool * string * pattern list * ast * string list)
  | LetAndAnalyzed of ast list * ast
  | Cons of (ast * ast)
  | EmptyList
  | ExprSeq of ast list
  | MatchWith of ast * (pattern * ast option * ast) list
  | MakeCls of string * int * string list
  | Lambda of pattern list * ast
  | TypeAnd of typedef list
  | CtorApp of string option * string * ast option
  | RefAssign of ast * ast
  | RecordAssign of string option * ast * string * ast
  | Deref of ast
  | ExpDef of string * typ option
  | TryWith of ast * (pattern * ast option * ast) list
  | StringGet of ast * ast
  | StringSet of ast * ast * ast
  | Nope
  | ModuleDef of string * ast list
  (* for analysis *)
  | ModuleDefEnd
  | ExternalDecl of string * typ * string
  | OpenModuleDef of string
  (* TODO: module Ptn *)
  | PtnOr of pattern * pattern
  | PtnAlias of pattern * ast
  | PtnRange of char * char

and pattern = ast

and typedef =
  | DefVariant of typ option * string * (string * typ option) list
  | DefTypeAlias of typ option * string * typ
  | DefRecord of string * (string * typ) list

exception Unexpected_ast

let rec varnames_in_pattern = function
  (* TODO: much faster algorithm? *)
  | UnitValue | IntValue _ | CharValue _ | StringValue _ | EmptyList
   |PtnRange _ ->
      []
  | Var varname -> [varname]
  | Cons (car, cdr) ->
      List.rev_append (varnames_in_pattern car) (varnames_in_pattern cdr)
  | TupleValue values ->
      List.fold_left
        (fun a b -> List.rev_append a (varnames_in_pattern b))
        [] values
  | CtorApp (_, _, None) -> []
  | CtorApp (_, _, Some arg) -> varnames_in_pattern arg
  | PtnOr (lhs, rhs) ->
      List.rev_append (varnames_in_pattern lhs) (varnames_in_pattern rhs)
  | PtnAlias (ptn, Var name) -> name :: varnames_in_pattern ptn
  | _ -> raise Unexpected_ast

let parse tokens =
  let is_primary = function
    | ( IntLiteral _ | CharLiteral _ | StringLiteral _ | LowerIdent _
      | LowerIdentWithModule _ | CapitalIdent _ | CapitalIdentWithModule _
      | LRBracket | NarutoNaruto | LParen | LBracket | LRParen | LBrace )
      :: _ ->
        true
    | _ -> false
  in
  let is_dot = function (Dot | DotLBracket) :: _ -> true | _ -> false in
  let is_prefix = function Exclam :: _ -> true | _ -> false in
  let is_let = function
    | (Function | Fun | Match | Try | Let) :: _ -> true
    | _ -> false
  in
  let is_if = function If :: _ -> true | _ -> false in
  let rec parse_primary = function
    | IntLiteral num :: tokens -> (tokens, IntValue num)
    | CharLiteral ch :: tokens -> (tokens, CharValue ch)
    | StringLiteral (id, str) :: tokens -> (tokens, StringValue (id, str))
    | LRParen :: tokens -> (tokens, UnitValue)
    | (LowerIdentWithModule varname | LowerIdent varname) :: tokens ->
        (tokens, Var varname)
    | (CapitalIdentWithModule ctorname | CapitalIdent ctorname) :: tokens ->
        (tokens, CtorApp (None, ctorname, None))
    | LRBracket :: tokens -> (tokens, EmptyList)
    | NarutoNaruto :: tokens -> parse_let tokens
    | LParen :: tokens -> (
        let tokens, ast = parse_expression tokens in
        match tokens with
        | RParen :: tokens -> (tokens, ast)
        | x -> raise_unexpected_token x )
    | LBracket :: tokens ->
        let rec aux = function
          | Semicolon :: tokens ->
              let tokens, car = parse_let tokens in
              let tokens, cdr = aux tokens in
              (tokens, Cons (car, cdr))
          | RBracket :: tokens -> (tokens, EmptyList)
          | x -> raise_unexpected_token x
        in
        let tokens, car = parse_let tokens in
        let tokens, cdr = aux tokens in
        (tokens, Cons (car, cdr))
    | LBrace :: tokens -> (
        let rec parse_record_fields first fields tokens =
          let aux fieldname = function
            | Equal :: tokens -> parse_let tokens
            | (Semicolon | RBrace) :: _ as tokens -> (tokens, Var fieldname)
            | x -> raise_unexpected_token x
          in
          match tokens with
          | (LowerIdent fieldname | LowerIdentWithModule fieldname) :: tokens
            when first ->
              let tokens, ast = aux fieldname tokens in
              parse_record_fields false ((fieldname, ast) :: fields) tokens
          | Semicolon
            :: (LowerIdent fieldname | LowerIdentWithModule fieldname)
               :: tokens
            when not first ->
              let tokens, ast = aux fieldname tokens in
              parse_record_fields false ((fieldname, ast) :: fields) tokens
          | RBrace :: tokens -> (tokens, fields)
          | x -> raise_unexpected_token x
        in
        match tokens with
        | (LowerIdent _ | LowerIdentWithModule _) :: (Equal | Semicolon) :: _
          ->
            let tokens, fields = parse_record_fields true [] tokens in
            (tokens, RecordValue (None, fields))
        | _ -> (
            let tokens, base = parse_prefix tokens in
            match tokens with
            | With :: tokens ->
                let tokens, fields = parse_record_fields true [] tokens in
                (tokens, RecordValueWith (base, fields))
            | x -> raise_unexpected_token x ) )
    | x -> raise_unexpected_token x
  and parse_prefix = function
    | Exclam :: tokens ->
        let tokens, ast = parse_primary tokens in
        (tokens, Deref ast)
    | tokens -> parse_primary tokens
  and parse_dot tokens =
    let tokens, lhs = parse_prefix tokens in
    match tokens with
    | Dot :: LowerIdent fieldname :: tokens ->
        (tokens, RecordDotAccess (None, lhs, fieldname))
    | DotLBracket :: tokens -> (
        let tokens, rhs = parse_expression tokens in
        match tokens with
        | RBracket :: tokens -> (tokens, StringGet (lhs, rhs))
        | x -> raise_unexpected_token x )
    | _ -> (tokens, lhs)
  and parse_funccall tokens =
    let rec aux tokens =
      if is_primary tokens || is_dot tokens || is_prefix tokens then
        let tokens, arg = parse_dot tokens in
        let tokens, args = aux tokens in
        (tokens, arg :: args)
      else (tokens, [])
    in
    let tokens, func = parse_dot tokens in
    let tokens, args = aux tokens in
    if args = [] then (tokens, func) (* not function call *)
    else (tokens, AppCls (func, args))
  and parse_unary = function
    | Minus :: tokens ->
        let tokens, ast = parse_unary tokens in
        (tokens, Negate ast)
    | Plus :: tokens ->
        let tokens, ast = parse_unary tokens in
        (tokens, Positate ast)
    | tokens -> parse_funccall tokens
  and parse_shift tokens =
    let rec aux lhs = function
      | Lsl :: tokens ->
          let tokens, rhs = parse_unary tokens in
          aux (LogicalLeftShift (lhs, rhs)) tokens
      | Lsr :: tokens ->
          let tokens, rhs = parse_unary tokens in
          aux (LogicalRightShift (lhs, rhs)) tokens
      | Asr :: tokens ->
          let tokens, rhs = parse_unary tokens in
          aux (ArithmeticRightShift (lhs, rhs)) tokens
      | tokens -> (tokens, lhs)
    in
    let tokens, lhs = parse_unary tokens in
    aux lhs tokens
  and parse_multiplicative tokens =
    let rec aux lhs tokens =
      match tokens with
      | Star :: tokens ->
          let tokens, rhs = parse_shift tokens in
          aux (Mul (lhs, rhs)) tokens
      | Slash :: tokens ->
          let tokens, rhs = parse_shift tokens in
          aux (Div (lhs, rhs)) tokens
      | Mod :: tokens ->
          let tokens, rhs = parse_shift tokens in
          aux (Rem (lhs, rhs)) tokens
      | _ -> (tokens, lhs)
    in
    let tokens, ast = parse_shift tokens in
    aux ast tokens
  and parse_additive tokens =
    let rec aux lhs tokens =
      match tokens with
      | Plus :: tokens ->
          let tokens, rhs = parse_multiplicative tokens in
          aux (Add (lhs, rhs)) tokens
      | Minus :: tokens ->
          let tokens, rhs = parse_multiplicative tokens in
          aux (Sub (lhs, rhs)) tokens
      | _ -> (tokens, lhs)
    in
    let tokens, ast = parse_multiplicative tokens in
    aux ast tokens
  and parse_cons tokens =
    let tokens, car = parse_additive tokens in
    match tokens with
    | ColonColon :: tokens ->
        let tokens, cdr = parse_cons tokens in
        (tokens, Cons (car, cdr))
    | _ -> (tokens, car)
  and parse_string_concat tokens =
    let rec aux lhs tokens =
      match tokens with
      | Hat :: tokens ->
          let tokens, rhs = parse_cons tokens in
          aux (StringConcat (lhs, rhs)) tokens
      | Naruto :: tokens ->
          let tokens, rhs = parse_cons tokens in
          aux (ListConcat (lhs, rhs)) tokens
      | _ -> (tokens, lhs)
    in
    let tokens, ast = parse_cons tokens in
    aux ast tokens
  and parse_structural_equal tokens =
    let rec aux lhs tokens =
      match tokens with
      | Equal :: tokens ->
          let tokens, rhs = parse_string_concat tokens in
          aux (StructEqual (lhs, rhs)) tokens
      | LTGT :: tokens ->
          let tokens, rhs = parse_string_concat tokens in
          aux (StructInequal (lhs, rhs)) tokens
      | LT :: Equal :: tokens ->
          let tokens, rhs = parse_string_concat tokens in
          aux (LessThanEqual (lhs, rhs)) tokens
      | GT :: Equal :: tokens ->
          let tokens, rhs = parse_string_concat tokens in
          aux (LessThanEqual (rhs, lhs)) tokens
      | LT :: tokens ->
          let tokens, rhs = parse_string_concat tokens in
          aux (LessThan (lhs, rhs)) tokens
      | GT :: tokens ->
          let tokens, rhs = parse_string_concat tokens in
          aux (LessThan (rhs, lhs)) tokens
      | _ -> (tokens, lhs)
    in
    let tokens, ast = parse_string_concat tokens in
    aux ast tokens
  and parse_logical_and tokens =
    let rec aux lhs = function
      | AndAnd :: tokens ->
          let tokens, rhs = parse_structural_equal tokens in
          aux (LogicalAnd (lhs, rhs)) tokens
      | tokens -> (tokens, lhs)
    in
    let tokens, ast = parse_structural_equal tokens in
    aux ast tokens
  and parse_logical_or tokens =
    let rec aux lhs = function
      | BarBar :: tokens ->
          let tokens, rhs = parse_logical_and tokens in
          aux (LogicalOr (lhs, rhs)) tokens
      | tokens -> (tokens, lhs)
    in
    let tokens, ast = parse_logical_and tokens in
    aux ast tokens
  and parse_tuple tokens =
    let rec aux lhs tokens =
      match tokens with
      | Comma :: tokens ->
          let tokens, rhs =
            if is_let tokens || is_if tokens then parse_let tokens
            else parse_logical_or tokens
          in
          aux (rhs :: lhs) tokens
      | _ -> (tokens, lhs)
    in
    let tokens, ast = parse_logical_or tokens in
    let tokens, ast_list = aux [ast] tokens in
    match ast_list with
    | [] -> raise_unexpected_token []
    | [ast] -> (tokens, ast)
    | asts -> (tokens, TupleValue (List.rev asts))
  and parse_assignment tokens =
    let tokens, lhs = parse_tuple tokens in
    match tokens with
    | ColonEqual :: tokens ->
        let tokens, rhs = parse_let tokens in
        (tokens, RefAssign (lhs, rhs))
    | LArrow :: tokens -> (
        let tokens, rhs = parse_let tokens in
        match lhs with
        | StringGet (str, idx) -> (tokens, StringSet (str, idx, rhs))
        | RecordDotAccess (None, lhs, fieldname) ->
            (tokens, RecordAssign (None, lhs, fieldname, rhs))
        | _ -> raise_unexpected_token tokens )
    | _ -> (tokens, lhs)
  and parse_if = function
    | If :: tokens -> (
        let tokens, cond = parse_expression tokens in
        match tokens with
        | Then :: tokens -> (
            let tokens, then_body = parse_let tokens in
            match tokens with
            | Else :: tokens ->
                let tokens, else_body = parse_let tokens in
                (tokens, IfThenElse (cond, then_body, Some else_body))
            | _ -> (tokens, IfThenElse (cond, then_body, None)) )
        | x -> raise_unexpected_token x )
    | tokens -> parse_assignment tokens
  and parse_pattern_match tokens =
    let rec aux first cases tokens =
      let aux' tokens =
        let tokens, ptn = parse_pattern tokens in
        let tokens, whn =
          match tokens with
          | When :: tokens ->
              let tokens, expr = parse_expression tokens in
              (tokens, Some expr)
          | _ -> (tokens, None)
        in
        match tokens with
        | Arrow :: tokens ->
            let tokens, case = parse_expression tokens in
            let tokens, cases = aux false ((ptn, whn, case) :: cases) tokens in
            (tokens, cases)
        | x -> raise_unexpected_token x
      in
      match tokens with
      | Bar :: tokens -> aux' tokens
      | _ -> if first then aux' tokens else (tokens, List.rev cases)
    in
    aux true [] tokens
  and parse_let = function
    | Function :: tokens ->
        let argname = ".arg" in
        let tokens, cases = parse_pattern_match tokens in
        (tokens, Lambda ([Var argname], MatchWith (Var argname, cases)))
    | Fun :: tokens ->
        let rec aux = function
          | Arrow :: tokens -> (tokens, [])
          | tokens ->
              let tokens, arg = parse_pattern tokens in
              let tokens, args = aux tokens in
              (tokens, arg :: args)
        in
        let tokens, args = aux tokens in
        let tokens, func = parse_expression tokens in
        (tokens, Lambda (args, func))
    | Match :: tokens -> (
        let tokens, cond = parse_expression tokens in
        match tokens with
        | With :: tokens ->
            let tokens, cases = parse_pattern_match tokens in
            (tokens, MatchWith (cond, cases))
        | x -> raise_unexpected_token x )
    | Try :: tokens -> (
        let tokens, cond = parse_expression tokens in
        match tokens with
        | With :: tokens ->
            let tokens, cases = parse_pattern_match tokens in
            (tokens, TryWith (cond, cases))
        | x -> raise_unexpected_token x )
    | Let :: tokens -> (
        let parse_let_binding tokens =
          let tokens, bind = parse_pattern tokens in
          match tokens with
          | Equal :: tokens ->
              (* define constants *)
              let tokens, lhs = parse_expression tokens in
              (tokens, ([bind], lhs))
          | _ ->
              (* define function *)
              let rec aux = function
                | Equal :: tokens -> (tokens, [])
                | tokens ->
                    let tokens, arg = parse_pattern tokens in
                    let tokens, args = aux tokens in
                    (tokens, arg :: args)
              in
              let tokens, args = aux tokens in
              let tokens, func = parse_expression tokens in
              (tokens, (bind :: args, func))
        in
        let tokens, recursive =
          match tokens with
          | Rec :: tokens -> (tokens, true)
          | _ -> (tokens, false)
        in
        let rec aux' lets = function
          | And :: tokens ->
              let tokens, le = parse_let_binding tokens in
              aux' (le :: lets) tokens
          | tokens -> (tokens, lets)
        in
        let tokens, le = parse_let_binding tokens in
        let tokens, lets = aux' [le] tokens in
        match tokens with
        | In :: tokens ->
            let tokens, rhs_of_in = parse_expression tokens in
            (tokens, LetAnd (recursive, lets, Some rhs_of_in))
        | _ -> (tokens, LetAnd (recursive, lets, None)) )
    | tokens -> parse_if tokens
  and parse_expr_sequence tokens =
    let rec aux = function
      | Semicolon :: tokens ->
          let tokens, expr = parse_let tokens in
          let tokens, exprs = aux tokens in
          (tokens, expr :: exprs)
      | tokens -> (tokens, [])
    in
    let tokens, expr = parse_let tokens in
    let tokens, exprs = aux tokens in
    if List.length exprs = 0 then (tokens, expr)
    else (tokens, ExprSeq (expr :: exprs))
  and parse_expression tokens = parse_expr_sequence tokens
  and parse_pattern_primary = function
    | IntLiteral num :: tokens -> (tokens, IntValue num)
    | CharLiteral ch :: tokens -> (tokens, CharValue ch)
    | StringLiteral (id, str) :: tokens -> (tokens, StringValue (id, str))
    | LRParen :: tokens -> (tokens, UnitValue)
    | (LowerIdent id | LowerIdentWithModule id) :: tokens -> (tokens, Var id)
    | (CapitalIdent id | CapitalIdentWithModule id) :: tokens ->
        (tokens, CtorApp (None, id, None))
    | LRBracket :: tokens -> (tokens, EmptyList)
    | LParen :: tokens -> (
        let tokens, ast = parse_pattern tokens in
        match tokens with
        | RParen :: tokens -> (tokens, ast)
        | x -> raise_unexpected_token x )
    | LBracket :: tokens ->
        let rec aux = function
          | Semicolon :: tokens ->
              let tokens, car = parse_pattern tokens in
              let tokens, cdr = aux tokens in
              (tokens, Cons (car, cdr))
          | RBracket :: tokens -> (tokens, EmptyList)
          | x -> raise_unexpected_token x
        in
        let tokens, car = parse_pattern tokens in
        let tokens, cdr = aux tokens in
        (tokens, Cons (car, cdr))
    | x -> raise_unexpected_token x
  and parse_pattern_range = function
    | CharLiteral st :: DotDot :: CharLiteral ed :: tokens ->
        (tokens, PtnRange (st, ed))
    | tokens -> parse_pattern_primary tokens
  and parse_pattern_ctor_app tokens =
    let tokens, ctorapp = parse_pattern_range tokens in
    match ctorapp with
    | CtorApp (None, ctorname, None) when is_primary tokens ->
        let tokens, arg = parse_pattern_range tokens in
        (tokens, CtorApp (None, ctorname, Some arg))
    | _ -> (tokens, ctorapp)
  and parse_pattern_cons tokens =
    let tokens, car = parse_pattern_ctor_app tokens in
    match tokens with
    | ColonColon :: tokens ->
        let tokens, cdr = parse_pattern_cons tokens in
        (tokens, Cons (car, cdr))
    | _ -> (tokens, car)
  and parse_pattern_tuple tokens =
    let rec aux lhs tokens =
      match tokens with
      | Comma :: tokens ->
          let tokens, rhs = parse_pattern_cons tokens in
          aux (rhs :: lhs) tokens
      | _ -> (tokens, lhs)
    in
    let tokens, ast = parse_pattern_cons tokens in
    let tokens, ast_list = aux [ast] tokens in
    match ast_list with
    | [] -> raise_unexpected_token []
    | [ast] -> (tokens, ast)
    | asts -> (tokens, TupleValue (List.rev asts))
  and parse_pattern_or tokens =
    let rec aux lhs = function
      | Bar :: tokens ->
          let tokens, rhs = parse_pattern_tuple tokens in
          aux (PtnOr (lhs, rhs)) tokens
      | tokens -> (tokens, lhs)
    in
    let tokens, lhs = parse_pattern_tuple tokens in
    aux lhs tokens
  and parse_pattern_as tokens =
    let tokens, ptn = parse_pattern_or tokens in
    match tokens with
    | As :: LowerIdent name :: tokens -> (tokens, PtnAlias (ptn, Var name))
    | _ -> (tokens, ptn)
  and parse_pattern tokens = parse_pattern_as tokens
  and parse_typexpr_primary = function
    | KwInt :: tokens -> (tokens, TyInt)
    | KwChar :: tokens -> (tokens, TyChar)
    | KwUnit :: tokens -> (tokens, TyUnit)
    | KwBool :: tokens -> (tokens, TyBool)
    | KwString :: tokens -> (tokens, TyString)
    | Apostrophe :: LowerIdent id :: tokens -> (tokens, TyVar id)
    | (LowerIdent typename | LowerIdentWithModule typename) :: tokens ->
        (tokens, TyCustom typename)
    | LParen :: _ ->
        failwith "Any token LParen should be handled in parse_typexpr_ctor_app"
    | x -> raise_unexpected_token x
  and parse_typexpr_ctor_app tokens =
    let tokens, lhs =
      match tokens with
      | LParen :: tokens ->
          let tokens, typexpr = parse_typexpr tokens in
          let rec aux types = function
            | Comma :: tokens ->
                let tokens, typexpr = parse_typexpr tokens in
                aux (typexpr :: types) tokens
            | RParen :: tokens -> (tokens, types)
            | x -> raise_unexpected_token x
          in
          let tokens, types = aux [typexpr] tokens in
          let types = List.rev types in
          ( tokens
          , if List.length types = 1 then List.hd types else TyArgs types )
      | _ -> parse_typexpr_primary tokens
    in
    let rec aux lhs = function
      | (LowerIdent typectorname | LowerIdentWithModule typectorname) :: tokens
        ->
          aux (TyCtorApp (lhs, typectorname)) tokens
      | tokens -> (tokens, lhs)
    in
    aux lhs tokens
  and parse_typexpr_tuple tokens =
    let rec aux lhs = function
      | Star :: tokens ->
          let tokens, rhs = parse_typexpr_ctor_app tokens in
          aux (rhs :: lhs) tokens
      | tokens -> (tokens, lhs)
    in
    let tokens, typexpr = parse_typexpr_ctor_app tokens in
    let tokens, typexprs = aux [typexpr] tokens in
    match typexprs with
    | [] -> raise_unexpected_token []
    | [typexpr] -> (tokens, typexpr)
    | typexprs -> (tokens, TyTuple typexprs)
  and parse_typexpr_func tokens =
    let tokens, lhs = parse_typexpr_tuple tokens in
    match tokens with
    | Arrow :: tokens ->
        let tokens, rhs = parse_typexpr_func tokens in
        (tokens, TyFunc (lhs, rhs))
    | _ -> (tokens, lhs)
  and parse_typexpr tokens = parse_typexpr_func tokens
  and parse_type_def tokens =
    let parse_type_param = function
      | Apostrophe :: LowerIdent id :: tokens -> (tokens, TyVar id)
      | x -> raise_unexpected_token x
    in
    let parse_type_params = function
      | LParen :: tokens ->
          let rec aux type_params = function
            | Comma :: tokens ->
                let tokens, type_param = parse_type_param tokens in
                aux (type_param :: type_params) tokens
            | RParen :: tokens -> (tokens, TyTuple type_params)
            | x -> raise_unexpected_token x
          in
          let tokens, type_param = parse_type_param tokens in
          Some (aux [type_param] tokens)
      | Apostrophe :: _ as tokens -> Some (parse_type_param tokens)
      | _ -> None
    in
    let parse_type_def_entry tokens =
      let tokens, type_param =
        match parse_type_params tokens with
        | None -> (tokens, None)
        | Some (tokens, type_params) -> (tokens, Some type_params)
      in
      match tokens with
      | LowerIdent typename :: Equal :: tokens -> (
          let parse_variant tokens =
            let rec aux first ctors = function
              | Bar :: CapitalIdent ctorname :: Of :: tokens ->
                  let tokens, typ = parse_typexpr tokens in
                  aux false ((ctorname, Some typ) :: ctors) tokens
              | CapitalIdent ctorname :: Of :: tokens when first ->
                  let tokens, typ = parse_typexpr tokens in
                  aux false ((ctorname, Some typ) :: ctors) tokens
              | Bar :: CapitalIdent ctorname :: tokens ->
                  aux false ((ctorname, None) :: ctors) tokens
              | CapitalIdent ctorname :: tokens when first ->
                  aux false ((ctorname, None) :: ctors) tokens
              | tokens -> (tokens, ctors)
            in
            let tokens, ctors = aux true [] tokens in
            (tokens, DefVariant (type_param, typename, ctors))
          in
          match tokens with
          | CapitalIdent str :: _ -> parse_variant tokens
          | Bar :: _ -> parse_variant tokens
          (* TODO: skip mutable *)
          | LBrace :: LowerIdent fieldname :: Colon :: tokens
           |LBrace :: Mutable :: LowerIdent fieldname :: Colon :: tokens ->
              let rec aux fields = function
                | Semicolon :: LowerIdent fieldname :: Colon :: tokens
                 |Semicolon
                  :: Mutable :: LowerIdent fieldname :: Colon :: tokens ->
                    let tokens, typexpr = parse_typexpr tokens in
                    aux ((fieldname, typexpr) :: fields) tokens
                | RBrace :: tokens -> (tokens, fields)
                | x -> raise_unexpected_token x
              in
              let tokens, typexpr = parse_typexpr tokens in
              let tokens, fields = aux [(fieldname, typexpr)] tokens in
              (tokens, DefRecord (typename, fields))
          | tokens ->
              let tokens, typ = parse_typexpr tokens in
              (tokens, DefTypeAlias (type_param, typename, typ)) )
      | x -> raise_unexpected_token x
    in
    let rec aux entries = function
      | And :: tokens ->
          let tokens, entry = parse_type_def_entry tokens in
          aux (entry :: entries) tokens
      | tokens -> (tokens, TypeAnd entries)
    in
    (* token Type is already fetched *)
    let tokens, entry = parse_type_def_entry tokens in
    aux [entry] tokens
  and parse_exp_def = function
    (* token Exception is already fetched *)
    | CapitalIdent expname :: Of :: tokens ->
        let tokens, typ = parse_typexpr tokens in
        (tokens, ExpDef (expname, Some typ))
    | CapitalIdent expname :: tokens -> (tokens, ExpDef (expname, None))
    | x -> raise_unexpected_token x
  in
  let parse_expressions_and_definitions tokens =
    (* Here are some tricks. All expressions split by double semicolons (;;)
     * are converted to (maybe large) one ExprSeq, and all 'let' without 'in'
     * come to have their following expressions as their 'in' part.
     * This change makes later processes such as semantic analysis easier. *)
    (* TODO: correct? *)
    (* TODO: not correct. definitions and expressions should be completely separated. *)
    let rec aux exprs = function
      | SemicolonSemicolon :: tokens -> aux exprs tokens
      | [] -> ([], List.rev exprs)
      | Type :: tokens ->
          let tokens, expr = parse_type_def tokens in
          aux (expr :: exprs) tokens
      | Exception :: tokens ->
          let tokens, expr = parse_exp_def tokens in
          aux (expr :: exprs) tokens
      | External :: LowerIdent id :: Colon :: tokens -> (
          let tokens, typexpr = parse_typexpr tokens in
          match tokens with
          | Equal :: StringLiteral (_, str) :: tokens ->
              let ast = ExternalDecl (id, typexpr, str) in
              aux (ast :: exprs) tokens
          | x -> raise_unexpected_token x )
      | Open :: CapitalIdent modname :: tokens
       |Open :: CapitalIdentWithModule modname :: tokens ->
          aux (OpenModuleDef modname :: exprs) tokens
      | Module :: CapitalIdent modulename :: Equal :: Struct :: tokens ->
          let tokens, asts = aux [] tokens in
          let ast = ModuleDef (modulename, asts) in
          aux (ast :: exprs) tokens
      | End :: tokens -> (* module end *)
                         (tokens, List.rev exprs)
      | tokens ->
          let tokens, expr = parse_expression tokens in
          aux (expr :: exprs) tokens
    in
    let _, exprs = aux [] tokens in
    exprs
  in
  parse_expressions_and_definitions tokens

;;
test
  (parse (tokenize "test ((4 / 2 * 3) + 1 - (10 / 2) + (4 * 2 * 1)) 10"))
  [ AppCls
      ( Var "test"
      , [ Add
            ( Sub
                ( Add
                    (Mul (Div (IntValue 4, IntValue 2), IntValue 3), IntValue 1)
                , Div (IntValue 10, IntValue 2) )
            , Mul (Mul (IntValue 4, IntValue 2), IntValue 1) )
        ; IntValue 10 ] ) ]

type environment =
  { symbols: (string, ast) HashMap.t
  ; parent: environment option
  ; freevars: (string * string) list ref }

let add_symbols_in_pattern symbols ptn =
  integrate symbols @@ hashmap_of_list
  @@ List.map (fun n -> (n, Var (make_id n)))
  @@ varnames_in_pattern ptn

let add_symbols_in_patterns symbols ptns =
  integrate symbols @@ hashmap_of_list
  @@ List.map (fun n -> (n, Var (make_id n)))
  @@ List.flatten
  @@ List.map varnames_in_pattern ptns

type type_toplevel =
  { letfuncs: ast list ref
  ; strings: ast list ref
  ; typedefs: typedef list ref
  ; exps_list: string list ref
  ; ctors_type: (string, string) Hashtbl.t
  ; exps: (string, string) Hashtbl.t
  ; records: (string, string) Hashtbl.t
  ; records_fields: (string, string list) Hashtbl.t
  ; modulename: string list ref
  ; (* TODO: opened_modulename should be in type environment
   * rather than type type_toplevel, because
   * functions, exceptions, types, and etc. in the opened module
   * mask previously defined ones with the same names.
   * For example, the current implementation doesn't allow the following code:
   *     module ABC = struct let f () = 5 end ;;
   *     let f () = 3 ;;
   *     open ABC;;
   *     test (f ()) 5 ;; (* expect 5 but will get 3 *)
   *)
    opened_modulename: string list ref }

(* Used in analysis of LetAnd *)
exception Should_be_closure

exception LetDef of ast list * environment

let analyze asts =
  let toplevel =
    { letfuncs= ref []
    ; strings= ref []
    ; typedefs= ref []
    ; exps_list= ref []
    ; ctors_type= Hashtbl.create 16
    ; exps= Hashtbl.create 16
    ; records= Hashtbl.create 16
    ; records_fields= Hashtbl.create 16
    ; modulename= ref []
    ; opened_modulename= ref [] }
  in
  let get_current_name_prefix () =
    let buf = Buffer.create 128 in
    List.iter (fun modname ->
        Buffer.add_string buf modname ;
        Buffer.add_char buf '.' )
    @@ List.rev @@ !(toplevel.modulename) ;
    Buffer.contents buf
  in
  let with_modulename name =
    String.concat "." @@ List.rev @@ (name :: !(toplevel.modulename))
  in
  let exprs2expr = function
    | [] -> Nope
    | [expr] -> expr
    | exprs -> ExprSeq exprs
  in
  let hashtbl_find_with_modulename hashtbl name =
    try ("", Hashtbl.find hashtbl name) with Not_found -> (
      try
        ( get_current_name_prefix ()
        , Hashtbl.find hashtbl (with_modulename name) )
      with Not_found ->
        let modname =
          List.find
            (fun modname -> Hashtbl.mem hashtbl (modname ^ name))
            !(toplevel.opened_modulename)
        in
        (modname, Hashtbl.find hashtbl (modname ^ name)) )
  in
  let hashmap_find_with_modulename name hashmap =
    try HashMap.find name hashmap with Not_found -> (
      try HashMap.find (with_modulename name) hashmap with Not_found ->
        let modname =
          List.find
            (fun modname -> HashMap.mem (modname ^ name) hashmap)
            !(toplevel.opened_modulename)
        in
        HashMap.find (modname ^ name) hashmap )
  in
  let find_symbol env name =
    let rec aux depth env =
      try (depth, hashmap_find_with_modulename name env.symbols)
      with Not_found -> (
        match env.parent with
        | Some parent -> aux (depth + 1) parent
        | None ->
            failwith (sprintf "not found in analysis (find_symbol): %s" name) )
    in
    aux 0 env
  in
  let rec aux_ptn env ptn =
    match ptn with
    | IntValue _ | CharValue _ | UnitValue | EmptyList | PtnRange _ -> ptn
    | StringValue _ ->
        append_to_list_ref ptn toplevel.strings ;
        ptn
    | TupleValue values ->
        TupleValue (List.map (fun x -> aux_ptn env x) values)
    | Cons (car, cdr) -> Cons (aux_ptn env car, aux_ptn env cdr)
    | Var name -> (
      match find_symbol env name with
      | 0, sym -> sym
      | _ -> failwith "[FATAL] variable not found in pattern analysis" )
    | PtnAlias (ptn, (Var _ as var)) ->
        PtnAlias (aux_ptn env ptn, aux_ptn env var)
    | PtnOr (lhs, rhs) -> PtnOr (aux_ptn env lhs, aux_ptn env rhs)
    | CtorApp (None, ctorname, arg) ->
        let arg =
          match arg with Some arg -> Some (aux_ptn env arg) | _ -> None
        in
        CtorApp
          ( Some
              ( try Hashtbl.find toplevel.ctors_type ctorname
                with Not_found -> Hashtbl.find toplevel.exps ctorname )
          , ctorname
          , arg )
    | _ -> failwith "unexpected pattern"
  in
  let rec analyze_pattern_match_cases env cases =
    List.map
      (fun (ptn, whn, ast) ->
        let env' =
          {env with symbols= add_symbols_in_pattern env.symbols ptn}
        in
        ( aux_ptn env' ptn
        , (match whn with Some expr -> Some (aux env' expr) | None -> None)
        , aux env' ast ) )
      cases
  and aux env ast =
    match ast with
    | IntValue _ | CharValue _ | UnitValue | EmptyList -> ast
    | StringValue _ ->
        append_to_list_ref ast toplevel.strings ;
        ast
    | TupleValue values -> TupleValue (List.map (fun x -> aux env x) values)
    | RecordValue (None, fields) ->
        let key_fieldname, _ = List.hd fields in
        let name_prefix, typename =
          hashtbl_find_with_modulename toplevel.records key_fieldname
        in
        RecordValue
          ( Some typename
          , List.map
              (fun (name, ast) -> (name_prefix ^ name, aux env ast))
              fields )
    | RecordValueWith (base, fields) ->
        let key_fieldname, _ = List.hd fields in
        let typename = Hashtbl.find toplevel.records key_fieldname in
        let fieldnames = Hashtbl.find toplevel.records_fields typename in
        let fields = hashmap_of_list fields in
        let new_base = Var (make_id "var") in
        aux env
        @@ LetAnd
             ( false
             , [([new_base], base)]
             , Some
                 (RecordValue
                    ( None
                    , List.map
                        (fun fieldname ->
                          try (fieldname, HashMap.find fieldname fields)
                          with Not_found ->
                            ( fieldname
                            , RecordDotAccess (None, new_base, fieldname) ) )
                        fieldnames )) )
    | RecordDotAccess (None, ast, fieldname) ->
        let name_prefix, typename =
          hashtbl_find_with_modulename toplevel.records fieldname
        in
        RecordDotAccess (Some typename, aux env ast, name_prefix ^ fieldname)
    | Cons (car, cdr) -> Cons (aux env car, aux env cdr)
    | Add (lhs, rhs) -> Add (aux env lhs, aux env rhs)
    | Sub (lhs, rhs) -> Sub (aux env lhs, aux env rhs)
    | Mul (lhs, rhs) -> Mul (aux env lhs, aux env rhs)
    | Div (lhs, rhs) -> Div (aux env lhs, aux env rhs)
    | Rem (lhs, rhs) -> Rem (aux env lhs, aux env rhs)
    | LogicalLeftShift (lhs, rhs) -> LogicalLeftShift (lhs, rhs)
    | LogicalRightShift (lhs, rhs) -> LogicalRightShift (lhs, rhs)
    | ArithmeticRightShift (lhs, rhs) -> ArithmeticRightShift (lhs, rhs)
    | StringConcat (lhs, rhs) -> StringConcat (aux env lhs, aux env rhs)
    | ListConcat (lhs, rhs) -> ListConcat (aux env lhs, aux env rhs)
    | RefAssign (lhs, rhs) -> RefAssign (aux env lhs, aux env rhs)
    | RecordAssign (None, lhs, fieldname, rhs) ->
        let name_prefix, typename =
          hashtbl_find_with_modulename toplevel.records fieldname
        in
        RecordAssign
          (Some typename, aux env lhs, name_prefix ^ fieldname, aux env rhs)
    | Deref ast -> Deref (aux env ast)
    | Negate ast -> Negate (aux env ast)
    | Positate ast -> Positate (aux env ast)
    | StructEqual (lhs, rhs) -> StructEqual (aux env lhs, aux env rhs)
    | StructInequal (lhs, rhs) -> StructInequal (aux env lhs, aux env rhs)
    | LessThan (lhs, rhs) -> LessThan (aux env lhs, aux env rhs)
    | LessThanEqual (lhs, rhs) -> LessThanEqual (aux env lhs, aux env rhs)
    | LogicalAnd (lhs, rhs) -> LogicalAnd (aux env lhs, aux env rhs)
    | LogicalOr (lhs, rhs) -> LogicalOr (aux env lhs, aux env rhs)
    | IfThenElse (cond, then_body, Some else_body) ->
        IfThenElse (aux env cond, aux env then_body, Some (aux env else_body))
    | IfThenElse (cond, then_body, None) ->
        IfThenElse (aux env cond, aux env then_body, None)
    | ExprSeq exprs -> ExprSeq (List.map (fun x -> aux env x) exprs)
    | Lambda (args, body) ->
        let funcname = ".lambda" in
        aux env
        @@ LetAnd (false, [(Var funcname :: args, body)], Some (Var funcname))
    | StringGet (str, idx) ->
        (* a.[b] returns a b-th character of a string a.
         * Therefore, convert it to String.get call *)
        aux env @@ AppCls (Var "String.get", [str; idx])
    | StringSet (str, idx, ast) ->
        aux env @@ AppCls (Var "String.set", [str; idx; ast])
    | TryWith (cond, cases) ->
        TryWith (aux env cond, analyze_pattern_match_cases env cases)
    | MatchWith (cond, cases) ->
        MatchWith (aux env cond, analyze_pattern_match_cases env cases)
    | Var name -> (
      match find_symbol env name with
      | 0, (Var _ as sym) -> sym
      | _, FuncVar (gen_funcname, 0) -> AppDir (gen_funcname, [])
      | 0, FuncVar (funcname, nargs) ->
          (* When FuncVar is processed here, AppDir will not be applied to this FuncVar.
           * Therefore the returned value should be closured in case
           * AppCls is applied to this value. *)
          MakeCls (funcname, nargs, [])
      | _, (Var id as sym) ->
          env.freevars := (name, id) :: !(env.freevars) ;
          sym
      | _ -> failwith @@ sprintf "not found variable in analysis: %s" name )
    | CtorApp (None, ctorname, None) ->
        CtorApp
          ( Some
              ( try Hashtbl.find toplevel.ctors_type ctorname
                with Not_found -> Hashtbl.find toplevel.exps ctorname )
          , ctorname
          , None )
    | TypeAnd entries ->
        toplevel.typedefs :=
          List.rev_append !(toplevel.typedefs)
          @@ List.map
               (function
                 | DefTypeAlias (type_param, typename, typ) ->
                     let typename = with_modulename typename in
                     DefTypeAlias (type_param, typename, typ)
                 | DefVariant (type_param, typename, ctornames) ->
                     let typename = with_modulename typename in
                     let ctornames =
                       List.map
                         (fun (ctorname, typexpr) ->
                           (with_modulename ctorname, typexpr) )
                         ctornames
                     in
                     List.iter
                       (fun (ctorname, _) ->
                         Hashtbl.add toplevel.ctors_type ctorname typename )
                       ctornames ;
                     DefVariant (type_param, typename, ctornames)
                 | DefRecord (typename, fields) ->
                     let typename = with_modulename typename in
                     let fields =
                       List.map
                         (fun (fieldname, typexpr) ->
                           (with_modulename fieldname, typexpr) )
                         fields
                     in
                     List.iter
                       (fun (fieldname, _) ->
                         Hashtbl.add toplevel.records fieldname typename )
                       fields ;
                     Hashtbl.add toplevel.records_fields typename
                     @@ List.map (fun (fieldname, _) -> fieldname) fields ;
                     DefRecord (typename, fields))
               entries ;
        Nope
    | ExpDef (expname, components) ->
        Hashtbl.add toplevel.exps expname expname ;
        toplevel.exps_list := expname :: !(toplevel.exps_list) ;
        Nope
    | OpenModuleDef modname ->
        toplevel.opened_modulename :=
          (modname ^ ".") :: !(toplevel.opened_modulename) ;
        Nope
    | AppCls ((CtorApp (None, ctorname, None) as ctor), args) -> (
      match aux env ctor with
      | CtorApp (typename, ctorname, None) when List.length args = 1 ->
          CtorApp (typename, ctorname, Some (aux env @@ List.hd args))
      | _ -> failwith "invalid CtorApp" )
    | AppCls ((Var funcname as var), args) -> (
      try
        match
          match find_symbol env funcname with
          (* the symbol is 'safe' when it's in the same env
           * or it can be called by its name *)
          | 0, sym | _, (FuncVar _ as sym) -> sym
          | _, (Var id as sym) ->
              env.freevars := (funcname, id) :: !(env.freevars) ;
              sym
          | _ ->
              failwith @@ sprintf "not found variable in analysis: %s" funcname
        with
        | FuncVar (gen_funcname, nargs) ->
            let args = List.map (fun x -> aux env x) args in
            if List.length args = nargs then AppDir (gen_funcname, args)
            else
              let rec split n lst =
                if n = 0 then ([], lst)
                else
                  match lst with
                  | x :: xs ->
                      let lhs, rhs = split (n - 1) xs in
                      (x :: lhs, rhs)
                  | [] -> failwith "n > List.length lst"
              in
              let head, tail = split nargs args in
              AppCls (AppDir (gen_funcname, head), tail)
        | Var varname ->
            AppCls (aux env var, List.map (fun x -> aux env x) args)
        | _ -> raise Not_found
      with Not_found ->
        failwith (sprintf "not found in analysis (AppCls): %s" funcname) )
    | AppCls (func, args) ->
        AppCls (aux env func, List.map (fun x -> aux env x) args)
    | LetAnd (recursive, lhs_of_in, rhs_of_in) ->
        (* Split rhs_of_eq into LetVar and LetFunc. At the same time,
         * make a conversion table for function names *)
        let funcnames2gen = Hashtbl.create 2 in
        let src =
          List.map
            (function
              | [Var funcname], rhs_of_eq when recursive ->
                  (* When recursive, LetVar should be LetFunc with no arguments. *)
                  (* TODO:
                    If the lhs doesn't have any freevars, then there is no need to convert it.
                    Also, we should check whether the lhs uses itself in a correct way e.g.
                        let rec length = function x :: xs -> 1 + length xs | [] -> 0;;
                    is okay, but
                        let rec id x = id;;
                    is ng. For now, we assume that 'let rec ...' expression is written properly.
                  *)
                  let funcname =
                    match rhs_of_in with
                    | Some _ -> funcname
                    | None -> with_modulename funcname
                  in
                  Hashtbl.add funcnames2gen funcname (make_id funcname) ;
                  LetFunc (true, funcname, [], rhs_of_eq, [])
              | [bind], rhs_of_eq -> LetVar (recursive, bind, rhs_of_eq)
              | Var funcname :: args, rhs_of_eq ->
                  let funcname =
                    match rhs_of_in with
                    | Some _ -> funcname
                    | None -> with_modulename funcname
                  in
                  Hashtbl.add funcnames2gen funcname (make_id funcname) ;
                  LetFunc (recursive, funcname, args, rhs_of_eq, [])
              | _ -> failwith "unexpected ast")
            lhs_of_in
        in
        (* Now, analyze all LetVar/LetFunc.
         * When we analyze *recursive* LetFunc, we must decide whether
         * we should call this function by name or as closure in itself.
         * Therefore, first, we assume that we can call them by name i.e. we use FuncVar.
         * Next, if we find we can't do so (i.e. there are any freevars), we decide to call them as closure,
         * that is, use Var, and analyze it again.
         * I (ushitora-anqou) 'pakutta' or borrowed this idea from MinCaml.
         * TODO: is there better way?*)
        let let_closures_freevars = ref [] in
        let should_be_closure = ref false in
        let rec analyze_lets first =
          let toplevel_letfuncs_backup = !(toplevel.letfuncs) in
          let toplevel_strings_backup = !(toplevel.strings) in
          let funcvars =
            hashmap_of_list
            @@ filter_after_map
                 (function
                   | LetFunc (_, funcname, args, _, _) ->
                       let gen_funcname =
                         Hashtbl.find funcnames2gen funcname
                       in
                       Some
                         ( if first then
                           (funcname, FuncVar (gen_funcname, List.length args))
                         else (funcname, Var gen_funcname) )
                   | _ -> None)
                 src
          in
          let rec aux' env' = function
            | LetVar (false, bind, lhs) ->
                let env' =
                  {env' with symbols= add_symbols_in_pattern env'.symbols bind}
                in
                (env', LetVar (false, aux_ptn env' bind, aux env lhs))
            | LetFunc (recursive, funcname, args, func, _) ->
                let gen_funcname = Hashtbl.find funcnames2gen funcname in
                let env_in =
                  { symbols= add_symbols_in_patterns HashMap.empty args
                  ; parent= Some env
                  ; freevars= ref [] }
                in
                (* if recursive then funcname(s) should be in env *)
                let env_in =
                  if not recursive then env_in
                  else {env_in with symbols= integrate env_in.symbols funcvars}
                in
                let func = aux env_in func in
                (* Delete duplicate freevars *)
                env_in.freevars := list_unique !(env_in.freevars) ;
                let freevars =
                  ref (List.map (fun (_, a) -> a) !(env_in.freevars))
                in
                if first then (
                  (* Save data for the possible second loop *)
                  let_closures_freevars := !freevars @ !let_closures_freevars ;
                  (* If the function is recursive and should call itself as a closure,
                   * then Var should be used rather than FuncVar *)
                  if recursive && List.length !freevars <> 0 then
                    should_be_closure := true ;
                  if !should_be_closure then raise Should_be_closure ) ;
                let func =
                  if first then func
                  else (
                    (* In the target function, all functions chained with keyword 'and' should be available.
                     * This means that they should be defined as closures at the head of the target function.
                     * Note that these closures should have *all* freevars in chained functions. *)
                    (* TODO: only functions appeared in freevars need to be available. *)
                    freevars := !let_closures_freevars ;
                    LetAndAnalyzed
                      ( filter_after_map
                          (function
                            | LetFunc (_, funcname, args, _, _) ->
                                let gen_funcname =
                                  Hashtbl.find funcnames2gen funcname
                                in
                                Some
                                  (LetVar
                                     ( false
                                     , Var gen_funcname
                                     , MakeCls
                                         ( gen_funcname
                                         , List.length args
                                         , !let_closures_freevars ) ))
                            | _ -> None)
                          src
                      , func ) )
                in
                (* freevars are passed to env if they are not defined in env *)
                List.iter
                  (fun ((name, _) as var) ->
                    let d, _ = find_symbol env name in
                    if d <> 0 then env.freevars := var :: !(env.freevars) )
                  !(env_in.freevars) ;
                if List.length !freevars = 0 then (
                  (* no freevars; no need for closure *)
                  let env_out =
                    { env' with
                      symbols=
                        HashMap.add funcname
                          (FuncVar (gen_funcname, List.length args))
                          env'.symbols }
                  in
                  let ast =
                    LetFunc
                      ( recursive
                      , gen_funcname
                      , List.map (fun x -> aux_ptn env_in x) args
                      , func
                      , [] )
                  in
                  append_to_list_ref ast toplevel.letfuncs ;
                  (env_out, ast) )
                else
                  (* closure *)
                  let funcvar = Var gen_funcname in
                  let env_out =
                    { env' with
                      symbols= HashMap.add funcname funcvar env'.symbols }
                  in
                  let ast =
                    LetFunc
                      ( recursive
                      , gen_funcname
                      , List.map (fun x -> aux_ptn env_in x) args
                      , func
                      , !freevars )
                  in
                  append_to_list_ref ast toplevel.letfuncs ;
                  ( env_out
                  , LetVar
                      ( false
                      , funcvar
                      , MakeCls (gen_funcname, List.length args, !freevars) )
                  )
            | _ -> raise Unexpected_ast
          in
          let env', lets =
            List.fold_left
              (fun (env', lets) le ->
                try
                  match le with
                  | LetVar _ ->
                      let env', le_analyzed = aux' env' le in
                      (env', le_analyzed :: lets)
                  | LetFunc _ ->
                      let env', le_analyzed = aux' env' le in
                      (env', le_analyzed :: lets)
                  | _ -> failwith "unexpected ast"
                with Should_be_closure when first -> (env', lets) )
              (env, []) src
          in
          if first && !should_be_closure then (
            toplevel.letfuncs := toplevel_letfuncs_backup ;
            toplevel.strings := toplevel_strings_backup ;
            let_closures_freevars := list_unique !let_closures_freevars ;
            analyze_lets false )
          else
            match rhs_of_in with
            | None -> raise (LetDef (lets, env'))
            | Some rhs -> LetAndAnalyzed (lets, aux env' rhs)
        in
        analyze_lets true
    | _ -> raise Unexpected_ast
  and analyze_module env exprs =
    let toplevel_env = ref env in
    let rec aux' exprs = function
      | ModuleDef (this_modulename, body) :: asts ->
          toplevel.modulename := this_modulename :: !(toplevel.modulename) ;
          (* TODO: is there any better way? *)
          aux' exprs @@ body @ (ModuleDefEnd :: asts)
      | ModuleDefEnd :: asts ->
          toplevel.modulename := List.tl !(toplevel.modulename) ;
          aux' exprs asts
      | ExternalDecl (id, typexpr, decl) :: asts ->
          let id = with_modulename id in
          let nargs =
            let rec aux cnt = function
              | TyFunc (lhs, rhs) -> aux (cnt + 1) rhs
              | _ -> cnt
            in
            aux 0 typexpr
          in
          toplevel_env :=
            { !toplevel_env with
              symbols=
                HashMap.add id (FuncVar (decl, nargs)) !toplevel_env.symbols } ;
          aux' exprs asts
      | ast :: asts -> (
        try aux' (aux !toplevel_env ast :: exprs) asts
        with LetDef (lets, env) ->
          toplevel_env := env ;
          exprs2expr @@ List.rev
          @@ (LetAndAnalyzed (lets, aux' [] asts) :: exprs) )
      | [] -> exprs2expr @@ List.rev exprs
    in
    let ast = aux' [] exprs in
    (!toplevel_env, ast)
  in
  let env = {symbols= HashMap.empty; parent= None; freevars= ref []} in
  let _, ast = analyze_module env asts in
  let ast = LetFunc (false, "aqaml_main", [UnitValue], ast, []) in
  append_to_list_ref ast toplevel.letfuncs ;
  ( !(toplevel.letfuncs)
  , !(toplevel.strings)
  , !(toplevel.typedefs)
  , !(toplevel.exps_list) )

;;
test
  (analyze (parse (tokenize "exception Match_failure")))
  ( [LetFunc (false, "aqaml_main", [UnitValue], Nope, [])]
  , []
  , []
  , ["Match_failure"] )

;;
test
  (analyze (parse (tokenize "external ref : 'a -> 'a ref = \"aqaml_ref\"\n")))
  ([LetFunc (false, "aqaml_main", [UnitValue], Nope, [])], [], [], [])

;;
let analyzed_data =
  analyze
    (parse
       (tokenize
          "\n\
           external ref : 'a -> 'a ref = \"aqaml_ref\"\n\n\
           external raise : exn -> 'a = \"aqaml_raise\"\n\n\
           external exit : int -> 'a = \"aqaml_exit\"\n\n\
           external print_string : string -> unit = \"aqaml_print_string\"\n\n\
           external string_of_int : int -> string = \"aqaml_string_of_int\"\n\n\
           external int_of_char : char -> int = \"aqaml_char_code\"\n\n\
           exception Match_failure of string * int * int\n\n\
           exception Not_found\n\n\
           exception Failure of string\n\n\
           let ignore _ = ()\n\n\
           let failwith str = raise (Failure str)\n\n\
           module Char = struct\n\
           external code : char -> int = \"aqaml_char_code\"\n\
           end\n\n\
           type bytes = string\n\n\
           module Bytes = struct\n\
           external length : bytes -> int = \"aqaml_string_length\"\n\n\
           external get : bytes -> int -> char = \"aqaml_string_get\"\n\n\
           external set : bytes -> int -> char -> unit = \"aqaml_string_set\"\n\n\
           external create : int -> bytes = \"aqaml_string_create\"\n\n\
           external sub : bytes -> int -> int -> bytes = \"aqaml_string_sub\"\n\n\
           external sub_string : bytes -> int -> int -> string = \
           \"aqaml_string_sub\"\n\n\
           external blit :\n\
           bytes -> int -> bytes -> int -> int -> unit\n\
           = \"aqaml_string_blit\"\n\n\
           external blit_string :\n\
           string -> int -> bytes -> int -> int -> unit\n\
           = \"aqaml_string_blit\"\n\n\
           let of_string str = str\n\n\
           let to_string bytes = bytes\n\
           end\n"))
in
test analyzed_data
  ( [ LetFunc
        ( false
        , "aqaml_main"
        , [UnitValue]
        , ExprSeq
            [ Nope
            ; Nope
            ; Nope
            ; LetAndAnalyzed
                ( [LetFunc (false, "ignore.17", [Var "_.18"], UnitValue, [])]
                , LetAndAnalyzed
                    ( [ LetFunc
                          ( false
                          , "failwith.19"
                          , [Var "str.20"]
                          , AppDir
                              ( "aqaml_raise"
                              , [ CtorApp
                                    ( Some "Failure"
                                    , "Failure"
                                    , Some (Var "str.20") ) ] )
                          , [] ) ]
                    , ExprSeq
                        [ Nope
                        ; LetAndAnalyzed
                            ( [ LetFunc
                                  ( false
                                  , "Bytes.of_string.21"
                                  , [Var "str.22"]
                                  , Var "str.22"
                                  , [] ) ]
                            , LetAndAnalyzed
                                ( [ LetFunc
                                      ( false
                                      , "Bytes.to_string.23"
                                      , [Var "bytes.24"]
                                      , Var "bytes.24"
                                      , [] ) ]
                                , Nope ) ) ] ) ) ]
        , [] )
    ; LetFunc
        (false, "Bytes.to_string.23", [Var "bytes.24"], Var "bytes.24", [])
    ; LetFunc (false, "Bytes.of_string.21", [Var "str.22"], Var "str.22", [])
    ; LetFunc
        ( false
        , "failwith.19"
        , [Var "str.20"]
        , AppDir
            ( "aqaml_raise"
            , [CtorApp (Some "Failure", "Failure", Some (Var "str.20"))] )
        , [] )
    ; LetFunc (false, "ignore.17", [Var "_.18"], UnitValue, []) ]
  , []
  , [DefTypeAlias (None, "bytes", TyString)]
  , ["Failure"; "Not_found"; "Match_failure"] )
