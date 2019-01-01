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

(* from commit 7201ef6ac69c3fd93bd1e6dcb236c91276a8a75e *)
type token = IntLiteral of int | Plus | Minus | Star | Slash

exception EOF

let digit x = match x with '0' .. '9' -> Char.code x - Char.code '0'

let tokenize program =
  let next_char i =
    if i < String.length program then (i + 1, program.[i]) else raise EOF
  in
  let rec next_int i acc =
    try
      let i, ch = next_char i in
      match ch with
      | '0' .. '9' -> next_int i ((acc * 10) + digit ch)
      | _ -> (i - 1, acc)
    with EOF -> (i, acc)
  in
  let rec tokenize i =
    try
      let i, ch = next_char i in
      match ch with
      | ' ' | '\t' | '\n' -> tokenize i
      | '0' .. '9' ->
          let i, num = next_int (i - 1) 0 in
          IntLiteral num :: tokenize i
      | '+' -> Plus :: tokenize i
      | '-' -> Minus :: tokenize i
      | '*' -> Star :: tokenize i
      | '/' -> Slash :: tokenize i
    with EOF -> []
  in
  tokenize 0

type ast =
  | Int of int
  | Add of (ast * ast)
  | Sub of (ast * ast)
  | Mul of (ast * ast)
  | Div of (ast * ast)

let parse tokens =
  let parse_integer = function
    | IntLiteral num :: tokens -> (tokens, Int num)
  in
  let parse_multiplicative tokens =
    let rec aux lhs tokens =
      match tokens with
      | Star :: tokens ->
          let tokens, rhs = parse_integer tokens in
          aux (Mul (lhs, rhs)) tokens
      | Slash :: tokens ->
          let tokens, rhs = parse_integer tokens in
          aux (Div (lhs, rhs)) tokens
      | _ -> (tokens, lhs)
    in
    let tokens, ast = parse_integer tokens in
    aux ast tokens
  in
  let parse_additive tokens =
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
  in
  let _, ast = parse_additive tokens in
  ast

let rec eval = function
  | Int num -> num
  | Add (lhs, rhs) -> eval lhs + eval rhs
  | Sub (lhs, rhs) -> eval lhs - eval rhs
  | Mul (lhs, rhs) -> eval lhs * eval rhs
  | Div (lhs, rhs) -> eval lhs / eval rhs

;;
test (eval (parse (tokenize "4 / 2 * 3 + 1 - 10 / 2 + 4 * 2 * 1"))) 10

let ignore _ = ()

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
