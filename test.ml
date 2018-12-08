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

;;
let rec fold_left f a bs =
  match bs with b :: bs -> fold_left f (f a b) bs | _ -> a
in
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
