#!/bin/bash

function test_aqaml() {
    echo "$1" | ./aqaml > _test.s
    gcc utility.o _test.s -o _test.o
    ./_test.o
    res=$?
    [ $res -eq $2 ] || echo "ERROR: $1 -> $res (expected $2)"
}

test_aqaml "42" 42
test_aqaml "1 + 2 + 3" 6
test_aqaml "1 + 2" 3
test_aqaml "2 - 1" 1
test_aqaml "3 - 2 - 1" 0
test_aqaml "2 * 3" 6
test_aqaml "4 / 2" 2
test_aqaml "1 + 2 * 3" 7
test_aqaml "2 * 3 + 1" 7
test_aqaml "1 + 2 * 3" 7
test_aqaml "4 / 2 * 3 + 1 - 10 / 2 + 4 * 2 * 1" 10
test_aqaml "(1 + 2) * 3" 9
test_aqaml "(1 + 2) * (3 + 4)" 21
test_aqaml "(33 * (1 + 2)) / 3" 33
test_aqaml "let pi = 3 in pi" 3
test_aqaml "let pi = 3 in pi*2" 6
test_aqaml "let pi = 3 in pi+2" 5
test_aqaml "let pi = 3 in 10-pi" 7
test_aqaml "let x = 1 in let y = 2 in x + y" 3
test_aqaml "let x = 1 in let y = 2 in let z = 3 in let w = x + y * z in w * 2" 14
test_aqaml "let x = 1 in let x = x + 1 in x" 2
test_aqaml "let x = (let y = 2 in y + 2) in x + 2" 6
test_aqaml "let id x = x in id 10" 10
test_aqaml "let id x = x in id (id 10)" 10
test_aqaml "let id x = x in 1 + id 2 + 3" 6
test_aqaml "let add1 x = x + 1 in add1 2 * 3" 9
test_aqaml "let add1 x = x + 1 in add1 (2 * 3)" 7
test_aqaml "let add1 x = x + 1 in let add1 x = x + 2 in add1 2" 4
test_aqaml "let add x y = x + y in add 2 3" 5
test_aqaml "let add x y = x + y in add (add 2 3) (add 3 4)" 12
test_aqaml "let add x y = x + y in 2 * add (add 2 3) (add 3 4)" 24
test_aqaml "if 1 = 1 then 1 else 0" 1
test_aqaml "if 2 = 1 then 1 else 0" 0
test_aqaml "let add x y = x + y in if (add 2 3) = (add 3 2) then 1 else 0" 1
test_aqaml "
if 4 * 3 = 1 + 2 then
    let f x y = x + y in
    f 3 2
else
    let f x y = x * y in
    f 3 2" 6
test_aqaml "let sub x y = x - y in sub 3 2" 1
test_aqaml "let calc x y z = y * z - x * x in calc 2 3 4" 8
test_aqaml "let rec pow n m = if m = 0 then 1 else pow n (m - 1) * n in pow 2 6" 64
test_aqaml "let rec fib n = if n <= 1 then n else fib (n - 1) + fib (n - 2) in fib 13" 233
test_aqaml "let rec trib n = if n <= 1 then 0 else if n = 2 then 1 else trib (n - 1) + trib (n - 2) + trib (n - 3) in trib 11" 149
test_aqaml "let rec lucas n = if n = 0 then 2 else if n = 1 then 1 else lucas (n - 1) + lucas (n - 2) in lucas 11" 199
test_aqaml "if 1 <> 2 then 1 else 0" 1
test_aqaml "if 1 <> 1 then 1 else 0" 0
test_aqaml "if 1 < 2 then 1 else 0" 1
test_aqaml "if 2 < 1 then 1 else 0" 0
test_aqaml "if 1 > 2 then 1 else 0" 0
test_aqaml "if 2 > 1 then 1 else 0" 1
test_aqaml "if 1 <= 2 then 1 else 0" 1
test_aqaml "if 2 <= 1 then 1 else 0" 0
test_aqaml "if 1 <= 1 then 1 else 0" 1
test_aqaml "if 1 >= 2 then 1 else 0" 0
test_aqaml "if 1 >= 1 then 1 else 0" 1
test_aqaml "let not x = if x then false else true in if not true then 1 else 0" 0
test_aqaml "let (a, b, c) = (10, 20, 30) in a + b - c" 0
test_aqaml "let x = (4, 2, 3) in let (a, b, c) = x in a * b - c" 5
test_aqaml "let ((a, b), c) = ((10, 40), 3) in b / a - c" 1
test_aqaml "let 1 = 1 in let ((10, b), c) = ((10, 40), 3) in b / 10 - c" 1
test_aqaml "let f x y = x - y in let t = (f 10 2, f 5 4) in let a, b = t in a - b" 7
test_aqaml "let f (x,y) = x - y in let t = (f (10,2), f (5,4)) in let a, b = t in a - b" 7
test_aqaml "let f (x, y, z, w) (a, b, c) = (x / y - z + w) * (a - b / c) in f (10, 2, 3, 4) (20, 14, 7)" 108
test_aqaml "if (1,2,3) = (1,2,3) then 1 else 0" 1
test_aqaml "if (1,2,4) = (1,2,3) then 1 else 0" 0
test_aqaml "if (1,2,3) <> (1,2,3) then 1 else 0" 0
test_aqaml "if (1,2,4) <> (1,2,3) then 1 else 0" 1
test_aqaml "let x = (1,2,3) in let y = (1,2,3) in if x = y then 1 else 0" 1
test_aqaml "if [] = [] then 1 else 0" 1
test_aqaml "let x::xs = [1;2;3] in if x = 1 then 1 else 0" 1
test_aqaml "let x::xs = [1;2;3] in let y::ys = xs in let z::zs = ys in if z = 3 then 1 else 0" 1
test_aqaml "let f (x::xs, y) z = x - y * z in f ([10;20], 2) 3" 4
test_aqaml "
let f ([x;y], z) w = x - y * z + w in
let add x y = x + y in
let sub x y = x - y in
let div x y = x / y in
f ([add 10 2; sub 5 1], div 6 4) 10" 18
test_aqaml "let f ([a;b;c], [d;e;f], [g;h;i]) = f in f ([1;2;3], [4;5;6], [7;8;9])" 6
test_aqaml "if [1;2;3] = [1;2;3] then 1 else 0" 1
test_aqaml "if [1;2;4] = [1;2;3] then 1 else 0" 0
test_aqaml "if [1;2;3] <> [1;2;3] then 1 else 0" 0
test_aqaml "if [1;2;4] <> [1;2;3] then 1 else 0" 1
test_aqaml "(**)45" 45
test_aqaml "let v = (*)Uh-huh.)**((
)*(*Chaos is not kaos*)really?*)45 in v" 45
