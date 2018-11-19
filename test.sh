#!/bin/bash

function test_aqaml() {
    echo "$1" | ./aqaml > _test.s
    gcc _test.s -o _test.o
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

