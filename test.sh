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
#test_aqaml "pi" 3
#test_aqaml "pi*2" 6
#test_aqaml "pi+2" 5
#test_aqaml "10-pi" 7
#test_aqaml "id 10" 10
#test_aqaml "id (id 10)" 10
#test_aqaml "1 + id 2 + 3" 6
#test_aqaml "add1 2 * 3" 9
#test_aqaml "add1 (2 * 3)" 7
#test_aqaml "add 2 3" 5
#test_aqaml "add (add 2 3) (add 3 4)" 12
#test_aqaml "2 * add (add 2 3) (add 3 4)" 24
