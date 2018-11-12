#!/bin/bash

function test_aqaml() {
    echo $1 | ./aqaml > _test.s
    gcc _test.s -o _test.o
    ./_test.o
    res=$?
    [ $res -eq $2 ] || echo "ERROR: $1 -> $res (expected $2)"
}

test_aqaml "42" 42
