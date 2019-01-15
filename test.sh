#!/bin/bash

cat _test.ml | ./aqaml > _test.s
gcc utility.o _test.s -o _test.o
./_test.o
