#!/bin/bash

cat stdlib.ml test.ml > _test.ml
cat _test.ml | ./aqaml > _test.s
gcc utility.o _test.s -o _test.o
./_test.o
