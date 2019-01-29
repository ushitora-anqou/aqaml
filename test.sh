#!/bin/bash

cat _test.ml | ./aqaml > _test.s
gcc utility.o _test.s -o _test.o
./_test.o test_command_line_argument1 test_command_line_argument2
