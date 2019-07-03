#!/bin/bash

./aqaml stdlib.ml test.ml > _test.s
gcc runtime.o _test.s -o _test.o
./_test.o test_command_line_argument1 test_command_line_argument2
