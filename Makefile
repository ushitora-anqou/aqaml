aqaml: main.ml
	ocamlopt $^ -o $@

test: aqaml test.sh utility.o _test.ml
	./test.sh

utility.o: utility.c
	gcc -Wall -std=c11 -c -o $@ $^

_test.ml: stdlib.ml test.ml
	cat stdlib.ml test.ml > $@

_self_aqaml: stdlib.ml main.ml utility.o aqaml
	cat stdlib.ml main.ml | ./aqaml > _self_aqaml.s
	gcc _self_aqaml.s utility.o -o $@
	strip $@

_self_test.sh: test.sh
	cat test.sh | sed "s#./aqaml#./_self_aqaml#g" > _self_test.sh
	chmod +x _self_test.sh

self_test: _self_aqaml _test.ml _self_test.sh utility.o
	./_self_test.sh

_selfself_aqaml: stdlib.ml main.ml utility.o _self_aqaml
	cat stdlib.ml main.ml | ./_self_aqaml > _selfself_aqaml.s
	gcc _selfself_aqaml.s utility.o -o $@
	strip $@

_selfself_test.sh: test.sh
	cat test.sh | sed "s#./aqaml#./_selfself_aqaml#g" > _selfself_test.sh
	chmod +x _selfself_test.sh

selfself_test: _selfself_aqaml _test.ml _selfself_test.sh utility.o
	./_selfself_test.sh
	cmp _self_aqaml _selfself_aqaml

clean:
	rm -f _test.ml _self_test.sh _self_aqaml _self_aqaml.s _test.o _test.s aqaml utility.o _selfself_aqaml _selfself_aqaml.s _selfself_test.sh

.PHONY: test self_test clean selfself_test
