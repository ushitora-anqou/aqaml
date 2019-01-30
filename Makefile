SRC=hashmap.ml hashtbl.ml helper.ml lexer.ml parser.ml main.ml

aqaml: $(SRC)
	ocamlopt $^ -o $@

test: aqaml test.sh utility.o
	./test.sh

utility.o: utility.c
	gcc -Wall -std=c11 -c -o $@ $^

_self_aqaml: stdlib.ml $(SRC) utility.o aqaml
	./aqaml stdlib.ml $(SRC) > _self_aqaml.s
	gcc _self_aqaml.s utility.o -o $@
	strip $@

_self_test.sh: test.sh
	cat test.sh | sed "s#./aqaml#./_self_aqaml#g" > _self_test.sh
	chmod +x _self_test.sh

self_test: _self_aqaml _self_test.sh utility.o
	./_self_test.sh

_selfself_aqaml: stdlib.ml $(SRC) utility.o _self_aqaml
	./_self_aqaml stdlib.ml $(SRC) > _selfself_aqaml.s
	gcc _selfself_aqaml.s utility.o -o $@
	strip $@

_selfself_test.sh: test.sh
	cat test.sh | sed "s#./aqaml#./_selfself_aqaml#g" > _selfself_test.sh
	chmod +x _selfself_test.sh

selfself_test: _selfself_aqaml _selfself_test.sh utility.o
	./_selfself_test.sh
	cmp _self_aqaml _selfself_aqaml

clean:
	rm -f _self_test.sh _self_aqaml _self_aqaml.s _test.o _test.s aqaml utility.o _selfself_aqaml _selfself_aqaml.s _selfself_test.sh
	rm -f $(SRC:.ml=.cmi) $(SRC:.ml=.cmx) $(SRC:.ml=.o) $(SRC:.ml=.cmo)

.PHONY: test self_test clean selfself_test
