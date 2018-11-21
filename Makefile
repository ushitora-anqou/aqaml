aqaml: main.ml
	ocamlopt $^ -o $@

test: aqaml test.sh utility.o
	./test.sh

utility.o: utility.c
	gcc -c -o $@ $^

.PHONY: test
