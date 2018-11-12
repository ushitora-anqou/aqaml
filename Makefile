aqaml: main.ml
	ocamlopt $^ -o $@

test: aqaml test.sh
	./test.sh

.PHONY: test
