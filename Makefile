.PHONY: clean run all

run: test_parse test_semantic
	cat example2.cap | ./test_parse.native
	cat example2.cap | ./test_semantic.native

test_parse:
	ocamlbuild test_parse.native

test_semantic:
	ocamlbuild test_semantic.native

clean:
	rm *.native

all: run clean
