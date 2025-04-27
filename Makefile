.PHONY: all

run_tests: capybara test_parse test_semantic
	cat example2.cap | ./test_parse.native
	cat example2.cap | ./test_semantic.native
	cat example2.cap | ./capybara.native

capybara: 
	ocamlbuild capybara.native

test_parse:
	ocamlbuild test_parse.native

test_semantic:
	ocamlbuild test_semantic.native

clean:
	rm *.native

all: clean run_tests
