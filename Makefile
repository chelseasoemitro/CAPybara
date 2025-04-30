OCB=ocamlbuild -use-ocamlfind -pkg llvm
TARGET=capybara.native

.PHONY: all tests clean run_example1 run_example2

all: $(TARGET)

$(TARGET):
	$(OCB) $(TARGET)

run_example1:
	./capybara.native -c example.cap
	lli a.out

run_example2:
	./capybara.native -c example2.cap
	lli a.out

clean:
	$(OCB) -clean
	rm -f parser.ml parser.mli parser.output scanner.ml a.out