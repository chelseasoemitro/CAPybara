OCB=ocamlbuild -use-ocamlfind -pkg llvm
TARGET=capybara.native

.PHONY: all tests clean

all: $(TARGET)

$(TARGET):
	$(OCB) $(TARGET)

clean:
	$(OCB) -clean
	rm -f parser.ml parser.mli parser.output scanner.ml a.out