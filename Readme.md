### Build the CAPybara parser

```
ocamlbuild test.native
```

### Run the CAPybara parser
```
./test.native
```

### Compiler files
-  `ast.ml`: abstract syntax tree (AST)
-  `scanner.mll`: scanner
-  `capyparse.mly`: parser
-  `sast.ml`: semantically-checked abstract syntax tree
-  `semant.ml`: semantic checker

### Other files

- `test_parse.ml`: top-level file to test and run the scanner and parser
- `test_semantic.ml`: top-level file to test and run the scanner, parser, and semantic analyzer
- `example.cap`: a sample CAPybara source code
- `example.out`: a sample parsed code of example.cap
