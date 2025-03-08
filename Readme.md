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

### Other files

- `test.ml`: top-level file to test and run the scanner
- `example.cap`: a sample CAPybara source code
- `example.out`: a sample parsed code of example.cap
