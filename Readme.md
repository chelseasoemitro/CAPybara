### Build the CAPybara compiler

```
make
```

### Compile and run a CAPybara file
```
./capybara.native -c <program_name>.cap
lli <program_name>
```

### Run the CAPybara test suite
```
bash test_runner.sh
```

### Compiler files
-  `ast.ml`: abstract syntax tree (AST)
-  `scanner.mll`: scanner
-  `capyparse.mly`: parser
-  `sast.ml`: semantically-checked abstract syntax tree
-  `semant.ml`: semantic checker
-  `codegen.ml`: LLVM IR code generator
-  `capybara.ml`: top-level of CAPybara compiler
-  `Makefile`: Makefile for the CAPybara compiler

### Other files and directories
- `tests/`: directory of test files
- `test_runner.sh`: bash script for running all tests
