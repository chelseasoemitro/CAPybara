(* Ocamllex scanner for CAPybara *)

{ open Capyparse }

let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z']
let ascii_char = [' '-'~']
let escape_char = '\\' ['\\' 'n' 't' 'r' ''' '"' ]

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf }      (* Whitespace *)

(* Comments *)
| "//"     { singlecomment lexbuf}
| "/*"     { multicomment lexbuf }

(* Parentheses and brackets *)
| '('      { L_PAREN }
| ')'      { R_PAREN }
| '{'      { L_CBRACE }
| '}'      { R_CBRACE }
| '['      { L_SQBRACE }
| ']'      { R_SQBRACE }

(* Delimiters *)
| ';'      { SEMI }
| ','      { COMMA }

(* Arithmetic operators *)
| '+'      { PLUS }
| '-'      { MINUS }
| '*'      { MUL }
| '/'      { DIV }
| '%'      { MOD }
| "**"     { POW }

(* Assignment *)
| '='      { ASSIGN }

(* Relational operators *)
| "=="     { EQ }
| "!="     { NEQ }
| '<'      { LT }
| "<="     { LE }
| '>'      { GT }
| ">="     { GE }

(* Logical operators *)
| "!"      { NOT }
| "&&"     { AND }
| "||"     { OR }

(* Statements and function keywords *)
| "if"     { IF }
| "else"   { ELSE }
| "while"  { WHILE }
| "for"    { FOR }
| "break"  { BREAK }
| "return" { RETURN }
| "void"   { VOID }

(* Primitive types *)
| "int"    { INT }
| "double" { DOUBLE }
| "bool"   { BOOL }
| "char"   { CHAR }
| "string" { STRING }

(* Literals *)
| digit+ as lem                                 { INT_LIT(int_of_string lem) }
| digit+ ('.' digit+)? as lem                   { DOUBLE_LIT(float_of_string lem) }
| "true"                                        { BOOL_LIT(true) }
| "false"                                       { BOOL_LIT(false) }
| '\'' (ascii_char as lem) '\''                 { CHAR_LIT(lem)}
| '"' ((ascii_char | escape_char)* as lem) '"'  { STRING_LIT(Scanf.unescaped lem) }

(* Array operators *)
| ':'      { COLON }
| '@'      { MMUL }
| "length"    { LENGTH }      
| "transpose" { TRANSPOSE }
| "map"       { MAP }
| "reduce"    { REDUCE }

| letter (digit | letter | '_')* as lem { ID(lem) }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }


and singlecomment = parse
'\n' { token lexbuf }
| _    { singlecomment lexbuf }

and multicomment = parse
  "*/" { token lexbuf }
| _    { multicomment lexbuf }
