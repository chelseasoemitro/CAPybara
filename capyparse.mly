/* Ocamlyacc parser for CAPybara */

%{
open Ast
%}

(* Parentheses and brackets *)
%token L_PAREN R_PAREN L_CBRACE R_CBRACE L_SQBRACE R_SQBRACE

(* Delimiters *)
%token SEMI COMMA

(* Arithmetic operators *)
%token PLUS MINUS MUL DIV MOD POW

(* Assignment *)
%token ASSIGN

(* Relational operators *)
%token EQ NEQ LT LE GT GE

(* Logical operators *)
%token NOT AND OR

(* Statements and function keywords *)
%token IF ELSE WHILE FOR BREAK RETURN VOID

(* Primitive types *)
%token INT DOUBLE BOOL CHAR STRING

(* Literals *)
%token <int> INT_LIT
%token <float> DOUBLE_LIT
%token <bool> BOOL_LIT
%token <char> CHAR_LIT
%token <string> STRING_LIT

(* Array Operators *)
%token COLON MMUL LENGTH TRANSPOSE MAP REDUCE

%token <string> ID
%token EOF



%start program
%type <Ast.tokenseq> program

(* lowest precedence *)
%right ASSIGN 
%left OR
%left AND
%left EQ NEQ
%left LT GT LE GE
%left PLUS MINUS 
%left MUL DIV MOD
%left MMUL
%left COLON 
%left LENGTH TRANSPOSE MAP REDUCE
%right NOT
%right POW 
%nonassoc L_PAREN R_PAREN L_SQBRACE R_SQBRACE
(* highest precedence *)

%%

program:
  tokens EOF { $1 }

tokens:
  /* nothing */ { [] }
  | one_token tokens { $1 :: $2 }

one_token:
  | L_PAREN { "L_PAREN" }
  | R_PAREN { "R_PAREN" }
  | L_CBRACE { "L_CBRACE" }
  | R_CBRACE { "R_CBRACE" }
  | L_SQBRACE { "L_SQBRACE" }
  | R_SQBRACE { "R_SQBRACE" }
  | SEMI { "SEMI" }
  | COMMA { "COMMA" }
  | PLUS { "PLUS" }
  | MINUS { "MINUS" }
  | MUL { "MUL" }
  | DIV { "DIV" }
  | MOD { "MOD" }
  | POW { "POW"} 
  | ASSIGN { "ASSIGN" }
  | EQ { "EQ" }
  | NEQ { "NEQ" }
  | LT { "LT" }
  | LE { "LE" }
  | GT { "GT" }
  | GE { "GE" }
  | NOT { "NOT" }
  | AND { "AND" }
  | OR { "OR" }
  | IF { "IF"}
  | ELSE { "ELSE" }
  | WHILE { "WHILE" }
  | FOR { "FOR" }
  | BREAK { "BREAK" }
  | RETURN { "RETURN" }
  | VOID { "VOID" }
  | INT { "INT" }
  | DOUBLE { "DOUBLE" }
  | BOOL { "BOOL" }
  | CHAR { "CHAR" }
  | STRING { "STRING" }
  | INT_LIT { "INT: " ^ string_of_int $1}
  | DOUBLE_LIT { "DOUBLE: " ^ string_of_float $1}
  | BOOL_LIT { "BOOL: " ^ string_of_bool $1}
  | CHAR_LIT { "CHAR: " ^ String.make 1 $1}

  | STRING_LIT { "STRING: " ^ $1}
  | COLON { "COLON" }
  | MMUL { "MMUL" }
  | LENGTH { "LENGTH" }
  | TRANSPOSE { "TRANSPOSE" }
  | MAP { "MAP" }
  | REDUCE { "REDUCE" }
  | ID { "ID: " ^ $1}



// program_rule:
//   vdecl_list_rule stmt_list_rule EOF { {locals=$1; body=$2} }

// vdecl_list_rule:
//   /*nothing*/                   { []       }
//   | vdecl_rule vdecl_list_rule  { $1 :: $2 }

// vdecl_rule:
//   typ_rule ID SEMI { ($1, $2) }


// typ_rule:
//   INT       { Int  }
//   | BOOL    { Bool }

// stmt_list_rule:
//     /* nothing */               { []     }
//     | stmt_rule stmt_list_rule  { $1::$2 }

// stmt_rule:
//   expr_rule SEMI                                          { Expr $1         }
//   | LBRACE stmt_list_rule RBRACE                          { Block $2        }
//   | IF LPAREN expr_rule RPAREN stmt_rule ELSE stmt_rule   { If ($3, $5, $7) }
//   | WHILE LPAREN expr_rule RPAREN stmt_rule               { While ($3,$5)   }

// expr_rule:
//   | BLIT                          { BoolLit $1            }
//   | LITERAL                       { Literal $1            }
//   | ID                            { Id $1                 }
//   | expr_rule PLUS expr_rule      { Binop ($1, Add, $3)   }
//   | expr_rule MINUS expr_rule     { Binop ($1, Sub, $3)   }
//   | expr_rule EQ expr_rule        { Binop ($1, Equal, $3) }
//   | expr_rule NEQ expr_rule       { Binop ($1, Neq, $3)   }
//   | expr_rule LT expr_rule        { Binop ($1, Less, $3)  }
//   | expr_rule AND expr_rule       { Binop ($1, And, $3)   }
//   | expr_rule OR expr_rule        { Binop ($1, Or, $3)    }
//   | ID ASSIGN expr_rule           { Assign ($1, $3)       }
//   | LPAREN expr_rule RPAREN       { $2                    }
