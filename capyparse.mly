/* Ocamlyacc parser for CAPybara */

%{
open Ast
%}

(* Parentheses and brackets *)
%token L_PAREN R_PAREN L_CBRACE R_CBRACE L_SQBRACE R_SQBRACE

(* Delimiters *)
%token SEMI COMMA

(* Arithmetic operators *)
%token PLUS MINUS MULT DIV MOD POW

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
%token COLON MMULT LENGTH TRANSPOSE MAP REDUCE

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
%left MULT DIV MOD
%left MMULT
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
  | MULT { "MULT" }
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
  | MMULT { "MMULT" }
  | LENGTH { "LENGTH" }
  | TRANSPOSE { "TRANSPOSE" }
  | MAP { "MAP" }
  | REDUCE { "REDUCE" }
  | ID { "ID: " ^ $1}

// MICROC

/* add function declarations*/
program:
  decls EOF { $1 }

decls:
   /* nothing */    { ([], [])                 }
 | vdecl_stmt SEMI decls { (($1 :: fst $3), snd $3) }  /* vdecls here are global */
 | fdecl decls      { (fst $2, ($1 :: snd $2)) }


/* int x */
vdecl:
  typ ID { ($1, $2)}

typ:
    INT   { Int   }
  | DOUBLE  { Double }
  | BOOL  { Bool  }
  | CHAR { Char }
  | STRING { String }
  | VOID { Void }
  | typ arr_index { Arr($1, [$2]) }
  | typ arr_index arr_index { Arr($1, [$2 :: $3]) }

arr_index:
  L_SQBRACE INT_LIT R_SQBRACE { $2 }

/* fdecl */
fdecl:
  vdecl LPAREN formals_opt RPAREN LBRACE stmt_list RBRACE
  {
    {
      rtyp=fst $1;
      fname=snd $1;
      formals=$3;
      body=$6
    }
  }

/* formals_opt */
formals_opt:
  /*nothing*/     { [] }
  | formals_list  { $1 }

formals_list:
  vdecl { [$1] }
  | vdecl COMMA formals_list { $1::$3 }

stmt_list:
  /* nothing */ { [] }
  | stmt stmt_list        { $1::$2 }

vdecl_stmt:
    typ ID              { ($1, $2, None)    }
  | typ ID ASSIGN expr  { ($1, $2, Some (Assign($2, $4))) }

stmt:
    expr SEMI                               { Expr $1      }
  | LBRACE stmt_list RBRACE                 { Block $2 }
  /* if (condition) { block1} else {block2} */
  /* if (condition) stmt else stmt */
  | IF LPAREN expr RPAREN stmt ELSE stmt    { If($3, $5, $7) }
  | WHILE LPAREN expr RPAREN stmt           { While ($3, $5)  }
  /* return */
  | RETURN expr SEMI                        { Return $2      }
  | vdecl_stmt SEMI                              { $1 } 

expr:
    LITERAL          { Literal($1)            }
  | BLIT             { BoolLit($1)            }
  | ID               { Id($1)                 }
  | expr PLUS   expr { Binop($1, Add,   $3)   }
  | expr MINUS  expr { Binop($1, Sub,   $3)   }
  | expr EQ     expr { Binop($1, Equal, $3)   }
  | expr NEQ    expr { Binop($1, Neq, $3)     }
  | expr LT     expr { Binop($1, Less,  $3)   }
  | expr AND    expr { Binop($1, And,   $3)   }
  | expr OR     expr { Binop($1, Or,    $3)   }
  | ID ASSIGN expr   { Assign($1, $3)         }
  | LPAREN expr RPAREN { $2                   }
  /* call */
  | ID LPAREN args_opt RPAREN { Call ($1, $3)  }

/* args_opt*/
args_opt:
  /*nothing*/ { [] }
  | args { $1 }

args:
  expr  { [$1] }
  | expr COMMA args { $1::$3 }


// NANOC
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
