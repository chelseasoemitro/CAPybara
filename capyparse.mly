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
%type <Ast.program> program

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
%right NOT NEG
%right POW 
%nonassoc L_PAREN R_PAREN L_SQBRACE R_SQBRACE
(* highest precedence *)

%%

// program:
//   tokens EOF { $1 }

// tokens:
//   /* nothing */ { [] }
//   | one_token tokens { $1 :: $2 }

// one_token:
//   | L_PAREN { "L_PAREN" }
//   | R_PAREN { "R_PAREN" }
//   | L_CBRACE { "L_CBRACE" }
//   | R_CBRACE { "R_CBRACE" }
//   | L_SQBRACE { "L_SQBRACE" }
//   | R_SQBRACE { "R_SQBRACE" }
//   | SEMI { "SEMI" }
//   | COMMA { "COMMA" }
//   | PLUS { "PLUS" }
//   | MINUS { "MINUS" }
//   | MULT { "MULT" }
//   | DIV { "DIV" }
//   | MOD { "MOD" }
//   | POW { "POW"} 
//   | ASSIGN { "ASSIGN" }
//   | EQ { "EQ" }
//   | NEQ { "NEQ" }
//   | LT { "LT" }
//   | LE { "LE" }
//   | GT { "GT" }
//   | GE { "GE" }
//   | NOT { "NOT" }
//   | AND { "AND" }
//   | OR { "OR" }
//   | IF { "IF"}
//   | ELSE { "ELSE" }
//   | WHILE { "WHILE" }
//   | FOR { "FOR" }
//   | BREAK { "BREAK" }
//   | RETURN { "RETURN" }
//   | VOID { "VOID" }
//   | INT { "INT" }
//   | DOUBLE { "DOUBLE" }
//   | BOOL { "BOOL" }
//   | CHAR { "CHAR" }
//   | STRING { "STRING" }
//   | INT_LIT { "INT: " ^ string_of_int $1}
//   | DOUBLE_LIT { "DOUBLE: " ^ string_of_float $1}
//   | BOOL_LIT { "BOOL: " ^ string_of_bool $1}
//   | CHAR_LIT { "CHAR: " ^ String.make 1 $1}
//   | STRING_LIT { "STRING: " ^ $1}
//   | COLON { "COLON" }
//   | MMULT { "MMULT" }
//   | LENGTH { "LENGTH" }
//   | TRANSPOSE { "TRANSPOSE" }
//   | MAP { "MAP" }
//   | REDUCE { "REDUCE" }
//   | ID { "ID: " ^ $1}

/* add function declarations*/
program:
  decls EOF { $1 }

decls:
   /* nothing */          { ([], [])                 }
 | vdecl_stmt SEMI decls  { (($1 :: fst $3), snd $3) }  /* vdecls here are global */
 | fdecl decls            { (fst $2, ($1 :: snd $2)) }


/* int x */
vdecl:
  typ ID { ($1, $2) }

typ:
    INT           { Int     }
  | DOUBLE        { Double  }
  | BOOL          { Bool    }
  | CHAR          { Char    }
  | STRING        { String  }
  | VOID          { Void    }
  | typ arr_index           { Arr1D($1, $2)     }
  | typ arr_index arr_index { Arr2D($1, $2, $3) }

arr_index:
  L_SQBRACE expr R_SQBRACE { $2 }

/* fdecl */
fdecl:
  vdecl L_PAREN formals_opt R_PAREN L_CBRACE stmt_list R_CBRACE
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
  vdecl  { [$1] }
  | vdecl COMMA formals_list  { $1::$3 }

stmt_list:
  /* nothing */ { [] }
  | stmt stmt_list        { $1::$2 }

vdecl_stmt:
    typ ID              { ($1, $2, None)                  }
  | typ ID ASSIGN expr  { ($1, $2, Some (Assign($2, $4))) }

stmt:
    expr SEMI                                   { Expr $1         }
  | L_CBRACE stmt_list R_CBRACE                 { Block $2        }
  /* if (condition) { block1} else {block2} */
  /* if (condition) stmt else stmt */
  | IF L_PAREN expr R_PAREN stmt ELSE stmt              { If($3, $5, $7)      }
  | IF L_PAREN expr R_PAREN stmt                        { If($3, $5, NoExpr)  }
  | FOR L_PAREN expr SEMI expr SEMI expr R_PAREN stmt   { For($3, $5, $7, $9) }
  | WHILE L_PAREN expr R_PAREN stmt                     { While ($3, $5)      }
  /* return */
  | RETURN expr SEMI                        { Return $2 }
  | BREAK SEMI                              { Break     }
  | vdecl_stmt SEMI                         { $1        } 

expr_list:
  expr { [$1] }
  | expr COMMA expr_list { $1::$3 }

expr_list_2D:
  expr_list { [$1] }
  | expr_list SEMI expr_list_2D { $1::$3 }  

expr:
    INT_LIT           { IntLit($1)    }
  | DOUBLE_LIT        { DoubleLit($1) }
  | BOOL_LIT          { BoolLit($1)   }
  | CHAR_LIT          { CharLit($1)   }
  | STRING_LIT        { StringLit($1) }
  | L_SQBRACE expr_list R_SQBRACE       { Arr1DLit($2)  }
  | L_SQBRACE expr_list_2D R_SQBRACE    { Arr2DLit($2)  }
  | ID               { Id($1)                 }
  | expr PLUS   expr { Binop($1, Add,   $3)   }
  | expr MINUS  expr { Binop($1, Sub,   $3)   }
  | expr MULT   expr { Binop($1, Mult,  $3)   }
  | expr DIV    expr { Binop ($1, Div, $3)    }
  | expr MOD    expr { Binop($1, Mod, $3)     }
  | expr POW    expr { Binop($1, Pow, $3)     }
  | expr EQ     expr { Binop($1, Equal, $3)   }
  | expr NEQ    expr { Binop($1, Neq, $3)     }
  | expr LT     expr { Binop($1, Lt,  $3)     }
  | expr GT     expr { Binop($1, Gt, $3)      }
  | expr LE     expr { Binop($1, Le, $3)      }
  | expr GE     expr { Binop($1, Ge, $3)      }
  | expr AND    expr { Binop($1, And,   $3)   }
  | expr OR     expr { Binop($1, Or,    $3)   }
  | expr MMULT  expr { Binop($1, Mmult, $3)   }
  | NOT         expr          { Uop(Not, $2)    }
  | MINUS expr %prec NEG      { Uop(Neg, $2)    }
  | ID ASSIGN expr            { Assign($1, $3)  }
  | L_PAREN expr R_PAREN      { $2              }
  /* call */
  | ID L_PAREN args_opt R_PAREN { Call ($1, $3)  }
  /* array */
  | ID arr_index ASSIGN expr                { Arr1DAssign($1, $2, $4)       }    
  | ID arr_index arr_index ASSIGN expr      { Arr2DAssign($1, $2, $3, $5 )  }
  | ID arr_index                            { Arr1DAccess($1, $2)           }
  | ID arr_index arr_index                  { Arr2DAccess($1, $2, $3)       }
  | LENGTH L_PAREN expr R_PAREN             { ArrUop(Length, $3)            }
  | TRANSPOSE L_PAREN expr R_PAREN          { ArrUop(Transpose, $3)         }
  | MAP L_PAREN expr COMMA expr R_PAREN     { ArrOp(Map, $3, $5)            }
  | REDUCE L_PAREN expr COMMA expr R_PAREN  { ArrOp(Reduce, $3, $5)         }
  | ID L_SQBRACE expr COLON expr R_SQBRACE  { Arr1DSlice($1, $3, $5)        }
  | ID L_SQBRACE expr COLON expr R_SQBRACE L_SQBRACE expr COLON expr R_SQBRACE  { Arr2DSlice($1, $3, $5, $8, $10) }

/* args_opt*/
args_opt:
  /*nothing*/ { [] }
  | args { $1 }

args:
  expr  { [$1] }
  | expr COMMA args { $1::$3 }
