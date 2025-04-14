open Ast

type sexpr = typ * sx
and sx =
  | SIntLit of int
  | SDoubleLit of float
  | SBoolLit of bool
  | SCharLit of char
  | SStringLit of string
  | SArr1DLit of sexpr list
  | SArr2DLit of sexpr list list
  | SId of string
  | SBinop of sexpr * bop * sexpr
  | SUop of uop * sexpr
  | SAssign of string * sexpr
  | SCall of string * sexpr list
  | SArr1DAssign of string * sexpr * sexpr           (* string: array id, expr: index, expr: value*)
  | SArr2DAssign of string * sexpr * sexpr * sexpr   (* string: array id, expr: row index, expr: col index, expr: value *) 
  | SArr1DAccess of string * sexpr                   (* string: array id, expr: index *)
  | SArr2DAccess of string * sexpr * sexpr           (* string: array id, expr: row index, expr: col index *)
  | SArrUop of arruop * sexpr                        (* arruop, expr: array *)
  | SArrOp of arrop * sexpr * string                 (* arrop, expr: array, string: function ptr id*)
  | SArr1DSlice of string * int * int                (* string: array id, expr: start index, expr: end index *)
  | SArr2DSlice of string * int * int * int * int    (* string: array id, expr: start row index, expr: end row index, expr: start col index, expr: end col index *)
  | SNoExpr                                          (* for empty else statements *)

  
type sbind_decl = typ * string
type sbind_init = sbind_decl * sexpr

type sbind =
  SBindDecl of sbind_decl
| SBindInit of sbind_init

type sstmt =
  | SBlock of sstmt list
  | SExpr of sexpr
  | SIf of sexpr * sstmt * sstmt
  | SFor of sstmt * sexpr * sexpr * sstmt
  | SWhile of sexpr * sstmt
  | SReturn of sexpr
  | SBreak
  | SVDecl of sbind


(* func_def: ret_typ fname formals locals body *)
type sfunc_def = {
  srtyp: typ;
  sfname: string;
  sformals: sbind_decl list; (* parameters *)
  sbody: sstmt list;
}

type sprogram = sbind list * sfunc_def list

(* Pretty-printing functions *)
let string_of_sop = function
    Add -> "+"
  | Sub -> "-"
  | Equal -> "=="
  | Neq -> "!="
  | Lt -> "<"
  | Gt -> ">"
  | Le -> "<="
  | Ge -> ">="
  | And -> "&&"
  | Or -> "||"
  | Mult -> "*"
  | Div -> "/"
  | Mod -> "%"
  | Pow -> "^"
  | Mmult -> "@"

let string_of_suop = function
    Not -> "!"
  | Neg -> "-"

let string_of_sarrop = function
    Map -> "map"
  | Reduce -> "reduce"

let string_of_sarruop = function
    Length -> "length"
  | Transpose -> "transpose"

  let rec string_of_sexpr (t, e) = 
  "(" ^ string_of_typ t ^ " : " ^ (
    match e with
   SIntLit(l) -> string_of_int l
  | SDoubleLit(l) -> string_of_float l
  | SBoolLit(true) -> "true"
  | SBoolLit(false) -> "false"
  | SCharLit(c) -> String.make 1 c
  | SStringLit(s) -> "\"" ^ s ^ "\""
  | SArr1DLit(a) -> 
      "[" ^ String.concat ", " (List.map string_of_sexpr a) ^ "]"
  | SArr2DLit(a) ->
      "[" ^ String.concat " " (List.map (fun row -> 
          String.concat ", " (List.map string_of_sexpr row) ^ ";"
      ) a) ^ "]"
  | SId(s) -> s
  | SBinop(e1, o, e2) ->
      string_of_sexpr e1 ^ " " ^ string_of_sop o ^ " " ^ string_of_sexpr e2
  | SUop(o, e) ->
      string_of_suop o ^ " " ^ string_of_sexpr e
  | SAssign(v, e) -> v ^ " = " ^ string_of_sexpr e
  | SCall(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_sexpr el) ^ ")"
  | SArrOp(o, s1, s2) -> 
      string_of_sarrop o ^ "(" ^ string_of_sexpr s1 ^ ", " ^ s2 ^ ")"
  | SArrUop(o, a) -> 
      string_of_sarruop o ^ "(" ^ string_of_sexpr a ^ ")"
  | SArr1DAssign(a, idx, value) -> 
      a ^ "[" ^ string_of_sexpr idx ^ "] = " ^ string_of_sexpr value
  | SArr2DAssign(a, idx1, idx2, value) -> 
      a ^ "[" ^ string_of_sexpr idx1 ^ "][" ^ string_of_sexpr idx2 ^ "] = " ^ string_of_sexpr value
  | SArr1DAccess(a, idx) -> 
      a ^ "[" ^ string_of_sexpr idx ^ "]"
  | SArr2DAccess(a, idx1, idx2) -> 
      a ^ "[" ^ string_of_sexpr idx1 ^ "][" ^ string_of_sexpr idx2 ^ "]"
  | SArr1DSlice(a, start_idx, end_idx) ->
      a ^ "[" ^ string_of_int start_idx ^ ":" ^ string_of_int end_idx ^ "]"
  | SArr2DSlice(a, start_row, end_row, start_col, end_col) ->
      a ^ "[" ^ string_of_int start_row ^ ":" ^ string_of_int end_row ^ ", " 
              ^ string_of_int start_col ^ ":" ^ string_of_int end_col ^ "]"
  | SNoExpr -> ""
  ) ^ ")"


let string_of_svdecl = function
SBindDecl(sdecl) -> string_of_typ (fst sdecl) ^ " " ^ (snd sdecl) ^ ";\n"
| SBindInit(sdecl, sexpr) -> string_of_typ (fst sdecl) ^ " " ^ (snd sdecl) ^ " = " ^ string_of_sexpr sexpr ^ ";\n"


let rec string_of_sstmt = function
    SBlock(sstmts) ->
    "{\n" ^ String.concat "" (List.map string_of_sstmt sstmts) ^ "}\n"
  | SExpr(sexpr) -> string_of_sexpr sexpr ^ ";\n"
  | SIf(e, s1, s2) ->  "if (" ^ string_of_sexpr e ^ ")\n" ^
                      string_of_sstmt s1 ^ "else\n" ^ string_of_sstmt s2
  | SWhile(e, s) -> "while (" ^ string_of_sexpr e ^ ") " ^ string_of_sstmt s
  | SFor(s1, e1, e2, s2) -> "for (" ^ string_of_sstmt s1 ^ string_of_sexpr e1 ^ "; " ^ string_of_sexpr e2 ^ ") " ^ string_of_sstmt s2
  | SReturn(sexpr) -> "return " ^ string_of_sexpr sexpr ^ ";\n"
  | SBreak -> "break;\n"
  | SVDecl(svdecl) -> string_of_svdecl svdecl

let string_of_sfdecl sfdecl =
  string_of_typ sfdecl.srtyp ^ " " ^
  sfdecl.sfname ^ "(" ^ String.concat ", " (List.map snd sfdecl.sformals) ^
  ")\n{\n" ^
  (* String.concat "" (List.map string_of_svdecl sfdecl.locals) ^ *)
  String.concat "" (List.map string_of_sstmt sfdecl.sbody) ^
  "}\n"


  let string_of_sprogram (vars, funcs) =
    "\n\nSemantically-checked program: \n\n" ^
    String.concat "" (List.map string_of_svdecl vars) ^ "\n" ^
    String.concat "\n" (List.map string_of_sfdecl funcs)
