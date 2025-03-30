type bop = Add | Sub | Mult | Div | Mod | Pow | 
            Equal | Neq | Lt | Gt | Le | Ge | 
            And | Or | 
            Mmult

type uop = Not | Neg

type arrop = Map | Reduce

type arruop = Length | Transpose

type typ = Int | Double | Bool | Char | String | Arr1D of typ * int | Arr2D of typ * int * int | Void

type expr =
  | IntLit of int
  | DoubleLit of float
  | BoolLit of bool
  | CharLit of char
  | StringLit of string
  | Arr1DLit of expr list
  | Arr2DLit of expr list list
  | Id of string
  | Binop of expr * bop * expr
  | Uop of uop * expr
  | Assign of string * expr
  | Call of string * expr list
  | Arr1DAssign of string * expr * expr               (* string: array id, expr: index, expr: value*)
  | Arr2DAssign of string * expr * expr * expr        (* string: array id, expr: row index, expr: col index, expr: value *) 
  | Arr1DAccess of string * expr                      (* string: array id, expr list: list of indices *)
  | Arr2DAccess of string * expr * expr               (* string: array id, expr list: list of indices *)
  | ArrUop of arruop * expr                           (* arruop, expr: array *)
  | ArrOp of arrop * expr * string                    (* arrop, expr: array, string: function ptr id*)
  | Arr1DSlice of string * expr * expr                (* string: array id, expr: start index, expr: end index *)
  | Arr2DSlice of string * expr * expr * expr * expr  (* string: array id, expr: start row index, expr: end row index, expr: start col index, expr: end col index *)
  | NoExpr                                            (* is this necessary? maybe for empty else statements *)

type bind_decl = typ * string
type bind_init = bind_decl * expr

type bind =
  BindDecl of bind_decl
| BindInit of bind_init


type stmt =
  | Block of stmt list
  | Expr of expr
  | If of expr * stmt * stmt
  | For of expr * expr * expr * stmt
  | While of expr * stmt
  | Return of expr
  | Break
  | VDecl of bind


(* func_def: ret_typ fname formals locals body *)
type func_def = {
  rtyp: typ;
  fname: string;
  formals: bind_decl list; (* parameters *)
  body: stmt list;
}

type program = bind list * func_def list

(* Pretty-printing functions *)
let string_of_op = function
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

let string_of_uop = function
    Not -> "!"
  | Neg -> "-"

let string_of_arrop = function
    Map -> "map"
  | Reduce -> "reduce"

let string_of_arruop = function
    Length -> "length"
  | Transpose -> "transpose"

  let rec string_of_expr = function
  | IntLit(l) -> string_of_int l
  | DoubleLit(l) -> string_of_float l
  | BoolLit(true) -> "true"
  | BoolLit(false) -> "false"
  | CharLit(c) -> String.make 1 c
  | StringLit(s) -> "\"" ^ s ^ "\""
  | Arr1DLit(a) -> 
      "[" ^ String.concat ", " (List.map string_of_expr a) ^ "]"
  | Arr2DLit(a) ->
      "[" ^ String.concat ", " (List.map (fun row -> 
          String.concat ", " (List.map string_of_expr row) ^ ";"
      ) a) ^ "]"
  | Id(s) -> s
  | Binop(e1, o, e2) ->
      string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Uop(o, e) ->
      string_of_uop o ^ " " ^ string_of_expr e
  | Assign(v, e) -> v ^ " = " ^ string_of_expr e
  | Call(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | ArrOp(o, s1, s2) -> 
      string_of_arrop o ^ "(" ^ string_of_expr s1 ^ ", " ^ s2 ^ ")"
  | ArrUop(o, a) -> 
      string_of_arruop o ^ "(" ^ string_of_expr a ^ ")"
  | Arr1DAssign(a, idx, value) -> 
      a ^ "[" ^ string_of_expr idx ^ "] = " ^ string_of_expr value
  | Arr2DAssign(a, idx1, idx2, value) -> 
      a ^ "[" ^ string_of_expr idx1 ^ "][" ^ string_of_expr idx2 ^ "] = " ^ string_of_expr value
  | Arr1DAccess(a, idx) -> 
      a ^ "[" ^ string_of_expr idx ^ "]"
  | Arr2DAccess(a, idx1, idx2) -> 
      a ^ "[" ^ string_of_expr idx1 ^ "][" ^ string_of_expr idx2 ^ "]"
  | Arr1DSlice(a, start_idx, end_idx) ->
      a ^ "[" ^ string_of_expr start_idx ^ ":" ^ string_of_expr end_idx ^ "]"
  | Arr2DSlice(a, start_row, end_row, start_col, end_col) ->
    a ^ "[" ^ string_of_expr start_row ^ ":" ^ string_of_expr end_row ^ ", " 
            ^ string_of_expr start_col ^ ":" ^ string_of_expr end_col ^ "]"
  | NoExpr -> ""


let rec string_of_typ = function
    Int -> "int"
  | Double -> "double"
  | Bool -> "bool"
  | Char -> "char"
  | String -> "string"
  | Arr1D (t, i) ->
    let t_str = string_of_typ t in
    t_str ^ "[" ^ string_of_int i ^ "]"
  | Arr2D (t, i1, i2) ->
    let t_str = string_of_typ t in
    t_str ^ "[" ^ string_of_int i1 ^ "][" ^ string_of_int i2 ^ "]"
  | Void -> "void"


let string_of_vdecl = function
BindDecl(decl) -> string_of_typ (fst decl) ^ " " ^ (snd decl) ^ ";\n"
| BindInit(decl, exp) -> string_of_typ (fst decl) ^ " " ^ (snd decl) ^ " = " ^ string_of_expr exp ^ ";\n"


let rec string_of_stmt = function
    Block(stmts) ->
    "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n"
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
                      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s
  | For(e1, e2, e3, s) -> "for (" ^ string_of_expr e1 ^ ";" ^ string_of_expr e2 ^ "; " ^ string_of_expr e3 ^ ") " ^ string_of_stmt s
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n"
  | Break -> "break"
  | VDecl(vdecl) -> string_of_vdecl vdecl

let string_of_fdecl fdecl =
  string_of_typ fdecl.rtyp ^ " " ^
  fdecl.fname ^ "(" ^ String.concat ", " (List.map snd fdecl.formals) ^
  ")\n{\n" ^
  (* String.concat "" (List.map string_of_vdecl fdecl.locals) ^ *)
  String.concat "" (List.map string_of_stmt fdecl.body) ^
  "}\n"


  let string_of_program (vars, funcs) =
    "\n\nParsed program: \n\n" ^
    String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
    String.concat "\n" (List.map string_of_fdecl funcs)
