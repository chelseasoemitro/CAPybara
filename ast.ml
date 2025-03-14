(* type tokenseq = string list

let string_of_program l =
  "\n Scanned program: \n" ^ (List.fold_left (fun s e -> s ^ "\n" ^ e) " " l) *)


type bop = Add | Sub | Mult | Div | Mod | Pow | 
            Equal | Neq | Lt | Gt | Le | Ge | 
            And | Or | 
            Mmult

type uop = Not | Neg

type arrop = Map | Reduce

type arruop = Length | Transpose

type typ = Int | Double | Bool | Char | String | Arr of typ * int list (* int list: list of dimensions *) | Void

type expr =
  | IntLit of int
  | DoubleLit of float
  | BoolLit of bool
  | CharLit of char
  | StringLit of string
  | ArrLit of expr list
  | Id of string
  | Binop of expr * bop * expr
  | Uop of uop * expr
  | Assign of string * expr
  | Call of string * expr list
  | ArrAssign of string * expr list * expr      (* string: array id, expr list: list of indices, expr: value *)
  | ArrAccess of string * expr list             (* string: array id, expr list: list of indices *)
  | ArrUop of arruop * string 
  | ArrOp of arrop * string * string
  | ArrSlice of string * (expr * expr) list     (* string: array id, expr * expr list: of (start:end) pairs for slicing *)
  | NoExpr                                      (* is this necessary? *)

  
type stmt =
  | Block of stmt list
  | Expr of expr
  | If of expr * stmt * stmt
  | For of expr * expr * expr * stmt 
  | While of expr * stmt
  | Return of expr
  | Break


type bind = typ * string

(* func_def: ret_typ fname formals locals body *)
type func_def = {
  rtyp: typ;
  fname: string;
  formals: bind list;
  locals: bind list;
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
    IntLit(l) -> string_of_int l
  | DoubleLit(l) -> string_of_float l
  | BoolLit(true) -> "true"
  | BoolLit(false) -> "false"
  | CharLit(c) -> String.make 1 c
  | StringLit(s) -> s
  | ArrLit(a) -> 
    "[" ^ String.concat ", " (List.map string_of_expr a) ^ "]"
  | Id(s) -> s
  | Binop(e1, o, e2) ->
    string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Uop(o, e) ->
    string_of_uop o ^ " " ^ string_of_expr e
  | Assign(v, e) -> v ^ " = " ^ string_of_expr e
  | Call(f, el) ->
    f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | ArrOp(o, s1, s2) -> string_of_arrop o ^ "(" ^ s1 ^ ", " ^ s2 ^ ")" 
  | ArrUop(o, a) -> string_of_arruop o ^ "(" ^ a ^ ")"
  | ArrAssign (a, indices, value) -> 
      a ^ "[" ^ 
      String.concat ", " (List.map string_of_expr indices) ^ "] = " ^ string_of_expr value
  | ArrAccess (a, indices) -> 
      a ^ "[" ^ 
      String.concat ", " (List.map string_of_expr indices) ^ "]"
  | ArrSlice (arr_name, slices) -> 
      arr_name ^ "[" ^ 
      String.concat ", " 
        (List.map (fun (s, e) -> string_of_expr s ^ ":" ^ string_of_expr e) slices) 
      ^ "]"
  | NoExpr -> ""


let rec string_of_stmt = function
    Block(stmts) ->
    "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n";
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
                      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s
  | For(e1, e2, e3, s) -> "for (" ^ string_of_expr e1 ^ ";" ^ string_of_expr e2 ^ "; " ^ string_of_expr e3 ^ ") " ^ string_of_stmt s
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n"
  | Break -> "break"

  
let rec string_of_typ = function
    Int -> "int"
  | Double -> "double"
  | Bool -> "bool"
  | Char -> "char"
  | String -> "string"
  | Arr (t, i) -> 
      let t_str = string_of_typ t in
        let i_str =
          i
          |> List.map string_of_int
          |> String.concat "]["
        in
        t_str ^ "[" ^ i_str ^  "]"
  | Void -> "void"

  
let string_of_vdecl (t, id) = string_of_typ t ^ " " ^ id ^ ";\n"

let string_of_fdecl fdecl =
  string_of_typ fdecl.rtyp ^ " " ^
  fdecl.fname ^ "(" ^ String.concat ", " (List.map snd fdecl.formals) ^
  ")\n{\n" ^
  String.concat "" (List.map string_of_vdecl fdecl.locals) ^
  String.concat "" (List.map string_of_stmt fdecl.body) ^
  "}\n"

let string_of_program (vars, funcs) =
  "\n\nParsed program: \n\n" ^
  String.concat "" (List.map string_of_vdecl vars) ^
  String.concat "" (List.map string_of_stmt funcs) ^
  "\n"
