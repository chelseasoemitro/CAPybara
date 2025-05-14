(* Semantic checking for the CAPybara compiler *)

open Ast
open Sast

module StringMap = Map.Make(String)

(* Semantic checking of the AST. Returns an SAST if successful,
   throws an exception if something is wrong.

   Check each global variable, then check each function *)

let check (globals, functions) =

  (* Ensure global variables do not bind to void type *)
  let check_not_void exceptf = function
    (Void, n) -> raise (Failure (exceptf n))
    | _ -> ()
  in

  (* Verify a list of bindings has no duplicate names *)
  let check_binds (kind : string) (binds : (typ * string) list) =
    List.iter (check_not_void (fun n -> "illegal void variable: " ^ n)) binds;
    let rec dups = function
        [] -> ()
      |	((_,n1) :: (_,n2) :: _) when n1 = n2 ->
        raise (Failure ("duplicate " ^ kind ^ " " ^ n1))
      | _ :: t -> dups t
    in dups (List.sort (fun (_,a) (_,b) -> compare a b) binds)
  in

  (* Raise an exception if the given rvalue type cannot be assigned to
    the given lvalue type *)
  let check_assign lvaluet rvaluet err =
    if lvaluet <> rvaluet then (raise (Failure err));
    lvaluet
  in

  (* Return a variable from our local symbol table *)
  let type_of_identifier s symtab =
    try StringMap.find s symtab
    with Not_found -> raise (Failure ("undeclared identifier " ^ s))
  in

  (* Extract binds from global vars. We turn them to sbinds at the end. *)
  let rec extract_global_binds = function
    [] -> []
  | hd::tl -> match hd with
    BindDecl(d) -> d::extract_global_binds tl
    | BindInit(d, e) -> d::extract_global_binds tl
  in

  (* Build list of global binds for symbol table *)
  let global_binds = extract_global_binds globals in

  (* Check global variables: make sure no globals duplicate, make sure bind inits are good *)
  check_binds "global" (global_binds);
 
  (* Collect function declarations for built-in functions: no bodies *)
  let built_in_decls =
    let add_built_in map (name, ty) = StringMap.add name {
      rtyp = Void;
      fname = name;
      formals = [(ty, "x")];
      body = []
    } map
    in List.fold_left add_built_in StringMap.empty [
      ("print_int", Int);
      ("print_double", Double);
      ("print_char", Char);
      ("print_bool", Bool);
      ("print_str", String);
      (* ("print_arr", List) -> can't do this bc don't know size of array *)
    ] 
  in

  (* Add function name to symbol table *)
  let add_func map fd =
    let built_in_err = "function " ^ fd.fname ^ " may not be defined"
    and dup_err = "duplicate function " ^ fd.fname
    and make_err er = raise (Failure er)
    and n = fd.fname (* Name of the function *)
    in match fd with (* No duplicate functions or redefinitions of built-ins *)
      _ when StringMap.mem n built_in_decls -> make_err built_in_err
    | _ when StringMap.mem n map -> make_err dup_err
    | _ ->  StringMap.add n fd map
  in

  (* Collect all function names into one symbol table *)
  let function_decls = List.fold_left add_func built_in_decls functions
  in

  (* Return a function from our symbol table *)
  let find_func s =
    try StringMap.find s function_decls
    with Not_found -> raise (Failure ("unrecognized function " ^ s))
  in

  let _ = find_func "main" in (* Ensure "main" is defined *)

  (* Return a semantically-checked expression, i.e., with a type *)
  let rec check_expr e symtab = 
    match e with
    | IntLit l -> (Int, SIntLit l)
    | DoubleLit l -> (Double, SDoubleLit l)
    | BoolLit l -> (Bool, SBoolLit l)
    | CharLit l -> (Char, SCharLit l)
    | StringLit l -> (String, SStringLit l)
    | Arr1DLit l ->   
      let checked_elements = List.map (fun x -> check_expr x symtab) l in
      (match checked_elements with
      | [] -> raise (Failure "1D array cannot be empty")
      | (typ, _) :: tail -> 
        if (match typ with Arr1D _ | Arr2D _ -> true | _ -> false) then
          raise (Failure "1D arrays cannot contain nested arrays") 
        else if List.for_all (fun (typ', _) -> typ' = typ) tail then
          (Arr1D (typ, List.length l), SArr1DLit checked_elements)
        else
          raise (Failure ("All elements in 1D array must be the same type: " ^ string_of_expr e)))
    | Arr2DLit l -> 
      let num_rows = List.length l in
      let row_length = List.length (List.hd l) in 
      if not (List.for_all (fun row -> List.length row = row_length) l) then
        raise (Failure ("All rows in a 2D array must have the same length: " ^ string_of_expr e))
      else
        let checked_rows =
          List.map (fun row -> List.map (fun x -> check_expr x symtab) row) l in 
        (match checked_rows with
          | [] -> raise (Failure "2D array cannot be empty")
          | ( (first_typ, _) :: _ ) :: _ ->
            if (match first_typ with Arr1D _ | Arr2D _ -> true | _ -> false) then
              raise (Failure "2D arrays cannot contain nested arrays")
            else if List.for_all (fun row -> List.for_all (fun (t, _) -> t = first_typ) row) checked_rows then
              (Arr2D (first_typ, num_rows, row_length), SArr2DLit checked_rows)
            else
              raise (Failure ("All elements of a 2D array must be of the same type" ^ string_of_expr e))
          | _ -> raise (Failure "poop"))
    | Id var -> (type_of_identifier var symtab, SId var)
    | Assign(var, e) as ex ->
      let lt = type_of_identifier var symtab
      and (rt, e') = check_expr e symtab in
      let err = "illegal assignment " ^ string_of_typ lt ^ " = " ^
                string_of_typ rt ^ " in " ^ string_of_expr ex
      in
      (check_assign lt rt err, SAssign(var, (rt, e')))
    | Call(fname, args) as call ->
      let fd = find_func fname in
      let param_length = List.length fd.formals in
      if List.length args != param_length then
        raise (Failure ("expecting " ^ string_of_int param_length ^
                        " arguments in " ^ string_of_expr call))
      else let check_call (ft, _) e =
              let (et, e') = check_expr e symtab in
              let err = "illegal argument found " ^ string_of_typ et ^
                        " expected " ^ string_of_typ ft ^ " in " ^ string_of_expr e
              in (check_assign ft et err, e')
        in
        let args' = List.map2 check_call fd.formals args
        in (fd.rtyp, SCall(fname, args'))
    | Uop(op, e1) as e ->
      let (t1, e1') = check_expr e1 symtab in
      let t = (match op with
          Neg when t1 = Int || t1 = Double -> t1
        | Not when t1 = Bool -> Bool
        | _ -> raise (Failure ("illegal unary operator " ^
                              string_of_uop op ^ " on " ^ string_of_typ t1 ^
                              " in " ^ string_of_expr e)))
      in (t, SUop(op, (t1, e1'))) 
    | Binop(e1, op, e2) as e ->
      let (t1, e1') = check_expr e1 symtab
      and (t2, e2') = check_expr e2 symtab in
      let err = "illegal binary operator " ^
                string_of_typ t1 ^ " " ^ string_of_op op ^ " " ^
                string_of_typ t2 ^ " in " ^ string_of_expr e
      in
      (* All binary operators except Mmult require operands of the same type*)
      (match op with
        | Mmult ->
          (match (t1, t2) with
            | Arr2D (t1_elem, r1, c1), Arr2D (t2_elem, r2, c2) ->
              if t1_elem <> t2_elem then
                raise (Failure ("matrix elements must be same type for matrix multiplication: " ^ 
                                string_of_typ t1_elem ^ " vs " ^ string_of_typ t2_elem))
              else if (t1_elem <> Int && t1_elem <> Double) then
                raise (Failure ("matrix elements must be Int or Double for matrix multiplication, got: " ^
                                string_of_typ t1_elem))
              else if c1 <> r2 then
                raise (Failure ("matrix dimensions mismatch for matrix multiplication: " ^
                                Printf.sprintf "(%d x %d) * (%d x %d)" r1 c1 r2 c2))
              else
                (Arr2D (t1_elem, r1, c2), SBinop((t1, e1'), op, (t2, e2')))
            | _ -> raise (Failure ("Matrix multiplication only supported for 2D arrays (matrices), got: " ^
                            string_of_typ t1 ^ " and " ^ string_of_typ t2)))
        | _ ->
          if t1 = t2 then
            (* Determine expression type based on operator and operand types *)
            let t =
              (match op, t1 with
              | (Add | Sub | Mult | Div | Mod), Int ->
                  Int
              | (Add | Sub | Mult | Div | Mod), Double ->
                  Double
              | (Add | Sub | Mult | Div ), Arr1D(Int, n) ->
                  Arr1D(Int, n)
              | (Add | Sub | Mult | Div ), Arr1D(Double, n) ->
                  Arr1D(Double, n)
              | (Add | Sub | Mult | Div ), Arr2D(Int, m, n) ->
                  Arr2D(Int, m, n)
              | (Add | Sub | Mult | Div ), Arr2D(Double, m, n) ->
                  Arr2D(Double, m, n)
              | (Equal | Neq), _ ->
                  Bool
              | ((Le | Lt | Ge | Gt), (Int | Double)) ->
                  Bool
              (* Can include these too:
                | ((Le | Lt | Ge | Gt), Arr1D(Int, n)) -> 
                  Arr1D(Bool, n)
              | ((Le | Lt | Ge | Gt), Arr1D(Double, n)) ->
                  Arr1D(Bool, n)
              | ((Le | Lt | Ge | Gt), Arr2D(Int, m, n)) ->
                    Arr2D(Bool, m, n)
              | ((Le | Lt | Ge | Gt), Arr2D(Double, m, n)) ->
                    Arr2D(Bool, m, n) *)
              | (And | Or), Bool ->
                  Bool
              (* | ((And | Or), Arr1D(Bool, n)) ->
                Arr1D(Bool, n)
              | ((And | Or), Arr2D(Bool, m, n)) ->
                Arr2D(Bool, m, n) *)
              | _ -> raise (Failure err))
            in
            (t, SBinop((t1, e1'), op, (t2, e2')))
          else if ((match (t1, t2) with
            | (Int, Arr1D(Int, _))
            | (Arr1D(Int, _), Int)
            | (Int, Arr2D(Int, _, _))
            | (Arr2D(Int, _, _), Int)
            | (Double, Arr1D(Double, _))
            | (Arr1D(Double, _), Double)
            | (Double, Arr2D(Double, _, _))
            | (Arr2D(Double, _, _), Double) -> true
            | _ -> false)
          &&
          op = Mult)
          then
            let t =
              (match t1, t2 with
              | (Int, (Arr1D(Int, n))) | ((Arr1D(Int, n)), Int) ->
                  Arr1D(Int, n)
              | (Int, (Arr2D(Int, m, n))) | ((Arr2D(Int, m, n)), Int) ->
                  Arr2D(Int, m, n)
              | (Double, (Arr1D(Double, n))) | ((Arr1D(Double, n)), Double) ->
                  Arr1D(Double, n)
              | (Double, (Arr2D(Double, m, n))) | ((Arr2D(Double, m, n)), Double) ->
                  Arr2D(Double, m, n)
              | _ -> raise (Failure err))
            in (t, SBinop((t1, e1'), op, (t2, e2')))
          else
            raise (Failure err)
        )
    | Arr1DAssign(id, index_expr, value_expr) ->  (* id[index_expr] = value_expr *)
      let arr_typ =
        try StringMap.find id symtab
        with Not_found -> raise (Failure ("undeclared array identifier: " ^ id))
      in
      (match arr_typ with
        | Arr1D (elem_typ, _) ->
          let (index_type, index_s) = check_expr index_expr symtab in
          let (value_type, value_s) = check_expr value_expr symtab in
          if index_type <> Int then
            raise (Failure ("array index must be of type int, got " ^
                          string_of_typ index_type ^ " in " ^ string_of_expr index_expr));
          let _ = check_assign elem_typ value_type
            ("array assignment type mismatch: expected " ^ string_of_typ elem_typ ^
                ", got " ^ string_of_typ value_type ^ " in " ^ string_of_expr value_expr)
          in (elem_typ, SArr1DAssign (id, (index_type, index_s), (value_type, value_s)))
        | _ -> raise (Failure (id ^ " is not a 1D array (Arr1DAssign)")))
    | Arr2DAssign(id, row_expr, col_expr, value_expr) -> (* id[row_expr][col_expr] = value_expr *)
      let arr_typ =
        try StringMap.find id symtab
        with Not_found -> raise (Failure ("undeclared array identifier: " ^ id))
      in
      (match arr_typ with
        | Arr2D (elem_typ, _, _) ->
          let (row_type, row_s) = check_expr row_expr symtab in
          let (col_type, col_s) = check_expr col_expr symtab in
          let (value_type, value_s) = check_expr value_expr symtab in

          if row_type <> Int || col_type <> Int then
            raise (Failure (
              "array indices must be of type int, got: [" ^
              string_of_typ row_type ^ ", " ^ string_of_typ col_type ^ "] " ^
              "in " ^ string_of_expr row_expr ^ " and " ^ string_of_expr col_expr
            ));

          let _ = check_assign elem_typ value_type
            ("matrix assignment type mismatch: expected " ^ string_of_typ elem_typ ^
            ", got " ^ string_of_typ value_type ^ " in " ^ string_of_expr value_expr)
          in (elem_typ, SArr2DAssign (id,
                                      (row_type, row_s),
                                      (col_type, col_s),
                                      (value_type, value_s)))
        | _ -> raise (Failure (id ^ " is not a 2D array")))
    | Arr1DAccess(id, index_expr) -> (* id[index_expr] *)
      let arr_typ =
        try StringMap.find id symtab
        with Not_found -> raise (Failure ("undeclared array identifier: " ^ id))
      in
      (match arr_typ with
        | Arr1D (elem_typ, _) ->
          let (index_t, index_s) = check_expr index_expr symtab in
          if index_t <> Int then
            raise (Failure ("array index must be of type int, got " ^
                            string_of_typ index_t ^ " in " ^ string_of_expr index_expr));
          (elem_typ, SArr1DAccess (id, (index_t, index_s)))
        (* Arr2D accesses will be parsed as a 1D access, it's the same syntax *)
        | Arr2D (elem_typ, _, n) ->
          let (index_t, index_s) = check_expr index_expr symtab in
          if index_t <> Int then
            raise (Failure ("array index must be of type int, got " ^
                            string_of_typ index_t ^ " in " ^ string_of_expr index_expr));
          (Arr1D (elem_typ, n) , SArr1DAccess (id, (index_t, index_s))) (* returns a 1D array *)
        | _ -> raise (Failure (id ^ " is not an array (Arr1DAccess)")))
    | Arr2DAccess(id, row_expr, col_expr) -> (* id[row_expr][col_expr] *)
      let arr_typ =
        try StringMap.find id symtab
        with Not_found -> raise (Failure ("undeclared array identifier: " ^ id))
      in
      (match arr_typ with
        | Arr2D (elem_typ, _, _) ->
          let (row_type, row_s) = check_expr row_expr symtab in
          let (col_type, col_s) = check_expr col_expr symtab in
          if row_type <> Int || col_type <> Int then
            raise (Failure (
              "array indices must be of type int, got: [" ^
              string_of_typ row_type ^ ", " ^ string_of_typ col_type ^ "] " ^
              "in " ^ string_of_expr row_expr ^ " and " ^ string_of_expr col_expr
            ));
          (elem_typ, SArr2DAccess (id, (row_type, row_s), (col_type, col_s)))
        | _ -> raise (Failure (id ^ " is not a 2D array")))
    | ArrUop(op, arr_expr) -> 
        let (arr_typ, arr_s) = check_expr arr_expr symtab in
        (match op, arr_typ with
        | Length, Arr1D (_, _) | Length, Arr2D (_, _, _) ->
            (Int, SArrUop (op, (arr_typ, arr_s)))
        | Transpose, Arr2D (elem_typ, rows, cols) ->
            (Arr2D (elem_typ, cols, rows), SArrUop (op, (arr_typ, arr_s)))
        | Length, _ ->
            raise (Failure ("'length' applied to non-array type: " ^ string_of_typ arr_typ))
        | Transpose, _ ->
            raise (Failure ("'transpose' applied to non-2D array type: " ^ string_of_typ arr_typ)))
    | ArrOp(op, arr_expr, func_name) ->
      let (arr_typ, arr_s) = check_expr arr_expr symtab in
      let fd = find_func func_name in
        (match op, arr_typ with
        | Map, Arr1D (elem_typ, n) ->
          (match fd.formals, fd.rtyp with
          | [(arg_typ, _)], ret_typ when arg_typ = elem_typ ->
              (Arr1D (ret_typ, n), SArrOp (op, (arr_typ, arr_s), func_name))
          | _ ->
              raise (Failure (
                "map function must take argument of type " ^ string_of_typ elem_typ
              )))
        | Map, Arr2D (elem_typ, m, n) ->
          (match fd.formals, fd.rtyp with
          | [(arg_typ, _)], ret_typ when arg_typ = elem_typ ->
              (Arr2D (ret_typ, m, n), SArrOp (op, (arr_typ, arr_s), func_name))
          | _ ->
              raise (Failure (
                "map function must take one argument of type " ^ string_of_typ elem_typ
              )))
        | Reduce, Arr1D (elem_typ, n) ->
          (match fd.formals, fd.rtyp with
          | [(t1, _); (t2, _)], ret_typ when t1 = elem_typ && t2 = elem_typ && ret_typ = elem_typ ->
              (elem_typ, SArrOp (op, (arr_typ, arr_s), func_name))
          | _ ->
              raise (Failure (
                "reduce function must take two arguments of type " ^ string_of_typ elem_typ ^
                " and return " ^ string_of_typ elem_typ ^ " in reduce(" ^ string_of_typ arr_typ ^ ", " ^ func_name ^ ")"
              )))
        | Reduce, Arr2D (elem_typ, m, n) ->
          (match fd.formals, fd.rtyp with
          | [(t1, _); (t2, _)], ret_typ when t1 = elem_typ && t2 = elem_typ && ret_typ = elem_typ ->
              (Arr1D (elem_typ, m), SArrOp (op, (arr_typ, arr_s), func_name))
          | _ ->
              raise (Failure (
                "reduce function must take two arguments of type " ^ string_of_typ elem_typ ^
                " and return " ^ string_of_typ elem_typ ^ " in reduce(" ^ string_of_typ arr_typ ^ ", " ^ func_name ^ ")"
              )))
        | Map, _ | Reduce, _ ->
            raise (Failure (
              string_of_arrop op ^ " expects an array operand, got " ^ string_of_typ arr_typ ^
              " in " ^ string_of_expr e))
        )
    | Arr1DSlice(id, s, e) -> (* id[s:e] *)
      let arr_typ =
        try StringMap.find id symtab
        with Not_found -> raise (Failure ("undeclared array identifier: " ^ id))
      in
      (match arr_typ with
        | Arr1D (elem_typ, orig_len) ->
          if s < 0 || e <= 0 || s >= e || e > orig_len then
            raise (Failure (
              "invalid slice bounds [" ^ string_of_int s ^ ":" ^ string_of_int e ^
              "] for array '" ^ id ^ "' of length " ^ string_of_int orig_len
            ))
          else
            let slice_len = e - s in
            (Arr1D (elem_typ, slice_len), SArr1DSlice (id, s, e))
        | _ ->
          raise (Failure (id ^ " is not a 1D array and cannot be sliced")))
    | Arr2DSlice(id, s1, e1, s2, e2) -> (* id[s1:e1, s2:e2] *)
      let arr_typ =
        try StringMap.find id symtab
        with Not_found -> raise (Failure ("undeclared array identifier: " ^ id))
      in
      (match arr_typ with
        | Arr2D (elem_typ, orig_row_len, orig_col_len) ->
          if s1 < 0 || e1 <= 0 || s1 >= e1 || e1 > orig_row_len || 
            s2 < 0 || e2 <= 0 || s2 >= e2 || e2 > orig_col_len then
            raise (Failure (
              "invalid slice bounds [" ^ string_of_int s1 ^ ":" ^ string_of_int e1 ^
              "][" ^  string_of_int s2 ^ ":" ^ string_of_int e2 ^ "] for array '" ^ id ^ 
              "' of shape " ^ string_of_int orig_row_len ^ "x" ^ string_of_int orig_col_len
            ))
          else
            let new_rows = e1 - s1 in
            let new_cols = e2 - s2 in
            (Arr2D (elem_typ, new_rows, new_cols), SArr2DSlice (id, s1, e1, s2, e2))
        | _ ->
          raise (Failure (id ^ " is not a 2D array and cannot be sliced")))
    | NoExpr -> (Void, SNoExpr)
  in


  let check_func func =
    (* Make sure no formals or locals are void or duplicates *)
    check_binds "formal" func.formals;

    (* Build local symbol table of variables for this function *)
    let symbols = List.fold_left (fun m (ty, name) -> StringMap.add name ty m)
        StringMap.empty ( global_binds @ func.formals )
    in

    let check_bool_expr e symtab =
      let (t, e') = check_expr e symtab in
      match t with
      | Bool -> (t, e')
      |  _ -> raise (Failure ("expected Boolean expression in " ^ string_of_expr e))
    in

    let rec check_stmt_list stmt_list symtab =
      match stmt_list with
      | [] -> []
      | Block sl :: sl'  -> check_stmt_list (sl @ sl') symtab (* Flatten blocks *)
      | s :: sl -> 
        let s', symtab' = check_stmt s symtab in 
            s' :: check_stmt_list sl symtab'
    (* Return a semantically-checked statement i.e. containing sexprs *)
    and check_stmt stmt symtab =
      (* A block is correct if each statement is correct and nothing
         follows any Return statement.  Nested blocks are flattened. *)
      match stmt with
      | Block sl -> (SBlock (check_stmt_list sl symtab), symtab)
      | Expr e -> (SExpr (check_expr e symtab), symtab)
      | If(e, st1, st2) ->
        (SIf(check_bool_expr e symtab, fst (check_stmt st1 symtab), fst (check_stmt st2 symtab)), symtab)
      | For(s1, e1, e2, st) ->
        let s1', symtab' = check_stmt s1 symtab in
        (* Make sure that s1 is either a VDecl statement or an Exp *)
        let checked_s1' =
          match s1' with
          | SVDecl (SBindInit ((decl_typ, var), init_checked_expr)) ->
              (SVDecl (SBindInit ((decl_typ, var), init_checked_expr)))
          | SExpr e ->
              (SExpr (e))
          | _ ->
              raise (Failure "For loop initializer must be either an expression or a declaration with initialization")
        in
        let e1' = check_bool_expr e1 symtab' in
        let e2' = check_expr e2 symtab' in
        (SFor(checked_s1', e1', e2', fst (check_stmt st symtab')), symtab)
      | While(e, st) ->
       (SWhile(check_bool_expr e symtab, fst (check_stmt st symtab)), symtab)
      | Return e ->
        let (t, e') = check_expr e symtab in
        if t = func.rtyp then (SReturn (t, e'), symtab)
        else raise (
            Failure ("return gives " ^ string_of_typ t ^ " expected " ^
                    string_of_typ func.rtyp ^ " in " ^ string_of_expr e))
      | Break -> (SBreak, symtab)
      | VDecl v ->
        match v with
          | BindDecl(typ, id) -> 
            if StringMap.mem id symtab then
              raise (Failure ("Variable " ^ id ^ " is already declared"))
            else
              (SVDecl(SBindDecl(typ, id)), StringMap.add id typ symtab)
          | BindInit((typ, id), e) ->
            let rt, e' = check_expr e symtab in
            let err = "illegal assignment " ^ string_of_typ typ ^ " = " ^
                string_of_typ rt ^ " in " ^ string_of_expr e
            in 
            let _ = check_assign typ rt err in
            if StringMap.mem id symtab then
              raise (Failure ("Variable " ^ id ^ " is already declared"))
            else
              (SVDecl(SBindInit((typ, id), (rt, e'))), StringMap.add id typ symtab)
    in (* body of check_func *)
    { srtyp = func.rtyp;
      sfname = func.fname;
      sformals = func.formals;
      sbody = check_stmt_list func.body symbols;
    }
  in

(* check the exprs on RHS of global BindInits are only literals *)
  let check_expr_global e symtab =
    let (t, e') = check_expr e symtab in
    let is_scalar_lit = function
      | SIntLit _ | SDoubleLit _ | SBoolLit _ | SCharLit _ | SStringLit _ -> true
      | _ -> false
    in
    let scalar_type = function
      | Int | Double | Bool | Char | String -> true
      | _ -> false
    in
      (match (t, e')  with
        | (Int   , SIntLit    _)
        | (Double, SDoubleLit _)
        | (Bool  , SBoolLit   _)
        | (Char  , SCharLit   _)
        | (String, SStringLit _) -> (t, e')
        | (Arr1D (elem_t, _), SArr1DLit els) when scalar_type elem_t ->
          if List.for_all (fun (_,e') -> is_scalar_lit e') els
            then (t, e') else raise (Failure ("global initializer for arrays must be literals: " ^ string_of_expr e))
        | (Arr2D (elem_t, _, _), SArr2DLit rows) when scalar_type elem_t ->
          let row_all_lits row = List.for_all (fun (_,e') -> is_scalar_lit e') row in
          if List.for_all row_all_lits rows
          then (t, e') else raise (Failure ("global initializer for arrays must be literals: " ^ string_of_expr e))
        | _ -> raise (Failure ("global initializer not supported " ^ string_of_expr e)))
  in

  (* Create sbinds from all of the globals, checking BindInit types while doing so *)
  let check_global_binds global =
    (* Build local symbol table of variables for this function *)
    let global_symtab = List.fold_left (fun m (ty, name) ->
        if StringMap.mem name m then
          raise (Failure ("Variable " ^ name ^ " is already declared"))
        else
          StringMap.add name ty m)
        StringMap.empty ( global_binds )
    in
    match global with
      BindDecl(d) -> SBindDecl(d)
      | BindInit(d, e) -> 
        let lt = fst d 
        and (rt, e') = check_expr_global e global_symtab in
        let err = "illegal global assignment " ^ string_of_typ lt ^ " = " ^
                  string_of_typ rt
        in 
        let _ = check_assign lt rt err in SBindInit(d, (rt, e'))
  in
  (List.map check_global_binds globals, List.map check_func functions)

(* END *)
