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
    List.iter (check_not_void (fun n -> "illegal void variable: " ^n)) bd;
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
    if lvaluet = rvaluet then lvaluet else raise (Failure err)
  in

  (* Return a variable from our local symbol table *)
  let type_of_identifier s symtab =
    try StringMap.find s symtab
    with Not_found -> raise (Failure ("undeclared identifier " ^ s))
  in
  

  (* Return a semantically-checked expression, i.e., with a type *)
  let rec check_expr e symtab = 
    match e with
    |  IntLit l -> (Int, SLiteral l)
    | DoubleLit l -> (Double, SDoubleLit l)
    | BoolLit l -> (Bool, SBoolLit l)
    | CharLit l -> (Char, SCharLit l)
    | StringLit l -> (String, SStringLit l)
    | Arr1DLit l ->   
      let check_elements = List.map check_expr lst symtab in
      match check_elements with
      | [] -> raise (Failure "1D array cannot be empty")
      | (typ, _) :: tail -> 
        if match t with Arr1D _ | Arr2D _ -> true | _ -> false then
          raise (Failure "1D arrays cannot contain nested arrays") 
        else if List.for_all (fun (t', _) -> t' = t) tail then
          (Arr1D (t, List.length lst), SArr1DLit (List.map snd check_elements))
        else
          raise (Failure "All elements in 1D array must be the same type")
    | Arr2DLit l -> 
      let num_rows = List.length lst in
      let row_length = List.length first_row in 
      if not (List.for_all (fun row -> List.length row = row_length) lst) then
        raise (Failure "All rows in a 2D array must have the same length")
      else
        let check_rows = List.map (List.map check_expr) lst symtab in 
        match check_rows with
        | [] -> raise (Failure "Unexpected empty matrix")
        | (first_typ, _) :: _ ->
          if match first_typ with Arr1D _ | Arr2D _ -> true | _ -> false then
            raise (Failure "2D arrays cannot contain nested arrays")
          else if List.for_all (fun row -> List.for_all (fun (t, _) -> t = first_typ) row) check_rows then
            (Arr2D (first_typ, num_rows, row_length),
            SArr2DLit (List.map (List.map snd) check_rows))
          else
            raise (Failure "All elements of a 2D array must be of the same type")
    | Id var -> (type_of_identifier var symtab, SId var)
    | Assign(var, e) as ex ->
      let lt = type_of_identifier var symtab
      and (rt, e') = check_expr e symtab in
      let err = "illegal assignment " ^ string_of_typ lt ^ " = " ^
                string_of_typ rt ^ " in " ^ string_of_expr ex
      in
      (check_assign lt rt err, SAssign(var, (rt, e')))
    | Unop(op, e1) as e ->
      let (t1, e1') = check_expr e1 symtab in
      let t = match op with
          Neg when t1 = Int || t = Double -> t1
        | Not when t1 = Bool -> Bool
        | _ -> raise (Failure ("illegal unary operator " ^
                              string_of_unop op ^ " on " ^ string_of_typ t1 ^
                              " in " ^ string_of_expr e))
    | Binop(e1, op, e2) as e ->
      let (t1, e1') = check_expr e1 symtab
      and (t2, e2') = check_expr e2 symtab in
      let err = "illegal binary operator " ^
                string_of_typ t1 ^ " " ^ string_of_op op ^ " " ^
                string_of_typ t2 ^ " in " ^ string_of_expr e
      in
      (* All binary operators require operands of the same type*)
      if t1 = t2 then
        (* Determine expression type based on operator and operand types *)
        let t =
          match op, t1 with
          | (Add | Sub | Mult | Div | Mod | Pow), Int ->
              Int
          | (Add | Sub | Mult | Div | Mod | Pow), Double ->
              Double
          | (Add | Sub | Mult | Div | Mod | Pow), Arr1D(Int, n) ->
              Arr1D(Int, n)
          | (Add | Sub | Mult | Div | Mod | Pow), Arr1D(Double, n) ->
              Arr1D(Double, n)
          | (Add | Sub | Mult | Div | Mod | Pow), Arr2D(Int, m, n) ->
              Arr2D(Int, m, n)
          | (Add | Sub | Mult | Div | Mod | Pow), Arr2D(Double, m, n) ->
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
          | _ -> raise (Failure err)
        in
        (t, SBinop((t1, e1'), op, (t2, e2')))
      
      else raise (Failure err)
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
      typ = Void;
      fname = name;
      formals = [(typ, "x")];
      body = []
    } map
    in List.fold_left add_built_in StringMap.empty [
      ("print_int", Int);
      ("print_double", Double);
      ("print_char", Char);
      ("print_bool", Bool);
      ("print_str", String);
      ("print_arr", List)
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

(* DONE UP UNTIL HERE *)

  let check_func func =
    (* Make sure no formals or locals are void or duplicates *)
    check_binds "formal" func.formals;

    (* Build local symbol table of variables for this function *)
    let symbols = List.fold_left (fun m (ty, name) -> StringMap.add name ty m)
        StringMap.empty ( global_binds @ func.formals )
    in

    let check_bool_expr e  =
      let (t, e') = check_expr e symbols in
      match t with
      | Bool -> (t, e')
      |  _ -> raise (Failure ("expected Boolean expression in " ^ string_of_expr e))
    in

    let rec check_stmt_list = function
        [] -> []
      | Block sl :: sl'  -> check_stmt_list (sl @ sl') (* Flatten blocks *)
      | s :: sl -> check_stmt s :: check_stmt_list sl
    (* Return a semantically-checked statement i.e. containing sexprs *)
    and check_stmt =function
      (* A block is correct if each statement is correct and nothing
         follows any Return statement.  Nested blocks are flattened. *)
        Block sl -> SBlock (check_stmt_list sl)
      | Expr e -> SExpr (check_expr e symbols)
      | If(e, st1, st2) ->
        SIf(check_bool_expr e, check_stmt st1, check_stmt st2)
      | While(e, st) ->
        SWhile(check_bool_expr e, check_stmt st)
      | Return e ->
        let (t, e') = check_expr e symbols in
        if t = func.rtyp then SReturn (t, e')
        else raise (
            Failure ("return gives " ^ string_of_typ t ^ " expected " ^
                     string_of_typ func.rtyp ^ " in " ^ string_of_expr e))
    in (* body of check_func *)
    { srtyp = func.rtyp;
      sfname = func.fname;
      sformals = func.formals;
      sbody = check_stmt_list func.body
    }
  in

(* DONE FROM HERE UNTIL END *)
(* check the exprs on RHS of BindInits *)
  let check_expr_global e symtab =
    let (t, e') = check_expr e symtab in
      match t with
        | (Int | Double | Bool | Char | 
            String | Arr1D _ | Arr2D _) -> (t, e')
        | _ -> raise Failure(("global initializer not supported " ^ string_of_expr e))
  in

  (* Create sbinds from all of the globals, checking BindInit types while doing so *)
  let check_global_binds global =
    (* Build local symbol table of variables for this function *)
    let globalsymtab = List.fold_left (fun m (ty, name) -> StringMap.add name ty m)
        StringMap.empty ( global_binds )
    in
    match global with
      BindDecl(d) -> SBindDecl(d)
      | BindInit(d, e) -> 
        let lt = fst d 
        and (rt, e') = check_expr_global e globalsymtab in
        let err = "illegal global assignment " ^ string_of_typ lt ^ " = " ^
                  string_of_typ rt
        in 
        let _ = check_assign lt rt err in
          SBindInit(d, e') 

  in
  (List.map check_global_binds globals, List.map check_func functions)

(* END *)

(*
TODOs:
- update check_expr function
- update check_stmt function
  - remember to update symbol table for VDecl statements
*)