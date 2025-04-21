(* IR generation: translate takes a semantically checked AST and
   produces LLVM IR

   LLVM tutorial: Make sure to read the OCaml version of the tutorial

   http://llvm.org/docs/tutorial/index.html

   Detailed documentation on the OCaml LLVM library:

   http://llvm.moe/
   http://llvm.moe/ocaml/

*)

module L = Llvm
module A = Ast
open Sast

module StringMap = Map.Make(String)

(* translate : Sast.program -> Llvm.module *)
let translate (globals, functions) =
  let context    = L.global_context () in

  (* Create the LLVM compilation module into which
     we will generate code *)
  let the_module = L.create_module context "CAPybara" in

  (* Get types from the context *)
  let i32_t      = L.i32_type     context    (* ints *)
  and double_t   = L.double_type  context    (* doubles *)
  and pointer_t  = L.pointer_type            (* function pointers and strings *)
  and i8_t       = L.i8_type      context    (* chars *)
  and i1_t       = L.i1_type      context    (* bool *)
  and void_t     = L.void_type    context    (* void for function return type *)
  and array_t    = L.array_type              (* 1d and 2d arrays *)
  in

  (* Return the LLVM type for a CAPybara type *)
  let rec ltype_of_typ = function
      A.Int    -> i32_t
    | A.Double -> double_t
    | A.Char   -> i8_t
    | A.String -> pointer_t i8_t (* pointer to a char *)
    | A.Bool   -> i1_t
    | A.Arr1D(ty, n) ->
        let elem_ty = ltype_of_typ in
          array_t elem_ty n
    | A.Arr2D(ty, m, n) ->
        let elem_ty = ltype_of_typ in
          array_t (array_t elem_ty m) n
    | A.Void   -> void_t
  in


  let map_to_const_llvm ((ty, sx) : sexpr) : L.llvalue =
    match (ty, sx) with
      | (A.Int, SIntLit i) -> L.const_int  (ltype_of_typ ty) i
      | (A.Double, SDoubleLit d) -> L.const_float (ltype_of_typ ty) d
      | (A.Bool, SBool b) -> L.const_int (ltype_of_typ ty) (if b then 1 else 0)
      | (A.Char, SChar c) -> L.const_int (ltype_of_typ ty) (Char.code c)
      | (A.String, SString s) ->
        let str_const = L.const_stringz context s in (* create a char array *)
        let global_str_ptr = L.define_global ".str" str_const the_module in (* Add our char array to the global context *)
        L.const_bitcast global_str_ptr (pointer_t i8_t) (* cast from a pointer to char array to a pointer to a char *)
      | _ -> raise (Failure ("global initializer for arrays must be literals"))
  in


  (* Create a map of global variables after creating each *)
  let global_vars : L.llvalue StringMap.t =
    let global_var m (t, n) =
      match (t, n) with
        | SBindDecl(ty, na) ->
          let init = L.const_int (ltype_of_typ t) 0
          in StringMap.add n (L.define_global n init the_module) m in
        | SBindInit((ty, na), e) ->
            SIntLit i -> L.const_int (ltype_of_typ t) i
          | SDoubleLit d -> L.const_float (ltype_of_typ t) d
          | SBoolLit b -> L.const_int (ltype_of_typ t) (if b then 1 else 0)
          | SCharLit c -> L.const_int (ltype_of_typ t) (Char.code c)
          | SStringLit s -> 
            let str_const = L.const_stringz context s in (* create a char array *)
            let global_str_ptr = L.define_global ".str" str_const the_module in (* Add our char array to the global context *)
            L.const_bitcast global_str_ptr (pointer_t i8_t) (* cast from a pointer to char array to a pointer to a char *)
          | SArr1DLit elems ->
            (* get the types *)
            let (elem_typ, _) :: _ = List.hd elems in
            let llvm_elem_ty = ltype_of_typ elem_typ in
            
            let array_const = Array.map (map_to_const_llvm) (Array.of_list elems) in  (* contruct all of the literals *) 
            let arr_1d = L.const_array llvm_elem_ty array_const in  (* make the array using L.const_array *)
            let arr_ptr = L.define_global ".1d_array" arr_1d the_module in  (* add it globally to the module *)
            arr_ptr    (* return the pointer *)
          | SArr2DLit elems_list -> 
            (* get all of the types *)
            let ((elem_typ, _) :: _) = List.hd elems_list in
            let m  = List.length elems_list
            and n  = List.length (List.hd elems_list) in
            let llvm_elem_ty = ltype_of_typ elem_typ in

            (* helper func to turn a single row into an LLVM array of constants*) 
            let build_row row =
              let consts = Array.map map_to_const_llvm (Array.of_list row) in   (* constructed llvalue array *)
              L.const_array llvm_elem_ty consts          (* array of n x T *)
            in
            
            (* build array of constant rows *)
            let row_consts : L.llvalue array =
              Array.map build_row (Array.of_list elems_list)    (* m x [n x T] *)
            in
            let row_ty = L.array_type llvm_elem_ty n       (* [n x T] type *)
            let arr_2d = L.const_array row_ty row_consts in  (* make the array of [m x [n x T]] using row_consts values *)
            let arr_2d_ptr = L.define_global ".2d_array" arr_2d the_module in  (* add it globally to the module *)
            arr_2d_ptr    (* return the pointer *)
    List.fold_left global_var StringMap.empty globals in

  (* Add the built-in functions *)
  let printf_t : L.lltype =
    L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_func : L.llvalue =
    L.declare_function "printf" printf_t the_module in

  (* Define each function (arguments and return type) so we can
     call it even before we've created its body *)
  let function_decls : (L.llvalue * sfunc_def) StringMap.t =
    let function_decl m fdecl =
      let name = fdecl.sfname
      and formal_types =
        Array.of_list (List.map (fun (t,_) -> ltype_of_typ t) fdecl.sformals)
      in let ftype = L.function_type (ltype_of_typ fdecl.srtyp) formal_types in
      StringMap.add name (L.define_function name ftype the_module, fdecl) m in
    List.fold_left function_decl StringMap.empty functions in


  (* Fill in the body of the given function *)
  let build_function_body fdecl =
    let (the_function, _) = StringMap.find fdecl.sfname function_decls in
    let builder = L.builder_at_end context (L.entry_block the_function) in

    let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder in
    let double_format_str = L.build_global_stringptr "%f\n" "fmt" builder in

    let bool_fmt = L.build_global_stringptr "%s\n" "bool_fmt" builder in
    let true_str = L.build_global_stringptr "true"  "true_lit"  builder in
    let false_str = L.build_global_stringptr "false" "false_lit" builder in

    let char_format_str = L.build_global_stringptr "%c\n" "fmt" builder in
    let str_format_str = L.build_global_stringptr "%s\n" "fmt" builder in

    (* Construct the function's "formals": formal arguments.
      Allocate each on the stack, initialize their value, if appropriate,
      and remember their values in the "formals" map. Locally declared
      variables in the function will be added to this map as they're 
      encountered as statements *)
    let formal_vars =
      let add_formal m (t, n) p =
        L.set_value_name n p;
        let local = L.build_alloca (ltype_of_typ t) n builder in
        ignore (L.build_store p local builder);
        StringMap.add n local m
      in
      List.fold_left2 add_formal StringMap.empty fdecl.sformals
          (Array.to_list (L.params the_function))
    in

    (* Allocate space for any locally declared variables and add the
       * resulting registers to our map *)
    let add_local m (t, n) =
      let local_var = L.build_alloca (ltype_of_typ t) n builder
      in StringMap.add n local_var m
    in

    (* Return the value for a variable or formal argument.
       Check local names first, then global names *)
    let lookup n = try StringMap.find n local_vars
      with Not_found -> StringMap.find n global_vars
    in

    (* done up to here *)

    (* Construct code for an expression; return its value *)
    let rec build_expr builder ((_, e) : sexpr) = match e with
        SLiteral i  -> L.const_int i32_t i
      | SBoolLit b  -> L.const_int i1_t (if b then 1 else 0)
      | SId s       -> L.build_load (lookup s) s builder
      | SAssign (s, e) -> let e' = build_expr builder e in
        ignore(L.build_store e' (lookup s) builder); e'
      | SBinop (e1, op, e2) ->
        let e1' = build_expr builder e1
        and e2' = build_expr builder e2 in
        (match op with
           A.Add     -> L.build_add
         | A.Sub     -> L.build_sub
         | A.And     -> L.build_and
         | A.Or      -> L.build_or
         | A.Equal   -> L.build_icmp L.Icmp.Eq
         | A.Neq     -> L.build_icmp L.Icmp.Ne
         | A.Less    -> L.build_icmp L.Icmp.Slt
        ) e1' e2' "tmp" builder
      | SCall ("print_int", [e]) ->
        L.build_call printf_func [| int_format_str ; (build_expr builder e) |]
          "printf" builder
      | SCall ("print_double", [e]) ->
        L.build_call printf_func [| double_format_str ; (build_expr builder e) |]
          "printf" builder
      | SCall ("print_char", [e]) ->
        L.build_call printf_func [| char_format_str ; (build_expr builder e) |]
          "printf" builder
      | SCall ("print_bool", [e]) ->
        let bool_val = build_expr builder e in (* i1 result of e *)
        let chosen =
          L.build_select bool_val                     (* i1 condition        *)
                        true_str                     (* i8* if true         *)
                        false_str                    (* i8* if false        *)
                        "bool_as_str" builder
        in
        L.build_call printf_func [| bool_fmt ; chosen |] "printf" builder
      | SCall ("print_str", [e]) ->
        L.build_call printf_func [| str_format_str ; (build_expr builder e) |]
          "printf" builder
      | SCall (f, args) ->
        let (fdef, fdecl) = StringMap.find f function_decls in
        let llargs = List.rev (List.map (build_expr builder) (List.rev args)) in
        let result = f ^ "_result" in
        L.build_call fdef (Array.of_list llargs) result builder
    in

    (* LLVM insists each basic block end with exactly one "terminator"
       instruction that transfers control.  This function runs "instr builder"
       if the current block does not already have a terminator.  Used,
       e.g., to handle the "fall off the end of the function" case. *)
    let add_terminal builder instr =
      match L.block_terminator (L.insertion_block builder) with
        Some _ -> ()
      | None -> ignore (instr builder) in

    (* Build the code for the given statement; return the builder for
       the statement's successor (i.e., the next instruction will be built
       after the one generated by this call) *)
    let rec build_stmt (builder, local_vars, we_bb) = function
        SBlock sl -> 
          let build_stmts (builder', local_vars') = 
            build_stmt (builder', local_vars', we_bb) in 
          (fst (List.fold_left build_stmts (builder, local_vars) sl), local_vars)
      | SExpr e -> ignore(build_expr builder e); (builder, local_vars)
      | SReturn e -> ignore(match fdec.srtyp with
           A.Void -> L.build_ret_void builder
          | _ -> L.build_ret (build_expr builder e) builder);
          (builder, local_vars)
      | SIf (predicate, then_stmt, else_stmt) ->
        let bool_val = build_expr builder predicate in

        let then_bb = L.append_block context "then" the_function in
        ignore (build_stmt ((L.builder_at_end context then_bb), local_vars, we_bb) then_stmt);
        let else_bb = L.append_block context "else" the_function in
        ignore (build_stmt ((L.builder_at_end context then_bb), local_vars, we_bb)  else_stmt);

        let end_bb = L.append_block context "if_end" the_function in
        let build_br_end = L.build_br end_bb in (* partial function *)
        add_terminal (L.builder_at_end context then_bb) build_br_end;
        add_terminal (L.builder_at_end context else_bb) build_br_end;

        ignore(L.build_cond_br bool_val then_bb else_bb builder);
        (L.builder_at_end context end_bb, local_vars)
      | SFor (s1, e1, e2, body) ->
        let builder1, local_vars' = build_stmt (builder, local_vars, we_bb) s1 in
        let builder2, _ = build_stmt (builder1, local_vars', we_bb) 
          SWhile (e1, SBlock [body ; SExpr e2]) in
        (builder2, local_vars)        
      | SWhile (predicate, body) ->
        let while_bb = L.append_block context "while" the_function in
        let build_br_while = L.build_br while_bb in (* partial function *)
        ignore (build_br_while builder);
        let while_builder = L.builder_at_end context while_bb in
        let bool_val = build_expr while_builder predicate in

        let body_bb = L.append_block context "while_body" the_function in
        let end_bb = L.append_block context "while_end" the_function in

        add_terminal (build_stmt ((L.builder_at_end context body_bb), local_vars, Some(end_bb)) body) build_br_while;

        ignore(L.build_cond_br bool_val body_bb end_bb while_builder);
        (L.builder_at_end context end_bb, local_vars)
      | SBreak ->
        (* we check what the end block of the while loop is to unconditionally jump to *)
        let while_end_bb = (match we_bb with 
           Some(bb) -> bb
         | None -> raise(Failure ("Break statement not in loop")))
        in ignore(L.build_br while_end_bb builder);
        (builder, local_vars)
      | SVDecl vdecl -> (builder, match vdecl with
        | SBindDecl(ty, n) ->
          let local_var = L.build_alloca (ltype_of_typ t) n builder
          in StringMap.add n local_var local_vars (* return the updated local_vars *)
        | SBindInit((ty, n), e) ->
          let e' = build_expr builder e (* update later to take local_vars map *)
          in L.set_value_name n e';
          let local_var = L.build_alloca (ltype_of_typ t) n builder
          in ignore(L.build_store e' local_var builder);
          StringMap.add n local_var local_vars (* return the updated local_vars *)
        )
    (* finished build_stmt *)
    in
    (* Build the code for each statement in the function *)
    let func_builder = build_stmt builder (SBlock fdecl.sbody) in

    (* Add a return if the last block falls off the end *)
    add_terminal func_builder (L.build_ret (L.const_int i32_t 0))

  in

  List.iter build_function_body functions;
  the_module

  (* TODO:
    - build_expr
  *)