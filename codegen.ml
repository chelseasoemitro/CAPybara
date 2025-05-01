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
        let elem_ty = ltype_of_typ ty in
          array_t elem_ty n
    | A.Arr2D(ty, m, n) ->
        let elem_ty = ltype_of_typ ty in
          array_t (array_t elem_ty n) m
    | A.Void   -> void_t
  in


  let map_to_const_llvm ((ty, sx) : sexpr) : L.llvalue =
    match (ty, sx) with
      | (A.Int, SIntLit i) -> L.const_int  (ltype_of_typ ty) i
      | (A.Double, SDoubleLit d) -> L.const_float (ltype_of_typ ty) d
      | (A.Bool, SBoolLit b) -> L.const_int (ltype_of_typ ty) (if b then 1 else 0)
      | (A.Char, SCharLit c) -> L.const_int (ltype_of_typ ty) (Char.code c)
      | (A.String, SStringLit s) ->
        let str_const = L.const_stringz context s in (* create a char array *)
        let global_str_ptr = L.define_global ".str" str_const the_module in (* Add our char array to the global context *)
        L.const_bitcast global_str_ptr (pointer_t i8_t) (* cast from a pointer to char array to a pointer to a char *)
      | _ -> raise (Failure ("global initializer for arrays must be literals"))
  in


  (* Create a map of global variables after creating each *)
  let global_vars : L.llvalue StringMap.t =
    let global_var m = function
        | SBindDecl(t, na) ->
          let init = L.const_int (ltype_of_typ t) 0
          in StringMap.add na (L.define_global na init the_module) m
        | SBindInit((t, na), (_, e)) ->
          let init = 
            (match e with
              | SIntLit i -> L.const_int (ltype_of_typ t) i
              | SDoubleLit d -> L.const_float (ltype_of_typ t) d
              | SBoolLit b -> L.const_int (ltype_of_typ t) (if b then 1 else 0)
              | SCharLit c -> L.const_int (ltype_of_typ t) (Char.code c)
              | SStringLit s -> 
                let str_const = L.const_stringz context s in (* create a char array *)
                let global_str_ptr = L.define_global ".str" str_const the_module in (* Add our char array to the global context *)
                L.const_bitcast global_str_ptr (pointer_t i8_t) (* cast from a pointer to char array to a pointer to a char *)
              | SArr1DLit elems ->
                (* get the types *)
                let (elem_typ, _) = List.hd elems in
                let llvm_elem_ty = ltype_of_typ elem_typ in
                
                let array_const = Array.map (map_to_const_llvm) (Array.of_list elems) in  (* contruct all of the literals *) 
                let arr_1d = L.const_array llvm_elem_ty array_const in  (* make the array using L.const_array *)
                let arr_ptr = L.define_global ".1d_array" arr_1d the_module in  (* add it globally to the module *)
                arr_ptr    (* return the pointer *)
              | SArr2DLit elems_list -> 
                (* get all of the types *)
                (match elems_list with
                  | ((elem_typ, _) :: _) :: _ ->
                    let n  = List.length (List.hd elems_list) in
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
                    let row_ty = L.array_type llvm_elem_ty n in      (* [n x T] type *)
                    let arr_2d = L.const_array row_ty row_consts in  (* make the array of [m x [n x T]] using row_consts values *)
                    let arr_2d_ptr = L.define_global ".2d_array" arr_2d the_module in  (* add it globally to the module *)
                    arr_2d_ptr    (* return the pointer *)
                | _ -> raise (Failure "empty 2-D array literal")
              )
              | _ -> raise (Failure ("global variable " ^ na ^ " must be assigned to a literal"))
            ) in 
            StringMap.add na (L.define_global na init the_module) m
    in 
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

    (* Return the value for a variable or formal argument.
       Check local names first, then global names *)
    let lookup n local_vars is_arr builder = 
      match StringMap.find_opt n local_vars with
        | Some(slot) -> slot
        | None ->
          let slot = StringMap.find n global_vars in
          if is_arr then L.build_load slot (n ^ "_body_ptr") builder else slot
    in

    let arr_1d_memcpy builder local_vars len src_body_ptr dst_body_ptr  =
      (* loop *)
      for i = 0 to len - 1 do
        let idx = L.const_int i32_t i in
        (* load element *)
        let src_gep =
          L.build_in_bounds_gep src_body_ptr
            [| L.const_int i32_t 0; idx |]
            "src_gep" builder
        in
        let v = L.build_load src_gep "src_val" builder in

        (* store *)
        let dst_gep =
          L.build_in_bounds_gep dst_body_ptr
            [| L.const_int i32_t 0; idx |]
            "dst_gep" builder
        in
        ignore (L.build_store v dst_gep builder)
      done;

      (* return the ptr to the memcpy'd array *)
      dst_body_ptr
    in

    let arr_2d_memcpy builder local_vars m n src_body_ptr dst_body_ptr  =
      (* loop *)
      for i = 0 to m - 1 do
        let i_idx = L.const_int i32_t i in
        (* pointer to row i of source *)
        let src_row_ptr =
          L.build_in_bounds_gep src_body_ptr
            [| L.const_int i32_t 0; i_idx |]
            "src_row" builder
        in
        (* pointer to row i of dest *)
        let dst_row_ptr =
          L.build_in_bounds_gep dst_body_ptr
            [| L.const_int i32_t 0; i_idx |]
            "dst_row" builder
        in
    
        for j = 0 to n - 1 do
          let j_idx = L.const_int i32_t j in
    
          (* load src[i][j] *)
          let src_elem_gep =
            L.build_in_bounds_gep src_row_ptr
              [| L.const_int i32_t 0; j_idx |]
              "src_gep" builder
          in
          let v = L.build_load src_elem_gep "src_val" builder in
    
          (* store into dest[i][j] *)
          let dst_elem_gep =
            L.build_in_bounds_gep dst_row_ptr
              [| L.const_int i32_t 0; j_idx |]
              "dst_gep" builder
          in
          ignore (L.build_store v dst_elem_gep builder)
        done
      done;

      (* return the ptr to the memcpy'd array *)
      dst_body_ptr
    in

    (* Construct code for an expression; return its a pointer to where it's value is *)
    let rec build_expr builder local_vars ((ty, e) : sexpr) = match e with
        SIntLit i  -> L.const_int i32_t i
      | SDoubleLit d -> L.const_float double_t d
      | SBoolLit b  -> L.const_int i1_t (if b then 1 else 0) 
      | SCharLit c -> L.const_int i8_t (Char.code c)
      | SStringLit s -> L.build_global_stringptr s ".str" builder
      | SArr1DLit elems ->
         (* get the types *)
        let (elem_typ, _) = List.hd elems in
        let llvm_elem_ty = ltype_of_typ elem_typ in
        (* create the array *)
        let const_vals = Array.map (map_to_const_llvm) (Array.of_list elems) in  (* contruct all of the literals *) 
        let arr_const = L.const_array llvm_elem_ty const_vals in
        (* put the constant somewhere (stack is fine) and return its address *)
        let alloca_ptr = L.build_alloca (L.type_of arr_const) "_arr1d_lit" builder in
        ignore (L.build_store arr_const alloca_ptr builder); (* store pointer address *)
        alloca_ptr 
        (* return a pointer to the array *)
      | SArr2DLit elems_list ->
        (* get the types *)
        (match elems_list with
          | ((elems_typ, _) :: _) :: _ ->
            let n  = List.length (List.hd elems_list) in
            let llvm_elem_ty = ltype_of_typ elems_typ in

            let build_row row =
              let consts = Array.map map_to_const_llvm (Array.of_list row) in   (* constructed llvalue array *)
              L.const_array llvm_elem_ty consts          (* array of n x T *)
            in

            (* build array of constant rows *)
            let row_consts : L.llvalue array =
              Array.map build_row (Array.of_list elems_list)    (* m x [n x T] *)
            in
            let row_ty = L.array_type llvm_elem_ty n  in     (* [n x T] type *)
            let arr_const = L.const_array row_ty row_consts in (* return the pointer to an array of [m x [n x T]] made of row_consts values *)
            let alloca_ptr = L.build_alloca (L.type_of arr_const) "_arr2d_lit" builder in
            ignore (L.build_store arr_const alloca_ptr builder);
            alloca_ptr
          | _ -> raise (Failure "empty 2-D array literal")) 
      | SId s -> 
        let is_arr = 
          (match ty with
            | A.Arr1D _ | A.Arr2D _ -> true
            | _ -> false 
          )
        in
        let ptr = lookup s local_vars is_arr builder in
        if is_arr then ptr else L.build_load ptr s builder
      | SAssign (s, e) -> 
        let e' = build_expr builder local_vars e in

        let is_arr = 
          (match ty with
            | A.Arr1D _ | A.Arr2D _ -> true 
            | _ -> false  
          ) 
        in
        let body_ptr = lookup s local_vars is_arr builder in

        let () = 
          (match ty with
          | A.Arr1D (elem_ty, len) ->
            ignore(arr_1d_memcpy builder local_vars len e' body_ptr)
          | A.Arr2D (elem_ty, m, n) ->
            ignore(arr_2d_memcpy builder local_vars m n e' body_ptr)
          | _ -> ignore(L.build_store e' (lookup s local_vars is_arr builder) builder)
          ) in
        e'
      | SBinop (((A.Arr1D(_, _), _) as e1), op, ((A.Arr1D(_, _), _) as e2)) ->
        (match op with
          | A.Add -> build_arr1d_add builder local_vars e1 e2
          | A.Sub -> build_arr1d_sub builder local_vars e1 e2
          | A.Mult -> build_arr1d_mult builder local_vars e1 e2
          | A.Div -> build_arr1d_div builder local_vars e1 e2
          | _ -> raise(Failure "Incompatible operation for Arr1D/Arr1D")
        )
      | SBinop (((A.Arr2D(_, _, _), _) as e1), op, ((A.Arr2D (_, _, _), _) as e2)) ->
          (match op with
            | A.Mmult -> build_arr2d_mmult builder local_vars e1 e2
            | A.Add -> build_arr2d_add builder local_vars e1 e2
            | A.Sub -> build_arr2d_sub builder local_vars e1 e2
            | A.Mult -> build_arr2d_mult builder local_vars e1 e2
            | A.Div -> build_arr2d_div builder local_vars e1 e2
            | _ -> raise(Failure "Incompatible operation for Arr2D/Arr2D")
            )
      | SBinop (((A.Double, _) as e1), op, ((A.Double, _) as e2)) ->
        let e1' = build_expr builder local_vars e1
        and e2' = build_expr builder local_vars e2 in
        (match op with
          | A.Add     -> L.build_fadd
          | A.Sub     -> L.build_fsub
          | A.Mult    -> L.build_fmul
          | A.Div     -> L.build_fdiv
          | A.Mod     -> L.build_frem
          | A.Equal   -> L.build_fcmp L.Fcmp.Oeq
          | A.Neq     -> L.build_fcmp L.Fcmp.One
          | A.Lt    -> L.build_fcmp L.Fcmp.Olt
          | A.Gt    -> L.build_fcmp L.Fcmp.Ogt
          | A.Le   -> L.build_fcmp L.Fcmp.Ole
          | A.Ge   -> L.build_fcmp L.Fcmp.Oge
          | _      -> raise (Failure("error: not a viable double-double operation"))
          ) e1' e2' "tmp" builder
      (* mismatching types for array * scalar multiplication *)
      | SBinop (((A.Arr1D(_, _), _) as e1), op, e2) ->
        (match (e1, op, e2) with 
          | ((A.Arr1D(A.Int, _), _), A.Mult, (A.Int, _)) -> build_arr_1d_int_mult builder local_vars e1 e2
          | ((A.Arr1D(A.Double, _), _), A.Mult, (A.Double, _)) -> build_arr_1d_double_mult builder local_vars e1 e2
          | _ -> raise (Failure ("build expr: SBinop (((A.Arr1D(_, _), _) as e1), op, e2)."))
        )
      | SBinop (e1, op, ((A.Arr1D(_, _), _) as e2)) ->
        (match (e1, op, e2) with 
          | ((A.Int, _), A.Mult, (A.Arr1D(A.Int, _), _)) -> build_arr_1d_int_mult builder local_vars e2 e1
          | ((A.Double, _), A.Mult, (A.Arr1D(A.Double, _), _)) -> build_arr_1d_double_mult builder local_vars e2 e1
          | _ -> raise (Failure ("build expr: SBinop (e1, op, ((A.Arr1D(_, _), _) as e2))."))
        )
      | SBinop (((A.Arr2D(_, _, _), _) as e1), op, e2) ->
        (match (e1, op, e2) with 
          | ((A.Arr2D(A.Int, _, _), _), A.Mult, (A.Int, _)) -> build_arr_2d_int_mult builder local_vars e1 e2
          | ((A.Arr2D(A.Double, _, _), _), A.Mult,(A.Double, _)) -> build_arr_2d_double_mult builder local_vars e1 e2
          | _ -> raise (Failure ("build expr: SBinop (((A.Arr2D(_, _, _), _) as e1), op, e2)"))
        )
      | SBinop (e1, op, ((A.Arr2D(_, _, _), _) as e2)) ->
        (match (e1, op, e2) with 
          | ((A.Int, _), A.Mult, (A.Arr2D(A.Int, _, _), _)) -> build_arr_2d_int_mult builder local_vars e2 e1
          | ((A.Double, _), A.Mult, (A.Arr2D(A.Double, _, _), _)) -> build_arr_2d_double_mult builder local_vars e2 e1
          | _ -> raise (Failure ("build expr: SBinop (e1, op, ((A.Arr2D(_, _, _), _) as e2))"))
        )
      (* General binop case: ints, bools, chars, strings *)
      | SBinop (e1, op, e2) ->        
        let e1' = build_expr builder local_vars e1
        and e2' = build_expr builder local_vars e2 in
        (match op with
          | A.Add     -> L.build_add
          | A.Sub     -> L.build_sub
          | A.Mult    -> L.build_mul
          | A.Div     -> L.build_sdiv
          | A.Mod     -> L.build_srem 
          | A.Equal   -> L.build_icmp L.Icmp.Eq
          | A.Neq     -> L.build_icmp L.Icmp.Ne
          | A.Lt      -> L.build_icmp L.Icmp.Slt
          | A.Gt      -> L.build_icmp L.Icmp.Sgt
          | A.Le      -> L.build_icmp L.Icmp.Sle
          | A.Ge      -> L.build_icmp L.Icmp.Sge
          | A.And     -> L.build_and
          | A.Or      -> L.build_or
          | _         -> raise (Failure("error: not a viable int-int operation"))
        ) e1' e2' "tmp" builder
      | SUop (op, ((ty, _) as e1)) ->
        let e1' = build_expr builder local_vars e1 in
        (match op with
          | A.Not                    -> L.build_not
          | A.Neg when ty = A.Double -> L.build_fneg
          | A.Neg                    -> L.build_neg
        )  e1' "tmp" builder
      | SArr1DAssign (id, ((index_ty, index_s) as i), ((value_ty, value_s) as v)) ->
          (* get ptrs and values *)
          let arr_ptr = lookup id local_vars true builder in
          let index_val = build_expr builder local_vars i in
          let scalar_val = build_expr builder local_vars v in

          (* load arr_ptr[index_val] *)
          let arr_elem_gep =
            L.build_in_bounds_gep arr_ptr
              [| L.const_int i32_t 0; index_val |]
              "arr_gep" builder
          in
          (* store value *)
          ignore (L.build_store scalar_val arr_elem_gep builder);

          (* return the stored value *)
          scalar_val
      | SArr2DAssign (id, ((row_index_ty, row_index_s) as r), ((col_index_ty, col_index_s) as c), ((value_ty, value_s) as v)) ->
        let arr_ptr = lookup id local_vars true builder in
        let row_index_val = build_expr builder local_vars r in
        let col_index_val = build_expr builder local_vars c in 
        let scalar_val = build_expr builder local_vars v in

        (* pointer to row i of source *)
         let arr_row_ptr =
          L.build_in_bounds_gep arr_ptr
            [| L.const_int i32_t 0; row_index_val |]
            "arr_row" builder
        in

        (* load arr_ptr[index_val] *)
        let arr_elem_gep =
          L.build_in_bounds_gep arr_row_ptr
            [| L.const_int i32_t 0; col_index_val |]
            "arr_gep" builder
        in
        (* store value *)
        ignore (L.build_store scalar_val arr_elem_gep builder);
        
        (* return the stored value *)
        scalar_val
      | SArr1DAccess (id, ((index_ty, index_s) as i)) ->
        (* get ptrs and values *)
        let arr_ptr = lookup id local_vars true builder in
        let index_val = build_expr builder local_vars i in

        (* load arr_ptr[index_val] *)
        let arr_elem_gep =
          L.build_in_bounds_gep arr_ptr
            [| L.const_int i32_t 0; index_val |]
            "arr_gep" builder
        in
        (* return the accessed value *)
        L.build_load arr_elem_gep "src_val" builder
      | SArr2DAccess (id, ((row_index_ty, row_index_s) as r), ((col_index_ty, col_index_s) as c)) ->
        print_string(string_of_typ ty);
        let arr_ptr = lookup id local_vars true builder in
        let row_index_val = build_expr builder local_vars r in
        let col_index_val = build_expr builder local_vars c in 

        (* pointer to row i of source *)
         let arr_row_ptr =
          L.build_in_bounds_gep arr_ptr
            [| L.const_int i32_t 0; row_index_val |]
            "arr_row" builder
        in

        (* load arr_ptr[index_val] *)
        let arr_elem_gep =
          L.build_in_bounds_gep arr_row_ptr
            [| L.const_int i32_t 0; col_index_val |]
            "arr_gep" builder
        in
        (* retrieve the value at the index and return *)
        L.build_load arr_elem_gep "src_val" builder
      | SArrOp (op, ((ty, arr_se) as arr_expr), func_name) -> 
        (match op with
          | A.Map    ->
            (match ty with
            | A.Arr1D (_) -> build_map_arr_1d builder local_vars arr_expr func_name
            | A.Arr2D (_) -> build_map_arr_2d builder local_vars arr_expr func_name
            | _ -> raise (Failure("huh"))
            )
          | A.Reduce ->
            (match ty with
            | A.Arr1D (_) -> build_reduce_arr_1d builder local_vars arr_expr func_name
            | A.Arr2D (_) -> build_reduce_arr_2d builder local_vars arr_expr func_name
            | _ -> raise (Failure("wat"))
            )
        )
      | SArrUop (op, ((ty, arr_se) as arr_expr)) ->
        (match op with
          | A.Length    -> 
            (match ty with
            | A.Arr1D (_, n) ->
                L.const_int i32_t n
            | A.Arr2D (_, m, _) ->
                L.const_int i32_t m
            | _ ->
                failwith "Length operator on non-array")
          | A.Transpose -> build_arr_2d_transpose builder local_vars arr_expr
        )
      | SArr1DSlice (id, s0, _) -> 
          build_arr_1d_slice builder local_vars id ty s0
      | SArr2DSlice (id, s0, _, r0, _) -> 
          build_arr_2d_slice builder local_vars id ty s0 r0
      | SNoExpr ->  L.const_int i32_t  0
      | SCall ("print_int", [e]) ->
        L.build_call printf_func [| int_format_str ; (build_expr builder local_vars e) |]
          "printf" builder
      | SCall ("print_double", [e]) ->
        L.build_call printf_func [| double_format_str ; (build_expr builder local_vars e) |]
          "printf" builder
      | SCall ("print_char", [e]) ->
        L.build_call printf_func [| char_format_str ; (build_expr builder local_vars e) |]
          "printf" builder
      | SCall ("print_bool", [e]) ->
        let bool_val = build_expr builder local_vars e in (* i1 result of e *)
        let chosen =
          L.build_select bool_val                    (* i1 condition        *)
                        true_str                     (* i8* if true         *)
                        false_str                    (* i8* if false        *)
                        "bool_as_str" builder
        in
        L.build_call printf_func [| bool_fmt ; chosen |] "printf" builder
      | SCall ("print_str", [e]) ->
        L.build_call printf_func [| str_format_str ; (build_expr builder local_vars e) |]
          "printf" builder
      | SCall (f, args) ->
        let (fdef, fdecl) = StringMap.find f function_decls in
        let llargs = List.rev (List.map (build_expr builder local_vars) (List.rev args)) in (* TODO this call may be wrong now *)
        let result = f ^ "_result" in
        L.build_call fdef (Array.of_list llargs) result builder
    and 
    
    build_arr1d_add builder local_vars arr1_se arr2_se =
      (* get the ptrs *)
      let arr1_ptr = build_expr builder local_vars arr1_se in
      let arr2_ptr = build_expr builder local_vars arr2_se in
      let elem_ty, len = 
        (match arr1_se with
          | (A.Arr1D (elem_ty, n), _) -> elem_ty, n
          | _ -> raise (Failure "build_arr1d_add: not a 1D array")
        ) in
        
      (* allocate result [len x i32] *)
      let llvm_arr_ty = ltype_of_typ (A.Arr1D (elem_ty, len)) in
      let res_ptr = L.build_alloca llvm_arr_ty "arr_add_res" builder in

      (* loop *)
      for i = 0 to len - 1 do
        let idx = L.const_int i32_t i in
        (* load elements *)
        let src1_gep =
          L.build_in_bounds_gep arr1_ptr
            [| L.const_int i32_t 0; idx |]
            "src1_gep" builder
        in
        let v1 = L.build_load src1_gep "src1_val" builder in
        let src2_gep =
          L.build_in_bounds_gep arr2_ptr
            [| L.const_int i32_t 0; idx |]
            "src2_gep" builder
        in
        let v2 = L.build_load src2_gep "src2_val" builder in

        (* v1 + v2 *)
        let sum =
          (match elem_ty with
          | A.Int -> L.build_add v1 v2 "elt_add" builder
          | A.Double -> L.build_fadd v1 v2 "elt_add" builder
          | _ -> raise (Failure "build_arr1d_add: not an int or double")
          )
        in
      
        (* store *)
        let dst_gep =
          L.build_in_bounds_gep res_ptr
            [| L.const_int i32_t 0; idx |]
            "dst_gep" builder
        in
        ignore (L.build_store sum dst_gep builder)
      done;
      
      (* return the ptr to the new array *)
      res_ptr
      
    and
    
    build_arr1d_sub builder local_vars arr1_se arr2_se =
      (* get the ptrs *)
      let arr1_ptr = build_expr builder local_vars arr1_se in
      let arr2_ptr = build_expr builder local_vars arr2_se in
      (* get the length *)
      let elem_ty, len = 
        (match arr1_se with
          | (A.Arr1D (elem_ty, n), _) -> elem_ty, n
          | _ -> raise (Failure "build_arr_1d_sub: not an array1D")
        ) in

      (* allocate result [len x i32] *)
      let llvm_arr_ty = ltype_of_typ (A.Arr1D (elem_ty, len)) in
      let res_ptr = L.build_alloca llvm_arr_ty "arr1d_sub_res" builder in

      (* loop *)
      for i = 0 to len - 1 do
        let idx = L.const_int i32_t i in
        (* load element *)
        let src_gep =
          L.build_in_bounds_gep arr1_ptr
            [| L.const_int i32_t 0; idx |]
            "src_gep" builder
        in
        let v1 = L.build_load src_gep "src_val1" builder in
        let src2_gep =
          L.build_in_bounds_gep arr2_ptr
            [| L.const_int i32_t 0; idx |]
            "src2_gep" builder
        in
        let v2 = L.build_load src2_gep "src_val2" builder in

        (* v1 - v2 *)
        let diff =
          (match elem_ty with
          | A.Int -> L.build_sub v1 v2 "elt_sub" builder
          | A.Double -> L.build_fsub v1 v2 "elt_sub" builder
          | _ -> raise (Failure "build_arr1d_sub: not an int or double")
          )
        in
      
        (* store *)
        let dst_gep =
          L.build_in_bounds_gep res_ptr
            [| L.const_int i32_t 0; idx |]
            "dst_gep" builder
        in
        ignore (L.build_store diff dst_gep builder)
      done;

      (* return the ptr to the new array *)
      res_ptr
    and
    
    build_arr1d_mult builder local_vars arr1_se arr2_se =
      (* get the ptrs *)
      let arr1_ptr = build_expr builder local_vars arr1_se in
      let arr2_ptr = build_expr builder local_vars arr2_se in
      (* get the length *)
      let elem_ty, len = 
        (match arr1_se with
          | (A.Arr1D (elem_ty, n), _) -> elem_ty, n
          | _ -> raise (Failure "build_arr1d_mult: not an array1D")
        ) in

      (* allocate result [len x i32] *)
      let llvm_arr_ty = ltype_of_typ (A.Arr1D (elem_ty, len)) in
      let res_ptr = L.build_alloca llvm_arr_ty "arr1d_mul_res" builder in

      (* loop *)
      for i = 0 to len - 1 do
        let idx = L.const_int i32_t i in
        (* load element *)
        let src_gep =
          L.build_in_bounds_gep arr1_ptr
            [| L.const_int i32_t 0; idx |]
            "src_gep" builder
        in
        let v1 = L.build_load src_gep "src_val1" builder in
        let src2_gep =
          L.build_in_bounds_gep arr2_ptr
            [| L.const_int i32_t 0; idx |]
            "src2_gep" builder
        in
        let v2 = L.build_load src2_gep "src_val2" builder in

        (* v1 * v2 *)
        let prod =
          (match elem_ty with
          | A.Int -> L.build_mul v1 v2 "elt_mul" builder
          | A.Double -> L.build_fmul v1 v2 "elt_mul" builder
          | _ -> raise (Failure "build_arr1d_mult: not an int or double")
          )
        in
        
        (* store *)
        let dst_gep =
          L.build_in_bounds_gep res_ptr
            [| L.const_int i32_t 0; idx |]
            "dst_gep" builder
        in
        ignore (L.build_store prod dst_gep builder)
      done;

      (* return the ptr to the new array *)
      res_ptr
      
    and 

    build_arr1d_div builder local_vars arr1_se arr2_se = 
      (* get the ptrs *)
      let arr1_ptr = build_expr builder local_vars arr1_se in
      let arr2_ptr = build_expr builder local_vars arr2_se in
      (* get the length *)
      let elem_ty, len = 
        (match arr1_se with
          | (A.Arr1D (elem_ty, n), _) -> elem_ty, n
          | _ -> raise (Failure "build_arr1d_div: not an array1D")
        ) in

      (* allocate result [len x i32] *)
      let llvm_arr_ty = ltype_of_typ (A.Arr1D (elem_ty, len)) in
      let res_ptr = L.build_alloca llvm_arr_ty "arr1d_div_res" builder in

      (* loop *)
      for i = 0 to len - 1 do
        let idx = L.const_int i32_t i in
        (* load element *)
        let src_gep =
          L.build_in_bounds_gep arr1_ptr
            [| L.const_int i32_t 0; idx |]
            "src_gep" builder
        in
        let v1 = L.build_load src_gep "src_val1" builder in
        let src2_gep =
          L.build_in_bounds_gep arr2_ptr
            [| L.const_int i32_t 0; idx |]
            "src2_gep" builder
        in
        let v2 = L.build_load src2_gep "src_val2" builder in

        (* quotient *)
        let quotient =
          (match elem_ty with
          | A.Int -> L.build_sdiv v1 v2 "elt_div" builder
          | A.Double -> L.build_fdiv v1 v2 "elt_div" builder
          | _ -> raise (Failure "build_arr1d_div: not an int or double")
          )
        in
        
        (* store *)
        let dst_gep =
          L.build_in_bounds_gep res_ptr
            [| L.const_int i32_t 0; idx |]
            "dst_gep" builder
        in
        ignore (L.build_store quotient dst_gep builder)
      done;

      (* return the ptr to the new array *)
      res_ptr
    and 
    
    build_arr2d_add builder local_vars arr1_se arr2_se = 
      (* get ptrs and values *)
      let arr1_ptr = build_expr builder local_vars arr1_se in
      let arr2_ptr = build_expr builder local_vars arr2_se in

      let elem_ty, m, n = 
        (match arr1_se with
          | (A.Arr2D (elem_ty, m, n), _) -> elem_ty, m, n
          | _ -> raise (Failure "build_arr2d_add: not an array2D")
        ) in

      (* allocate result [len x i32] *)
      let llvm_arr_ty = ltype_of_typ (A.Arr2D (elem_ty, m, n)) in
      let res_ptr = L.build_alloca llvm_arr_ty "arr2d_add_res" builder in

      (* loop *)
      for i = 0 to m - 1 do
        let i_idx = L.const_int i32_t i in
        (* pointer to row i of source 1 *)
        let src1_row_ptr =
          L.build_in_bounds_gep arr1_ptr
            [| L.const_int i32_t 0; i_idx |]
            "src_row1" builder
        in

        (* pointer to row i of source 2 *)
        let src2_row_ptr =
          L.build_in_bounds_gep arr2_ptr
            [| L.const_int i32_t 0; i_idx |]
            "src_row2" builder
        in

        (* pointer to row i of dest *)
        let dst_row_ptr =
          L.build_in_bounds_gep res_ptr
            [| L.const_int i32_t 0; i_idx |]
            "dst_row" builder
        in
    
        for j = 0 to n - 1 do
          let j_idx = L.const_int i32_t j in
    
          (* load src1[i][j] *)
          let src1_elem_gep =
            L.build_in_bounds_gep src1_row_ptr
              [| L.const_int i32_t 0; j_idx |]
              "src_gep1" builder
          in
          let v1 = L.build_load src1_elem_gep "src_val1" builder in

          (* load src2[i][j] *)
          let src2_elem_gep =
            L.build_in_bounds_gep src2_row_ptr
              [| L.const_int i32_t 0; j_idx |]
              "src_gep2" builder
          in    
          let v2 = L.build_load src2_elem_gep "src_val2" builder in

          (* v1 + v2 *)
          let sum =
            (match elem_ty with
            | A.Int -> L.build_add v1 v2 "elt_add" builder
            | A.Double -> L.build_fadd v1 v2 "elt_add" builder
            | _ -> raise (Failure "build_arr2d_add: not an int or double")
            )
          in
    
          (* store into dest[i][j] *)
          let dst_elem_gep =
            L.build_in_bounds_gep dst_row_ptr
              [| L.const_int i32_t 0; j_idx |]
              "dst_gep" builder
          in
          ignore (L.build_store sum dst_elem_gep builder)
        done
      done;
    
      (* return the ptr to the new array *)
      res_ptr
    and 
    
    build_arr2d_sub builder local_vars arr1_se arr2_se =
      (* get ptrs and values *)
      let arr1_ptr = build_expr builder local_vars arr1_se in
      let arr2_ptr = build_expr builder local_vars arr2_se in

      let elem_ty, m, n = 
        (match arr1_se with
          | (A.Arr2D (elem_ty, m, n), _) -> elem_ty, m, n
          | _ -> raise (Failure "build_arr2d_sub: not an array2D")
        ) in

      (* allocate result [len x i32] *)
      let llvm_arr_ty = ltype_of_typ (A.Arr2D (elem_ty, m, n)) in
      let res_ptr = L.build_alloca llvm_arr_ty "arr2d_sub_res" builder in

      (* loop *)
      for i = 0 to m - 1 do
        let i_idx = L.const_int i32_t i in
        (* pointer to row i of source 1 *)
        let src1_row_ptr =
          L.build_in_bounds_gep arr1_ptr
            [| L.const_int i32_t 0; i_idx |]
            "src_row1" builder
        in

        (* pointer to row i of source 2 *)
        let src2_row_ptr =
          L.build_in_bounds_gep arr2_ptr
            [| L.const_int i32_t 0; i_idx |]
            "src_row2" builder
        in

        (* pointer to row i of dest *)
        let dst_row_ptr =
          L.build_in_bounds_gep res_ptr
            [| L.const_int i32_t 0; i_idx |]
            "dst_row" builder
        in
    
        for j = 0 to n - 1 do
          let j_idx = L.const_int i32_t j in
    
          (* load src1[i][j] *)
          let src1_elem_gep =
            L.build_in_bounds_gep src1_row_ptr
              [| L.const_int i32_t 0; j_idx |]
              "src_gep1" builder
          in
          let v1 = L.build_load src1_elem_gep "src_val1" builder in

          (* load src2[i][j] *)
          let src2_elem_gep =
            L.build_in_bounds_gep src2_row_ptr
              [| L.const_int i32_t 0; j_idx |]
              "src_gep2" builder
          in    
          let v2 = L.build_load src2_elem_gep "src_val2" builder in

          (* v1 - v2 *)
          let diff =
            (match elem_ty with
            | A.Int -> L.build_add v1 v2 "elt_add" builder
            | A.Double -> L.build_fadd v1 v2 "elt_add" builder
            | _ -> raise (Failure "build_arr2d_sub: not an int or double")
            )
          in
    
          (* store into dest[i][j] *)
          let dst_elem_gep =
            L.build_in_bounds_gep dst_row_ptr
              [| L.const_int i32_t 0; j_idx |]
              "dst_gep" builder
          in
          ignore (L.build_store diff dst_elem_gep builder)
        done
      done;
    
      (* return the ptr to the new array *)
      res_ptr

    and 
    
    build_arr2d_mult builder local_vars arr1_se arr2_se =
      (* get ptrs and values *)
      let arr1_ptr = build_expr builder local_vars arr1_se in
      let arr2_ptr = build_expr builder local_vars arr2_se in

      let elem_ty, m, n = 
        (match arr1_se with
          | (A.Arr2D (elem_ty, m, n), _) -> elem_ty, m, n
          | _ -> raise (Failure "build_arr2d_mult: not an array2D")
        ) in

      (* allocate result [len x i32] *)
      let llvm_arr_ty = ltype_of_typ (A.Arr2D (elem_ty, m, n)) in
      let res_ptr = L.build_alloca llvm_arr_ty "arr2d_mul_res" builder in

      (* loop *)
      for i = 0 to m - 1 do
        let i_idx = L.const_int i32_t i in
        (* pointer to row i of source 1 *)
        let src1_row_ptr =
          L.build_in_bounds_gep arr1_ptr
            [| L.const_int i32_t 0; i_idx |]
            "src_row1" builder
        in

        (* pointer to row i of source 2 *)
        let src2_row_ptr =
          L.build_in_bounds_gep arr2_ptr
            [| L.const_int i32_t 0; i_idx |]
            "src_row2" builder
        in

        (* pointer to row i of dest *)
        let dst_row_ptr =
          L.build_in_bounds_gep res_ptr
            [| L.const_int i32_t 0; i_idx |]
            "dst_row" builder
        in
    
        for j = 0 to n - 1 do
          let j_idx = L.const_int i32_t j in
    
          (* load src1[i][j] *)
          let src1_elem_gep =
            L.build_in_bounds_gep src1_row_ptr
              [| L.const_int i32_t 0; j_idx |]
              "src_gep1" builder
          in
          let v1 = L.build_load src1_elem_gep "src_val1" builder in

          (* load src2[i][j] *)
          let src2_elem_gep =
            L.build_in_bounds_gep src2_row_ptr
              [| L.const_int i32_t 0; j_idx |]
              "src_gep2" builder
          in    
          let v2 = L.build_load src2_elem_gep "src_val2" builder in

          (* v1 * v2 *)
          let prod =
            (match elem_ty with
            | A.Int -> L.build_mul v1 v2 "elt_add" builder
            | A.Double -> L.build_fmul v1 v2 "elt_add" builder
            | _ -> raise (Failure "build_arr2d_mult: not an int or double")
            )
          in
    
          (* store into dest[i][j] *)
          let dst_elem_gep =
            L.build_in_bounds_gep dst_row_ptr
              [| L.const_int i32_t 0; j_idx |]
              "dst_gep" builder
          in
          ignore (L.build_store prod dst_elem_gep builder)
        done
      done;
    
      (* return the ptr to the new array *)
      res_ptr
    and

    build_arr2d_div builder local_vars arr1_se arr2_se = 
      (* get ptrs and values *)
      let arr1_ptr = build_expr builder local_vars arr1_se in
      let arr2_ptr = build_expr builder local_vars arr2_se in

      let elem_ty, m, n = 
        (match arr1_se with
          | (A.Arr2D (elem_ty, m, n), _) -> elem_ty, m, n
          | _ -> raise (Failure "build_arr2d_div: not an array2D")
        ) in

      (* allocate result [len x i32] *)
      let llvm_arr_ty = ltype_of_typ (A.Arr2D (elem_ty, m, n)) in
      let res_ptr = L.build_alloca llvm_arr_ty "arr2d_div_res" builder in

      (* loop *)
      for i = 0 to m - 1 do
        let i_idx = L.const_int i32_t i in
        (* pointer to row i of source 1 *)
        let src1_row_ptr =
          L.build_in_bounds_gep arr1_ptr
            [| L.const_int i32_t 0; i_idx |]
            "src_row1" builder
        in

        (* pointer to row i of source 2 *)
        let src2_row_ptr =
          L.build_in_bounds_gep arr2_ptr
            [| L.const_int i32_t 0; i_idx |]
            "src_row2" builder
        in

        (* pointer to row i of dest *)
        let dst_row_ptr =
          L.build_in_bounds_gep res_ptr
            [| L.const_int i32_t 0; i_idx |]
            "dst_row" builder
        in
    
        for j = 0 to n - 1 do
          let j_idx = L.const_int i32_t j in
    
          (* load src1[i][j] *)
          let src1_elem_gep =
            L.build_in_bounds_gep src1_row_ptr
              [| L.const_int i32_t 0; j_idx |]
              "src_gep1" builder
          in
          let v1 = L.build_load src1_elem_gep "src_val1" builder in

          (* load src2[i][j] *)
          let src2_elem_gep =
            L.build_in_bounds_gep src2_row_ptr
              [| L.const_int i32_t 0; j_idx |]
              "src_gep2" builder
          in    
          let v2 = L.build_load src2_elem_gep "src_val2" builder in

          (* v1 / v2 *)
          let quotient =
            match elem_ty with
            | A.Int    -> L.build_sdiv  v1 v2 "elt_div" builder
            | A.Double -> L.build_fdiv  v1 v2 "elt_div" builder
            | _       -> raise (Failure "build_arr2d_div: not an int or double")
          in
    
          (* store into dest[i][j] *)
          let dst_elem_gep =
            L.build_in_bounds_gep dst_row_ptr
              [| L.const_int i32_t 0; j_idx |]
              "dst_gep" builder
          in
          ignore (L.build_store quotient dst_elem_gep builder)
        done
      done;
    
      (* return the ptr to the new array *)
      res_ptr
    and 
    
    build_arr_1d_int_mult builder local_vars arr_se int_se =
      (* get ptrs and values *)
      let arr_ptr = build_expr builder local_vars arr_se in
      let int_val = build_expr builder local_vars int_se in
      let len = 
        (match arr_se with
          | (A.Arr1D (_, n), _) -> n
          | _ -> raise (Failure "build_arr_1d_int_mult: not a 1D array")
        ) in
      (* allocate result [len x i32] *)
      let llvm_arr_ty = ltype_of_typ (A.Arr1D (A.Int, len)) in
      let res_ptr = L.build_alloca llvm_arr_ty "arr_mul_res" builder in

      (* loop *)
      for i = 0 to len - 1 do
        let idx = L.const_int i32_t i in
        (* load element *)
        let src_gep =
          L.build_in_bounds_gep arr_ptr
            [| L.const_int i32_t 0; idx |]
            "src_gep" builder
        in
        let v = L.build_load src_gep "src_val" builder in

        (* multiply *)
        let prod = L.build_mul v int_val "elt_mul" builder in

        (* store *)
        let dst_gep =
          L.build_in_bounds_gep res_ptr
            [| L.const_int i32_t 0; idx |]
            "dst_gep" builder
        in
        ignore (L.build_store prod dst_gep builder)
      done;

      (* return the ptr to the new array *)
      res_ptr
    and

    build_arr_2d_int_mult builder local_vars arr_se int_se =
      (* get ptrs and values *)
      let arr_ptr = build_expr builder local_vars arr_se in
      let int_val = build_expr builder local_vars int_se in
      let m, n = 
        (match arr_se with
          | (A.Arr2D (_, m, n), _) -> m, n
          | _ -> raise (Failure "build_arr_2d_int_mult: not an array2D")
        ) in

      (* allocate result [len x i32] *)
      let llvm_arr_ty = ltype_of_typ (A.Arr2D (A.Int, m, n)) in
      let res_ptr = L.build_alloca llvm_arr_ty "arr2d_mul_res" builder in

      (* loop *)
      for i = 0 to m - 1 do
        let i_idx = L.const_int i32_t i in
        (* pointer to row i of source *)
        let src_row_ptr =
          L.build_in_bounds_gep arr_ptr
            [| L.const_int i32_t 0; i_idx |]
            "src_row" builder
        in
        (* pointer to row i of dest *)
        let dst_row_ptr =
          L.build_in_bounds_gep res_ptr
            [| L.const_int i32_t 0; i_idx |]
            "dst_row" builder
        in
    
        for j = 0 to n - 1 do
          let j_idx = L.const_int i32_t j in
    
          (* load src[i][j] *)
          let src_elem_gep =
            L.build_in_bounds_gep src_row_ptr
              [| L.const_int i32_t 0; j_idx |]
              "src_gep" builder
          in
          let v = L.build_load src_elem_gep "src_val" builder in
    
          (* mul v * scalar *)
          let prod =
            L.build_mul v int_val "elt_mul" builder
          in
    
          (* store into dest[i][j] *)
          let dst_elem_gep =
            L.build_in_bounds_gep dst_row_ptr
              [| L.const_int i32_t 0; j_idx |]
              "dst_gep" builder
          in
          ignore (L.build_store prod dst_elem_gep builder)
        done
      done;
    
      (* return the ptr to the new array *)
      res_ptr
    and

    build_arr_1d_double_mult builder local_vars arr_se double_se =
      (* get ptrs and values *)
      let arr_ptr = build_expr builder local_vars arr_se in
      let double_val = build_expr builder local_vars double_se in
      let len = 
        (match arr_se with
          | (A.Arr1D (_, n), _) -> n
          | _ -> raise (Failure "build_arr_1d_double_mult: not a 1D array")
        ) in

      (* allocate result [len x i32] *)
      let llvm_arr_ty = ltype_of_typ (A.Arr1D (A.Double, len)) in
      let res_ptr = L.build_alloca llvm_arr_ty "arr_mul_res" builder in

      (* loop *)
      for i = 0 to len - 1 do
        let idx = L.const_int i32_t i in
        (* load element *)
        let src_gep =
          L.build_in_bounds_gep arr_ptr
            [| L.const_int i32_t 0; idx |]
            "src_gep" builder
        in
        let v = L.build_load src_gep "src_val" builder in

        (* multiply *)
        let prod = L.build_fmul v double_val "elt_mul" builder in

        (* store *)
        let dst_gep =
          L.build_in_bounds_gep res_ptr
            [| L.const_int i32_t 0; idx |]
            "dst_gep" builder
        in
        ignore (L.build_store prod dst_gep builder)
      done;

      (* return the ptr to the new array *)
      res_ptr
    and

    build_arr_2d_double_mult builder local_vars arr_se double_se =
       (* get ptrs and values *)
       let arr_ptr = build_expr builder local_vars arr_se in
       let double_val = build_expr builder local_vars double_se in
       let m, n = 
        (match arr_se with
          | (A.Arr2D (_, m, n), _) -> m, n
          | _ -> raise (Failure "build_arr_2d_double_mult: not an 2D array")
        ) in
 
       (* allocate result [m x n x i32] *)
       let llvm_arr_ty = ltype_of_typ (A.Arr2D (A.Double, m, n)) in
       let res_ptr = L.build_alloca llvm_arr_ty "arr2d_mul_res" builder in
 
       (* loop *)
       for i = 0 to m - 1 do
         let i_idx = L.const_int i32_t i in
         (* pointer to row i of source *)
         let src_row_ptr =
           L.build_in_bounds_gep arr_ptr
             [| L.const_int i32_t 0; i_idx |]
             "src_row" builder
         in
         (* pointer to row i of dest *)
         let dst_row_ptr =
           L.build_in_bounds_gep res_ptr
             [| L.const_int i32_t 0; i_idx |]
             "dst_row" builder
         in
     
         for j = 0 to n - 1 do
           let j_idx = L.const_int i32_t j in
     
           (* load src[i][j] *)
           let src_elem_gep =
             L.build_in_bounds_gep src_row_ptr
               [| L.const_int i32_t 0; j_idx |]
               "src_gep" builder
           in
           let v = L.build_load src_elem_gep "src_val" builder in
     
           (* mul v * scalar *)
           let prod =
             L.build_fmul v double_val "elt_mul" builder
           in
     
           (* store into dest[i][j] *)
           let dst_elem_gep =
             L.build_in_bounds_gep dst_row_ptr
               [| L.const_int i32_t 0; j_idx |]
               "dst_gep" builder
           in
           ignore (L.build_store prod dst_elem_gep builder)
         done
       done;
     
       (* return the ptr to the new array *)
       res_ptr
    and

    build_arr2d_mmult builder local_vars (ty1, sx1) (ty2, sx2) =
      (* get ptrs to arrays *)
      let arr1_ptr = build_expr builder local_vars (ty1, sx1)in
      let arr2_ptr = build_expr builder local_vars (ty2, sx2) in
      let elem_ty, m, n =
        (match ty1 with
          | A.Arr2D(elem_ty, m, n) -> elem_ty, m, n
          | _ -> raise (Failure "build_arr2d_mmult: not a 2D array: ty1")
        ) in

      let p =
        (match ty2 with
          | A.Arr2D(_, _, p) -> p
          | _ -> raise (Failure "build_arr2d_mmult: not a 2D array: ty2")
        ) in
        
      (* allocate result [m x p x i32] *)
      let llvm_arr_ty = ltype_of_typ (A.Arr2D (elem_ty, m, p)) in
      let res_ptr = L.build_alloca llvm_arr_ty "arr2d_mmult_res" builder in

      (* a zero value for the accumulator *)
      let zero_acc = match elem_ty with
        | A.Int    -> L.const_int  i32_t 0
        | A.Double -> L.const_float double_t 0.0
        | _        -> failwith "matrix-mul on non-numeric type"
      in

      (* triple nested loop to do the matrix mult *)
      for i = 0 to m - 1 do
        let i_idx = L.const_int i32_t i in
    
        for j = 0 to p - 1 do
          let j_idx = L.const_int i32_t j in
    
          (* accumulator register in OCaml *)
          let acc = ref zero_acc in
    
          (* sum over k *)
          for k = 0 to n - 1 do
            let k_idx = L.const_int i32_t k in
    
            (* load a = arr1[i][k] *)
            let a_ptr =
              L.build_in_bounds_gep arr1_ptr
                [| L.const_int i32_t 0; i_idx; k_idx |]
                "mm_a_gep" builder
            in
            let a_val = L.build_load a_ptr "mm_a" builder in
    
            (* load b = arr2[k][j] *)
            let b_ptr =
              L.build_in_bounds_gep arr2_ptr
                [| L.const_int i32_t 0; k_idx; j_idx |]
                "mm_b_gep" builder
            in
            let b_val = L.build_load b_ptr "mm_b" builder in
    
            (* prod = a * b *)
            let prod = match elem_ty with
              | A.Int    -> L.build_mul  a_val b_val "mm_mul" builder
              | A.Double -> L.build_fmul a_val b_val "mm_fmul" builder
              | _        -> assert false
            in
    
            (* acc = acc + prod *)
            let sum = match elem_ty with
              | A.Int    -> L.build_add  !acc prod "mm_add"  builder
              | A.Double -> L.build_fadd !acc prod "mm_fadd" builder
              | _        -> assert false
            in
    
            acc := sum
          done;
          
          (* store the final acc into res[i][j] *)
          let dst_ptr =
            L.build_in_bounds_gep res_ptr
              [| L.const_int i32_t 0; i_idx; j_idx |]
              "mm_res_gep" builder
          in
          ignore (L.build_store !acc dst_ptr builder)
        done
      done;

      (* return the ptr to the new array *)
      res_ptr
    and

    build_arr_2d_transpose builder local_vars (ty, sx) =
      (* get the array ptr we're transposing *)
      let arr_ptr = build_expr builder local_vars (ty, sx) in
      let elem_ty, m, n =
        (match ty with
          | A.Arr2D(elem_ty, m, n) -> elem_ty, m, n
          | _ -> raise (Failure "build_arr_2d_transpose: not a 2D array: ")
        ) in
      
      (* get array types *)
      let llvm_out_ty = ltype_of_typ (A.Arr2D(elem_ty, n, m)) in

      (* allocate new transposed array on stack*)
      let res_ptr = L.build_alloca llvm_out_ty "arr2d_transpose" builder in

      (* loop to transpose elements *)
      for i = 0 to m - 1 do
        for j = 0 to n - 1 do
          (* a) load element (i,j) from input *)
          let src_gep =
            L.build_in_bounds_gep arr_ptr
              [| L.const_int i32_t 0
               ; L.const_int i32_t i
               ; L.const_int i32_t j
              |]
              "src_gep" builder
          in
          let v = L.build_load src_gep "src" builder in
    
          (* b) store it into (j,i) of result *)
          let dst_gep =
            L.build_in_bounds_gep res_ptr
              [| L.const_int i32_t 0
               ; L.const_int i32_t j
               ; L.const_int i32_t i
              |]
              "dst_gep" builder
          in
          ignore (L.build_store v dst_gep builder)
        done
      done;
    
      (* return ptr to transposed array *)
      res_ptr
    and


    build_map_arr_1d builder local_vars (ty, sx) func_name =
      (* get ptrs and values *)
      let arr_ptr = build_expr builder local_vars (ty, sx) in
      let (fn_val, _) = StringMap.find func_name function_decls in
      let elem_ty, n =
        (match ty with
          | A.Arr1D(elem_ty, n) -> elem_ty, n
          | _ -> raise (Failure "build_map_arr_1d: not a 1D array: ty: ")
        ) in

      (* allocate result [len x i32] *)
      let llvm_arr_ty = ltype_of_typ ty in
      let res_ptr = L.build_alloca llvm_arr_ty (func_name ^ "_map2d") builder in

      (* loop *)
      for i = 0 to n - 1 do
        let idx = L.const_int i32_t i in
        (* load element *)
        let src_gep =
          L.build_in_bounds_gep arr_ptr
            [| L.const_int i32_t 0; idx |]
            "src_gep" builder
        in
        let v = L.build_load src_gep "src_val" builder in

        (* apply the map and get a value *)
        let mapped  = L.build_call fn_val [| v |] (func_name ^ "_call") builder in

        (* store *)
        let dst_gep =
          L.build_in_bounds_gep res_ptr
            [| L.const_int i32_t 0; idx |]
            "dst_gep" builder
        in
        ignore (L.build_store mapped dst_gep builder)
      done;

      (* return the ptr to the new array *)
      res_ptr
    and


    build_map_arr_2d builder local_vars (ty, sx) func_name  =
      (* get ptrs and values *)
      let arr_ptr = build_expr builder local_vars (ty, sx) in
      let (fn_val, _) = StringMap.find func_name function_decls in
      let elem_ty, m, n =
        (match ty with
          | A.Arr2D(elem_ty, m, n) -> elem_ty, m, n
          | _ -> raise (Failure "build_map_arr_2d: not a 2D array: ty: ")
        ) in

      (* allocate result [len x i32] *)
      let llvm_arr_ty = ltype_of_typ ty in
      let res_ptr = L.build_alloca llvm_arr_ty (func_name ^ "_map2d") builder in

       (* loop *)
       for i = 0 to m - 1 do
        let i_idx = L.const_int i32_t i in
        (* pointer to row i of source *)
        let src_row_ptr =
          L.build_in_bounds_gep arr_ptr
            [| L.const_int i32_t 0; i_idx |]
            "src_row" builder
        in
        (* pointer to row i of dest *)
        let dst_row_ptr =
          L.build_in_bounds_gep res_ptr
            [| L.const_int i32_t 0; i_idx |]
            "dst_row" builder
        in
    
        for j = 0 to n - 1 do
          let j_idx = L.const_int i32_t j in
    
          (* load src[i][j] *)
          let src_elem_gep =
            L.build_in_bounds_gep src_row_ptr
              [| L.const_int i32_t 0; j_idx |]
              "src_gep" builder
          in
          let v = L.build_load src_elem_gep "src_val" builder in
    
          (* apply the map and get a value *)
          let mapped  = L.build_call fn_val [| v |] (func_name ^ "_call") builder in
    
          (* store into dest[i][j] *)
          let dst_elem_gep =
            L.build_in_bounds_gep dst_row_ptr
              [| L.const_int i32_t 0; j_idx |]
              "dst_gep" builder
          in
          ignore (L.build_store mapped dst_elem_gep builder)
        done
      done;

      res_ptr

    and

    build_reduce_arr_1d builder local_vars (ty, sx) func_name =
      (* get ptrs and values *)
      let arr_ptr = build_expr builder local_vars (ty, sx) in
      let (fn_val, _) = StringMap.find func_name function_decls in
      let elem_ty, len =
        (match ty with
          | A.Arr1D(elem_ty, n) -> elem_ty, n
          | _ -> raise (Failure "build_reduce_arr_1d: not a 1D array: ty: ")
        ) in

      (* allocate accumulator with initial value of arr_ptr[0] *)
      let zero = L.const_int i32_t 0 in
      let gep0 = L.build_in_bounds_gep arr_ptr [| zero; zero |] "red1d_0_gep" builder in
      let acc = ref (L.build_load gep0 "red1d_acc0" builder) in
    
      (* loop from 1 to len, reducing each element *)
      for i = 1 to len - 1 do
        let idx = L.const_int i32_t i in
        (* load element *)
        let gep =
          L.build_in_bounds_gep arr_ptr
            [| L.const_int i32_t 0; idx |]
            "red1d_i_gep" builder
        in
        let v = L.build_load gep "red1d_vi" builder in
        let new_acc =
          L.build_call fn_val [| !acc; v |] (func_name ^ "_call") builder
        in
        acc := new_acc
      done;
      (* return the final accumulated value *)
      !acc
    and

    build_reduce_arr_2d builder local_vars (ty, sx) func_name =
      (* get ptrs and values *)
      let arr_ptr = build_expr builder local_vars (ty, sx) in
      let (fn_val, _) = StringMap.find func_name function_decls in
      let elem_ty, m, n =
        (match ty with
          | A.Arr2D(elem_ty, m, n) -> elem_ty, m, n
          | _ -> raise (Failure "build_reduce_arr_2d: not a 2D array: ty: ")
        ) in

      (* allocate result [len x i32] *)
      let llvm_arr_ty = ltype_of_typ (A.Arr1D(elem_ty, m)) in
      let res_ptr = L.build_alloca llvm_arr_ty (func_name ^ "_reduce2d") builder in
      let zero = L.const_int i32_t 0 in

      (* loop over all rows, for each row loop from 1 to length, reducing the elems in each row *)
      for i = 0 to m - 1 do
        (* start-of-row accumulator = arr[i][0] *)
        let i_idx = L.const_int i32_t i in
        let gep00 = L.build_in_bounds_gep arr_ptr [| zero; i_idx; zero |] "red2d_src00" builder in
        let acc_row0 = L.build_load gep00 "red2d_acc00" builder in
        (* accumulate row arr[i] into value dst[i] *)
        let dst0 =
          L.build_in_bounds_gep res_ptr
            [| zero; i_idx |]
            "reduce2d_dst0" builder
        in
        ignore (L.build_store acc_row0 dst0 builder);
        let acc = ref acc_row0 in

        (* j = 1..n-1: acc = f(acc, arr[i][j]); out[i][j] = acc *)
        for j = 1 to n - 1 do
          let j_idx = L.const_int i32_t j in
          let gepij =
            L.build_in_bounds_gep arr_ptr [| zero; i_idx; j_idx |] "red2d_srcij" builder
          in
          let v = L.build_load gepij "red2d_vij" builder in
          let new_acc =
            L.build_call fn_val [| !acc; v |] (func_name ^ "_call") builder
          in
          acc := new_acc;
        done;

        (* store the final accumulator into res[i] *)
        let dst_i =
          L.build_in_bounds_gep res_ptr
            [| zero; i_idx |]
            "reduce2d_dsti" builder
        in
        ignore (L.build_store !acc dst_i builder)
      done;
    
      (* return ptr to the new array of reduced rows *)
      res_ptr
    and
    
    build_arr_1d_slice builder local_vars id ty start_index =
      let arr_ptr = lookup id local_vars true builder in
      let elem_ty, len =
        (match ty with
          | A.Arr1D(elem_ty, n) -> elem_ty, n
          | _ -> raise (Failure "build_arr_1d_slice: not a 1D array: ty: ")
        ) in
      
      (* allocate result [len x type_size] *)
      let llvm_arr_ty = ltype_of_typ (Arr1D(elem_ty, len)) in
      let res_ptr = L.build_alloca llvm_arr_ty ("_slice1d") builder in
      let zero   = L.const_int i32_t 0 in
      
      (* loop to copy elements *)
      for i = 0 to len - 1 do
        (* get element at arr[start_index+i] *)
        let src_gep = L.build_in_bounds_gep arr_ptr
            [| zero; L.const_int i32_t (start_index + i) |]
            "slice1d_src" builder
        in
        let v = L.build_load src_gep "slice1d_v" builder in
        (* store at dest[i]*)
        let dst_gep = L.build_in_bounds_gep res_ptr
            [| zero; L.const_int i32_t i |]
            "slice1d_dst" builder
        in
        ignore (L.build_store v dst_gep builder)
      done;
      
      (* return pointer to new array *)
      res_ptr
    and

    build_arr_2d_slice builder local_vars id ty start_row_index start_col_index = 
      let arr_ptr = lookup id local_vars true builder in
      let elem_ty, m, n =
      (match ty with
        | A.Arr2D(elem_ty, m, n) -> elem_ty, m, n
        | _ -> raise (Failure "build_arr_2d_slice: not a 2D array: ty: ")
      ) in

      (* allocate result [m x type size][n x type size] *)
      let llvm_arr_ty = ltype_of_typ (Arr2D(elem_ty, m, n)) in
      let res_ptr = L.build_alloca llvm_arr_ty ("_slice2d") builder in
      let zero   = L.const_int i32_t 0 in

      (* loop to copy elements *)
      for i = 0 to m - 1 do
        for j = 0 to n - 1 do
          (* get element at arr[m+i][n+j] *)
          let src_gep = L.build_in_bounds_gep arr_ptr
              [| zero
               ; L.const_int i32_t (start_row_index + i)
               ; L.const_int i32_t (start_col_index + j)
              |]
              "slice2d_src" builder
          in
          let v = L.build_load src_gep "slice2d_v" builder in
          (* store at dest[i][j]*)
          let dst_gep = L.build_in_bounds_gep res_ptr
              [| zero
               ; L.const_int i32_t i
               ; L.const_int i32_t j
              |]
              "slice2d_dst" builder
          in
          ignore (L.build_store v dst_gep builder)
        done
      done;

      (* return pointer to new array *)
      res_ptr
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
    let rec build_stmt (builder, local_vars, we_bb, outer_bb) = function
        SBlock sl -> 
          let build_stmts (builder', local_vars') = 
            build_stmt (builder', local_vars', we_bb, outer_bb) in 
          (fst (List.fold_left build_stmts (builder, local_vars) sl), local_vars)
      | SExpr e -> ignore(build_expr builder local_vars e); (builder, local_vars)
      | SReturn e -> ignore(match fdecl.srtyp with
           A.Void -> L.build_ret_void builder
          | _ -> L.build_ret (build_expr builder local_vars e) builder);
          (builder, local_vars)
      | SIf (predicate, then_stmt, else_stmt) ->
        let bool_val = build_expr builder local_vars predicate in

        let then_bb = L.append_block context "then" the_function in
        let else_bb = L.append_block context "else" the_function in
        let end_bb = L.append_block context "if_end" the_function in

        ignore (build_stmt ((L.builder_at_end context then_bb), local_vars, we_bb, Some(end_bb)) then_stmt);
        ignore (build_stmt ((L.builder_at_end context else_bb), local_vars, we_bb, Some(end_bb))  else_stmt);

        let build_br_end = L.build_br end_bb in (* partial function *)
        add_terminal (L.builder_at_end context then_bb) build_br_end;
        add_terminal (L.builder_at_end context else_bb) build_br_end;

        (match outer_bb with
        | Some(bb) -> add_terminal (L.builder_at_end context end_bb) (fun b -> L.build_br bb b)
        | None -> ()
        );

        ignore(L.build_cond_br bool_val then_bb else_bb builder);
        (L.builder_at_end context end_bb, local_vars)

        (*
        ignore(L.build_cond_br bool_val then_bb else_bb builder);
        L.builder_at_end context end_bb
        *)
      | SFor (s1, e1, e2, body) ->
        let builder1, local_vars' = build_stmt (builder, local_vars, we_bb, outer_bb) s1 in
        let s2 = SWhile (e1, SBlock [body ; SExpr e2]) in
        let builder2, _ = build_stmt (builder1, local_vars', we_bb, outer_bb) s2 in
        (builder2, local_vars)        
      | SWhile (predicate, body) ->
        let while_bb = L.append_block context "while" the_function in
        let build_br_while = L.build_br while_bb in (* partial function *)
        ignore (build_br_while builder);
        let while_builder = L.builder_at_end context while_bb in
        let bool_val = build_expr while_builder local_vars predicate in

        let body_bb = L.append_block context "while_body" the_function in
        let end_bb = L.append_block context "while_end" the_function in

        add_terminal (fst (build_stmt ((L.builder_at_end context body_bb), 
                                        local_vars, Some(end_bb), Some(end_bb)) body)) build_br_while;

        (match outer_bb with
        | Some(bb) -> add_terminal (L.builder_at_end context end_bb) (fun b -> L.build_br bb b)
        | None -> ()
        );                    

        ignore(L.build_cond_br bool_val body_bb end_bb while_builder);
        (L.builder_at_end context end_bb, local_vars)
      | SBreak ->
        (* we check what the end block of the while loop is to unconditionally jump to *)
        let while_end_bb = (match we_bb with 
           Some(bb) -> bb
         | None -> raise(Failure ("Break statement not in loop")))
        in ignore(L.build_br while_end_bb builder);
        (builder, local_vars)
      | SVDecl vdecl -> 
        let add_array_local local_vars name arr_typ builder =
          (* declare body type *)
          let body_ty   = ltype_of_typ arr_typ in  (* pointer to [m x [n x T]] *)
          (* allocate space on stack for body *)
          let body_ptr  = L.build_alloca body_ty name builder in
          (* let handle_ptr  = L.build_alloca handle_ty name builder in *)
          let local_vars' = StringMap.add name body_ptr local_vars in
          body_ptr, local_vars'
        in
  
        (builder, match vdecl with
        | SBindDecl(ty, n) ->
          (* Declaring an array will allocate space on the stack for the array itself and its handle.
              The handle is a pointer to the array itself, both are on the stack.
             Declaring any other type will allocate space on the stack for the type itself.
          *)
          (match ty with
            | A.Arr1D _ | A.Arr2D _ ->
                let _, local_vars' = add_array_local local_vars n ty builder in
                local_vars'
            | _ ->
                let local_var = L.build_alloca (ltype_of_typ ty) n builder
                in StringMap.add n local_var local_vars
          )
            
        | SBindInit((ty, na), e) ->
          (* When declaring and initializing an array, it will allocate space on the stack for the
              array itself and its handle. The handle is a pointer to the array itself.  It will 
              then deepcopy the array, regardless if there is another variable or an array literal on the RHS.
             When declaring and initialzing any other type, it will allocate space on the stack for
              the type itself. It will then simply store the RHS value into the allocated stack space.
          *)
          let e' = build_expr builder local_vars e
          in L.set_value_name na e';
          
          (match ty with
            | A.Arr1D(elem_ty, len) ->
              let body_ptr, local_vars' = add_array_local local_vars na ty builder in
              (* always deep-copy literals AND variable sources *)
              ignore(arr_1d_memcpy builder local_vars' len e' body_ptr);
              local_vars'
            | A.Arr2D (elem_ty, m, n) ->
              let body_ptr, local_vars' = add_array_local local_vars na ty builder in
              (* always deep-copy literals AND variable sources *)
              ignore(arr_2d_memcpy builder local_vars' m n e' body_ptr);
              local_vars'
            | _ -> (* for regular scalar types *)
              (* allocate the var, store the value, add it's addr to the map *)
              let local_var = L.build_alloca (ltype_of_typ ty) na builder in
              ignore(L.build_store e' local_var builder);
              StringMap.add na local_var local_vars
          )
        )
    (* finished build_stmt *)
    in
    (* Build the code for each statement in the function *)
    let func_builder = fst (build_stmt (builder, formal_vars, None, None) (SBlock fdecl.sbody)) in

    (* Add a return if the last block falls off the end *)
    add_terminal func_builder (L.build_ret_void)

  in

  List.iter build_function_body functions;
  the_module
