(* Top-level of the CAPybara compiler: scan & parse the input,
   check the resulting AST and generate an SAST from it, generate LLVM IR,
   and dump the module *)

   type action = Ast | Sast | LLVM_IR | Compile

   let () =
     let action = ref LLVM_IR in
     let set_action a () = action := a in
     let speclist = [
       ("-a", Arg.Unit (set_action Ast), "Print the AST");
       ("-s", Arg.Unit (set_action Sast), "Print the SAST");
       ("-l", Arg.Unit (set_action LLVM_IR), "Print the generated LLVM IR");
       ("-c", Arg.Unit (set_action Compile), "Dump the compiled program to the executable file")
     ] in
     let usage_msg = "usage: ./capybara.native [-a|-s|-l|-c] [file.cap]" in
     let channel = ref stdin in
     let filename = ref "" in

     (* Parsing the command-line arguments *)
     Arg.parse speclist (fun fname -> 
       channel := open_in fname; 
       filename := fname
     ) usage_msg;
   
     let lexbuf = Lexing.from_channel !channel in
   
     let ast = Capyparse.program Scanner.token lexbuf in
     match !action with
       Ast -> print_string (Ast.string_of_program ast)
     | _ -> let sast = Semant.check ast in
       match !action with
         Ast     -> ()
       | Sast    -> print_string (Sast.string_of_sprogram sast)
       | LLVM_IR -> print_string (Llvm.string_of_llmodule (Codegen.translate sast))
       | Compile -> 
          let output_filename = Filename.remove_extension !filename in
          (* open the output file and write the LLVM IR *)
          let oc = open_out output_filename in
          output_string oc (Llvm.string_of_llmodule (Codegen.translate sast));
          close_out oc;
