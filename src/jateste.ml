open Printf 
module A = Ast
module S = Sast


type action = Compile | Compile_with_test

let determine_action args = 
	let num_args = Array.length args in
	(match num_args with
	  1 -> raise Exceptions.IllegalInputFormat
	| 2 -> Compile
	| 3 -> let arg = Array.get args 1 in 
		(match arg with
		  "-t" -> Compile_with_test
		| _ -> raise (Exceptions.IllegalArgument arg)
		)
	
	| _ -> Compile
	)

let executable_filename filename =
	let len = String.length filename in
	let str = String.sub filename 0 (len - 3) in
	let exec = String.concat "" [str ; ".ll"] in
	exec 

let test_executable_filename filename =
	let len = String.length filename in
	let str = String.sub filename 0 (len - 3) in
	let exec = String.concat "" [str ; "-test.ll"] in
	exec 

let _ =
	(* Read in command line args *)
	let arguments = Sys.argv in
	let action = determine_action arguments in
	let source_file = open_in arguments.((Array.length Sys.argv - 1)) in
	(* Create a file to put executable in *)
	let exec_name = executable_filename arguments.((Array.length Sys.argv -1)) in
	let test_exec_name = test_executable_filename arguments.((Array.length Sys.argv -1)) in
	(* Start scanning *)
	let lexbuf = Lexing.from_channel source_file in
	(print_string "Scanned\n");
	(* Parse the scanned source code, and build an AST *)
	let ast:(A.program) = Parser.program Scanner.token lexbuf in
	(print_string "Parsed\n");
	(* Run semantic checker, and return as SAST(Semantic AST) *)
	let sast:(S.sprogram) = Semant.check ast in
	(print_string "Semantic check passed\n");
	let file = exec_name in
	let test_file = test_exec_name in
	let oc = open_out file in
	let test_oc = open_out test_file in
	let m = Codegen.gen_llvm sast false in 
	Llvm_analysis.assert_valid_module m;
	fprintf oc "%s\n" (Llvm.string_of_llmodule m); 
	close_out oc;
	let _ = (match action with 
	  Compile ->  ()
	| Compile_with_test -> let test_m = (Codegen.gen_llvm sast true) in ignore(Llvm_analysis.assert_valid_module test_m); fprintf test_oc "%s\n" (Llvm.string_of_llmodule test_m); close_out test_oc;
	) in
	close_out test_oc;
	close_in source_file
