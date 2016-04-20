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


let _ =
	let arguments = Sys.argv in
	let action = determine_action arguments in
	let source_file = open_in arguments.((Array.length Sys.argv - 1)) in
	let exec_name = executable_filename arguments.((Array.length Sys.argv -1)) in
	let lexbuf = Lexing.from_channel source_file in
	(print_string "Scanned\n");
	let ast:(A.program) = Parser.program Scanner.token lexbuf in
	(print_string "Parsed\n");
	let sast:(S.sprogram) = Semant.check ast in
	(print_string "Semantic check passed\n");
	let file = exec_name in
	let oc = open_out file in
	let m = Codegen.gen_llvm sast in 
	Llvm_analysis.assert_valid_module m;
	let _ = (match action with 
	  Compile ->  fprintf oc "%s\n" (Llvm.string_of_llmodule m);
	| Compile_with_test ->  fprintf oc "%s\n" (Llvm.string_of_llmodule m);
	) in
	close_out oc;
	close_in source_file
