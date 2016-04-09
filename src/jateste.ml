open Printf 
module A = Ast
module S = Sast


let executable_filename filename =
	let len = String.length filename in
	let str = String.sub filename 0 (len - 3) in
	let exec = String.concat "" [str ; ".ll"] in
	exec 


let _ =
	let arguments = Sys.argv in
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
	fprintf oc "%s\n" (Llvm.string_of_llmodule m);
	print_string (Llvm.string_of_llmodule m);
	close_out oc;
	close_in source_file
