open Printf 
module A = Ast
module S = Sast

let eval_prog prog =
		match prog with
		_ ->  "Successfully parsed\n"
	
let _ =
	let lexbuf = Lexing.from_channel stdin in
	let ast:(A.program) = Parser.program Scanner.token lexbuf in
	print_string (eval_prog ast);
	let sast:(S.sprogram) = Semant.check ast in
	let file = "file.bc" in
	let oc = open_out file in
	let m = Codegen.gen_llvm sast in 
	Llvm_analysis.assert_valid_module m;
	fprintf oc "%s\n" (Llvm.string_of_llmodule m);
	print_string (Llvm.string_of_llmodule m)
