open Printf 

let eval_prog prog =
		match prog with
		_ ->  "Successfully parsed\n"
	
let _ =
	let lexbuf = Lexing.from_channel stdin in
	let ast = Parser.program Scanner.token lexbuf in

	print_string (eval_prog ast);
	Semant.check ast;
	let file = "file.bc" in
	let oc = open_out file in
	let m = Codegen.gen_llvm ast in 
	Llvm_analysis.assert_valid_module m;
	fprintf oc "%s\n" (Llvm.string_of_llmodule m);
	print_string (Llvm.string_of_llmodule m)
