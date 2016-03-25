open Myprinter;;

let eval_prog prog =
		match prog with
		_ ->  "Successfully parsed\n"
	
let _ =
	let lexbuf = Lexing.from_channel stdin in
	let ast = Parser.program Scanner.token lexbuf in

	print_string (eval_prog ast);
	Semant.check ast;
	let m = Codegen.translate ast in 
	Llvm_analysis.assert_valid_module m;
	print_string (Llvm.string_of_llmodule m) 
