open Myprinter;;

let eval_prog prog =
		match prog with
		_ ->  "Successfully parsed\n"
	
let _ =
	let lexbuf = Lexing.from_channel stdin in
	let ast = Parser.program Scanner.token lexbuf in
	print_string (eval_prog ast);
	print_string (string_of_program 0 ast);
	(* print_string (Ast.string_of_program ast) *)

