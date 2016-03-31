open Ast

let my_print list_in = 
let rec my_print_newline list = 
	match list with
	[] -> ()
	| Literal(x) :: tail -> print_string x;
	print_string " "; my_print_newline tail
	in  
	my_print_newline list_in; print_endline "";;

	let _ =
	let lexbuf = Lexing.from_channel stdin in
	let expr = Parser.program Scanner.token lexbuf in
		my_print expr
