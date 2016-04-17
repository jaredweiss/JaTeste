open Ast;;
open Myprinter;;


(*
let rec eval_expr = function
	  Lit(x) -> string_of_int x
	| Assign(x, y) -> x ^ " = " ^ (eval_expr y)
	| Binop(e1, op, e2) ->
		let v1 = eval_expr e1 and v2 = eval_expr e2 in
		(match op with
		  Add -> v1 ^ "+"^ v2
		| Sub -> v1 ^ "-"^ v2
		| _ -> "")
	| _ -> ""

let rec eval_stmts = function
	    Expr(x) -> eval_expr x
	  | _ -> ""

let rec eval_fun func1 = 
		let fname = func1.fname in
			let rec get_stmts = function
				  [] -> ""
				| next_stmt::remaining_stmts -> (eval_stmts next_stmt ^ "\n") ^ (get_stmts remaining_stmts)
		in fname ^ ": \n" ^ (get_stmts func1.body)

*)
let rec eval_prog prog =
		match prog with
		_ ->  "Successfully parsed\n"
	



let _ =
	let lexbuf = Lexing.from_channel stdin in
	let ast = Parser.program Scanner.token lexbuf in
	print_string (eval_prog ast);
	print_string (string_of_program 0 ast)
	(* print_string (Ast.string_of_program ast) *)

