open Ast


let rec eval_expr = function
	  Lit(x) -> string_of_int x
	| Assign(x, y) -> x ^ " = " ^ (eval_expr y)
	| Binop(e1, op, e2) ->
		let v1 = eval_expr e1 and v2 = eval_expr e2 in
		match op with
		  Add -> v1 ^ "+"^ v2
		| Sub -> v1 ^ "-"^ v2

let rec eval_stmts = function
	  Expr(x) -> eval_expr x

let rec eval_fun func1 = 
		let fname = func1.fname in
			let rec get_stmts = function
				  [] -> ""
				| next_stmt::remaining_stmts -> (eval_stmts next_stmt ^ "\n") ^ (get_stmts remaining_stmts)
		in fname ^ ": \n" ^ (get_stmts func1.body)

<<<<<<< HEAD
=======
let rec eval_prog prog =
	match prog with
	  [] -> ""
	| p1::p2 -> let next_stmt = p1 in
		match next_stmt with
		  Func(x) -> eval_fun x
		| Var(x) -> ""
	


>>>>>>> initial_flow

let _ =
	let lexbuf = Lexing.from_channel stdin in
	let ast = Parser.program Scanner.token lexbuf in
<<<<<<< HEAD
	print_string (eval_fun ast)
=======
	print_string (eval_prog ast)
>>>>>>> initial_flow
