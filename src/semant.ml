module A = Ast
module S = Sast
module StringMap = Map.Make(String)

type variable_decls = A.bind list;;

type symbol_table = {
	parent : symbol_table option;
	variables : variable_decls list;
}

type env = {
	scope : symbol_table;
}

type tmpr = {
	age : int;
}


let rec expr_sast expr =
	match expr with
	  A.Lit a -> S.SLit a
	| A.String_Lit s -> S.SString_Lit s	
	| A.Binop (e1, op, e2) -> S.SBinop (expr_sast e1, op, expr_sast e2)
	| A.Unop (u, e) -> S.SUnop(u, expr_sast e)
	| A.Assign (s, e) -> S.SAssign (s, expr_sast e)
	| A.Noexpr -> S.SNoexpr
	| A.Id s -> S.SId s
	| A.Struct_create s -> S.SStruct_create s
	| A.Struct_Access (e1, e2) -> S.SStruct_Access (expr_sast e1, expr_sast e2)
	| A.Array_create (i, p) -> S.SArray_create (i, p)
	| A.Array_access (e, i) -> S.SArray_access (expr_sast e, i)
	| A.Call (s, e) -> S.SCall (s, (List.map expr_sast e))

let rec stmt_sast stmt =
	match stmt with
	  A.Block l -> S.SBlock (List.map stmt_sast l)
	| A.Expr e -> S.SExpr (expr_sast e)
	| A.If (e, s1, s2) -> S.SIf (expr_sast e, stmt_sast s1, stmt_sast s2)
	| A.While (e, s) -> S.SWhile (expr_sast e, stmt_sast s)
	| A.For (e1, e2, e3, s) -> S.SFor(expr_sast e1, expr_sast e2, expr_sast e3, stmt_sast s)
	| A.Return e -> S.SReturn (expr_sast e)


let with_using_sast r = 
	let tmp = {S.suvdecls = r.A.uvdecls; S.sstmts = (List.map stmt_sast r.A.stmts)} in
	 tmp

let with_test_sast r =
	let tmp = {S.sexprs = (List.map expr_sast r.A.exprs) ; S.susing = (with_using_sast r.A.using)} in
	tmp 

let func_decl_sast r = 
	let tmp = {S.styp = r.A.typ; S.sfname = r.A.fname; S.sformals = r.A.formals; S.svdecls = r.A.vdecls ; S.sbody = (List.map stmt_sast r.A.body); S.stests = (with_test_sast r.A.tests) } in
	tmp	

let struct_sast r = 
	let tmp = {S.ssname = r.A.sname ; S.sattributes = r.A.attributes } in
	tmp

let program_sast (globals, functions, structs) = 
	let tmp = (globals, (List.map func_decl_sast functions), (List.map struct_sast structs)) in
	tmp

let check_structs structs = ignore(structs); ()

let check_globals globals = ignore(globals);()

let check_functions functions = ignore(functions);()


let check (globals, functions, structs) =  
	let _ = check_structs structs in
	let _ = check_globals globals in
	let _ = check_functions functions in
	let sprogram = program_sast (globals, functions, structs) in
	sprogram
