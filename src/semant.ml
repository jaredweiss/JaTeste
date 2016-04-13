module A = Ast
module S = Sast
module StringMap = Map.Make(String)


type variable_decls = A.bind;;

(* Hashtable of valid structs. This is filled out when we iterate through the user defined structs *)
let struct_types:(string, A.struct_decl) Hashtbl.t = Hashtbl.create 10
let func_names:(string, A.func_decl) Hashtbl.t = Hashtbl.create 10

let built_in_print_string:(A.func_decl) = {A.typ = A.Primitive(A.Void) ; A.fname = "print"; A.formals = [(A.Primitive(A.String), "arg1")]; A.vdecls = []; A.body = []; A.tests = {A.exprs = []; A.using = {A.uvdecls = []; A.stmts = []}} }

let built_in_print_int:(A.func_decl) = {A.typ = A.Primitive(A.Void) ; A.fname = "print_int"; A.formals = [(A.Primitive(A.Int), "arg1")]; A.vdecls = []; A.body = []; A.tests = {A.exprs = []; A.using = {A.uvdecls = []; A.stmts = []}} }


(* Symbol table used for checking scope *)
type symbol_table = {
	parent : symbol_table option;
	variables : (string, A.typ) Hashtbl.t;
}

(* Environment*)
type environment = {
	scope : symbol_table;
	return_type : A.typ option;
}

(* For debugging *)
let rec string_of_typ t =
	match t with
	  A.Primitive(A.Int) -> "Int"
	| A.Primitive(A.Double) -> "Double"
	| A.Primitive(A.String) -> "String"
	| A.Primitive(A.Char) -> "Char"
	| A.Primitive(A.Void) -> "Void"
	| A.Struct_typ(s) -> "struct " ^ s
	| A.Pointer_typ(t) -> "pointer " ^ (string_of_typ t)
	| A.Array_typ(p,_) -> "Array type " ^ (string_of_typ (A.Primitive(p)))
	| _ -> "not sure"

(* Search symbol tables to see if the given var exists somewhere *)
let rec find_var (scope : symbol_table) var =
	try Hashtbl.find scope.variables var
	with Not_found ->
	match scope.parent with
	  Some(parent) -> find_var parent var
	| _ -> raise (Exceptions.UndeclaredVariable var)	

let type_of_identifier var env = 
	find_var env.scope var

let type_of_array arr _ =
	match arr with
	  A.Array_typ(p,_) -> A.Primitive(p)
	| _ -> raise (Exceptions.InvalidArrayVariable)

(* Function is done for creating sast after semantic checking. Should only be called on struct or array access *)
let rec string_identifier_of_expr expr = 
	match expr with
	  A.Id(s) -> s
	| A.Struct_access(e1, _) -> string_identifier_of_expr e1 
	| A.Array_access(e1, _) -> string_identifier_of_expr e1
	| A.Call(s,_) -> s
	| _ -> raise (Exceptions.BugCatch "string_identifier_of_expr")

(* Function is done for creating sast after semantic checking. Should only be called on struct fields *)
let string_of_struct_expr expr = 
	match expr with
	  A.Id(s) -> s
	| _ -> raise (Exceptions.BugCatch "string_of_struct_expr")
	
(* Helper function to check for dups in a list *)
let report_duplicate exceptf list =
    let rec helper = function
        n1 :: n2 :: _ when n1 = n2 -> raise (Failure (exceptf n1))
      | _ :: t -> helper t
      | [] -> ()
    in helper (List.sort compare list)

(* Helper function to check a typ is not void *)
let check_not_void exceptf = function
      (A.Primitive(A.Void), n) -> raise (Failure (exceptf n))
    | _ -> ()

(* Helper function to check two types match up *)
let check_assign lvaluet rvaluet err =
     if lvaluet = rvaluet then lvaluet else raise err

(* Search hash table to see if the struct is valid *)
let check_valid_struct s =
	try Hashtbl.find struct_types s
	with | Not_found -> raise (Exceptions.InvalidStruct s)

let check_valid_func_call s = 
	try Hashtbl.find func_names s
	with | Not_found -> raise (Exceptions.InvalidFunctionCall (s ^ " does not exist. Unfortunately you can't just expect functions to magically exist"))

let struct_contains_field s field env = 
		let struct_var = find_var env.scope s in 
		match struct_var with 
		  A.Struct_typ(struc_name) | A.Pointer_typ(A.Struct_typ(struc_name)) ->
		(let stru:(A.struct_decl) = check_valid_struct struc_name in 
		try let (my_typ,_) = (List.find (fun (_,nm) -> if nm = field then true else false) stru.A.attributes) in my_typ with | Not_found -> raise (Exceptions.InvalidStructField))
		| _ -> raise (Exceptions.InvalidStruct s)
	
let struct_contains_expr stru expr env = 
	match stru with
	  A.Id(s) -> (match expr with A.Id(s1) -> struct_contains_field s s1 env | _ -> raise (Exceptions.InvalidStructField)) 
	| _ -> raise (Exceptions.InvalidStructField)

		

(* Dont think we need this - but could be wrong so going to keep it around for now *)
(*
let rec type_of_expression expr env = 
	match expr with
	  A.Lit(_) -> A.Primitive(A.Int)
	| A.String_lit(_) -> A.Primitive(A.String)
	| A.Binop(e1,_,_) -> type_of_expression e1 env
	| A.Unop(_,e) -> type_of_expression e env
	| A.Assign(e1, _) -> type_of_expression e1 env
	| A.Noexpr -> A.Primitive(A.Void)
	| A.Id(s) -> type_of_identifier s env
	| A.Struct_create(s) -> (try let tmp_struct = check_valid_struct s in (A.Struct_typ(tmp_struct.A.sname)) with | Not_found -> raise (Exceptions.InvalidStruct s))
	| A.Struct_access(s,f) -> struct_contains_expr s f
	| A.Array_create(size,prim_type) -> A.Array_typ(prim_type, size)
	| A.Array_access(e,_) -> type_of_array (type_of_expression e env) env
	| A.Call(s,_) -> (try let call = check_valid_func_call s in call.A.typ with | Not_found -> raise (Exceptions.InvalidFunctionCall s))
*)
		
(* convert expr to sast expr *)
let rec expr_sast expr =
	match expr with
	  A.Lit a -> S.SLit a
	| A.String_lit s -> S.SString_lit s	
	| A.Binop (e1, op, e2) -> S.SBinop (expr_sast e1, op, expr_sast e2)
	| A.Unop (u, e) -> S.SUnop(u, expr_sast e)
	| A.Assign (s, e) -> S.SAssign (expr_sast s, expr_sast e)
	| A.Noexpr -> S.SNoexpr
	| A.Id s -> S.SId s
	| A.Struct_create s -> S.SStruct_create s
	| A.Struct_access (e1, e2) -> S.SStruct_access (string_identifier_of_expr e1, string_of_struct_expr e2)
	| A.Array_create (i, p) -> S.SArray_create (i, p)
	| A.Array_access (e, i) -> S.SArray_access (string_identifier_of_expr e, i)
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
	let tmp:(S.swith_using_decl) = {S.suvdecls = r.A.uvdecls; S.sstmts = (List.map stmt_sast r.A.stmts)} in
	 tmp

let with_test_sast r =
	let tmp:(S.swith_test_decl) = {S.sexprs = (List.map expr_sast r.A.exprs) ; S.susing = (with_using_sast r.A.using)} in
	tmp 

let func_decl_sast r = 
	let tmp:(S.sfunc_decl) = {S.styp = r.A.typ; S.sfname = r.A.fname; S.sformals = r.A.formals; S.svdecls = r.A.vdecls ; S.sbody = (List.map stmt_sast r.A.body); S.stests = (with_test_sast r.A.tests) } in
	tmp	

let struct_sast r = 
	let tmp:(S.sstruct_decl) = {S.ssname = r.A.sname ; S.sattributes = r.A.attributes } in
	tmp

(* Entry to transform AST to SAST *)
let program_sast (globals, functions, structs) = 
	let tmp:(S.sprogram) = (globals, (List.map func_decl_sast functions), (List.map struct_sast structs)) in
	tmp

(* Functions to check the semantics of JaTeste Program *)

(* Struct semantic checker *)
let check_structs structs = 
	(report_duplicate(fun n -> "duplicate struct " ^ n) (List.map (fun n -> n.A.sname) structs)); 

	ignore (List.map (fun n -> (report_duplicate(fun n -> "duplicate struct field " ^ n) (List.map (fun n -> snd n) n.A.attributes))) structs);

	ignore (List.map (fun n -> (List.iter (check_not_void (fun n -> "Illegal void field" ^ n)) n.A.attributes)) structs);
	ignore(List.iter (fun n -> Hashtbl.add struct_types n.A.sname n) structs);
()

(* Globa variables semantic checker *)
let check_globals globals env = 
	ignore(env);
	ignore (report_duplicate (fun n -> "duplicate global " ^ n) (List.map snd globals)); 
	List.iter (check_not_void (fun n -> "illegal void global " ^ n)) globals;
	(* Check that any global structs are actually valid structs that have been defined *)
	List.iter (fun (t,_) -> match t with 
		  A.Struct_typ(nm) -> ignore(check_valid_struct nm); ()
		| _ -> ()
	) globals;
	(* Add global variables to top level symbol table. Side effects *)
	List.iter (fun (t,s) -> (Hashtbl.add env.scope.variables s t)) globals;

()

let rec check_expr expr env =
	match expr with
	  A.Lit(_) -> A.Primitive(A.Int)
	| A.String_lit(_) -> A.Primitive(A.String)
	| A.Binop(e1,op,e2) -> let e1' = (check_expr e1 env) in let e2' = (check_expr e2 env) in
		(match op with
		  A.Add | A.Sub | A.Mult | A.Div | A.Exp | A.Mod  when e1' = e2' && (e1' = A.Primitive(A.Int) || e1' = A.Primitive(A.Double))-> e1'
		| A.Equal | A.Neq when e1' = e2' -> ignore("got equal");A.Primitive(A.Int)
		| A.Less | A.Leq | A.Greater | A.Geq when e1' = e2' && (e1' = A.Primitive(A.Int) || e1' = A.Primitive(A.Double))-> e1'
		| A.And | A.Or when e1' = e2' && (e1' = A.Primitive(A.Int) || e1' = A.Primitive(A.Double))-> e1'
		| _ -> raise (Exceptions.InvalidExpr "Illegal binary op") 
) 
	| A.Unop(uop,e) -> ignore(uop);ignore(e);A.Primitive(A.Int)
	| A.Assign(var,e) -> (let right_side_type = check_expr e env in 
			let left_side_type  = check_expr var env in
				check_assign left_side_type right_side_type Exceptions.IllegalAssignment)
	| A.Noexpr -> A.Primitive(A.Void)
	| A.Id(s) -> type_of_identifier s env 
	| A.Struct_create(s) -> (try let tmp_struct = check_valid_struct s in (A.Pointer_typ(A.Struct_typ(tmp_struct.A.sname))) with | Not_found -> raise (Exceptions.InvalidStruct s))
	| A.Struct_access(e1,e2) -> struct_contains_expr e1 e2 env
	| A.Array_create(size,prim_type) -> A.Array_typ(prim_type, size)
	| A.Array_access(e, _) -> type_of_array (check_expr e env) env
	| A.Call(s,el) -> let func_info = (check_valid_func_call s) in
	let func_info_formals = func_info.A.formals in
		if List.length func_info_formals != List.length el then
		raise (Exceptions.InvalidArgumentsToFunction (s ^ " is supplied with wrong args"))
	else
		List.iter2 (fun (ft,_) e -> let e = check_expr e env in ignore(check_assign ft e (Exceptions.InvalidArgumentsToFunction ("Args to functions " ^ s ^ " don't match up with it's definition")))) func_info_formals el;
	func_info.A.typ

let check_is_bool expr env = 
	ignore(check_expr expr env);
	match expr with
	 A.Binop(_,A.Equal,_) | A.Binop(_,A.Neq,_) | A.Binop(_,A.Less,_) | A.Binop(_,A.Leq,_) | A.Binop(_,A.Greater,_) | A.Binop(_,A.Geq,_) -> ()

	| _ ->  raise (Exceptions.InvalidBooleanExpression)

let check_return_expr expr env = 
	match env.return_type with
	  Some(rt) -> if rt = check_expr expr env then () else raise (Exceptions.InvalidReturnType "return type doesnt match with function definition")
	| _ -> raise (Exceptions.BugCatch "Should not be checking return type outside a function")


let rec check_stmt stmt env = 
	match stmt with
	  A.Block(l) -> (let rec check_block b env2=
			match b with
			  [A.Return _ as s] -> check_stmt s env2
			| A.Return _ :: _ -> raise (Exceptions.InvalidReturnType "Can't have any code after return statement")
			| A.Block l :: ss -> check_block (l @ ss) env2
			| l :: ss -> check_stmt l env2; check_block ss env
			| [] -> ()
			in
			check_block l env)
	| A.Expr(e) -> ignore(check_expr e env); ()
	| A.If(e1,s1,s2) ->ignore(check_is_bool e1 env);check_stmt s1 env;check_stmt s2 env; ()
	| A.While(e,s) -> ignore(check_is_bool e env);check_stmt s env;()
	| A.For(e1,e2,e3,s) -> ignore(e1);ignore(e2);ignore(e3);ignore(s);()
	| A.Return(e) -> ignore(check_return_expr e env);()
	
(* Function names (aka can't have two functions with same name) semantic checker *)
let check_function_names functions = 
	ignore(report_duplicate (fun n -> "duplicate function names " ^ n) (List.map (fun n -> n.A.fname) functions));	
	(* Add the built in function(s) here. There shouldnt be too many of these *)
	ignore(Hashtbl.add func_names built_in_print_string.A.fname built_in_print_string);
	ignore(Hashtbl.add func_names built_in_print_int.A.fname built_in_print_int);
	(* Go through the functions and add their names to a global hashtable that stores the whole function as its value -> (key, value) = (func_decl.fname, func_decl) *)
	ignore(List.iter (fun n -> Hashtbl.add func_names n.A.fname n) functions); ()

(* Checks programmer hasn't defined function print as it's reserved *)
let check_function_not_print names = 
	ignore(if List.mem "print" (List.map (fun n -> n.A.fname) names ) then raise (Failure ("function print may not be defined")) else ()); ()

(* Check the body of the function here *)
let check_function_body funct env =
	report_duplicate (fun n -> "duplicate formal arg " ^ n) (List.map snd funct.A.formals);
	report_duplicate (fun n -> "duplicate local " ^ n) (List.map snd funct.A.vdecls);
	(* Check no duplicates *)
	let formals_and_locals = List.append funct.A.formals funct.A.vdecls in
	report_duplicate (fun n -> "same name for formal and local var " ^ n) (List.map snd formals_and_locals);
	(* Check structs are valid *)
	List.iter (fun (t,_) -> match t with 
			A.Struct_typ(nm) -> ignore(check_valid_struct nm); ()
		| _ -> ()
	) formals_and_locals;
	(* Create new enviornment -> symbol table parent is set to previous scope's symbol table *)
	let new_env = {scope = {parent = Some(env.scope) ; variables = Hashtbl.create 10}; return_type = Some(funct.A.typ)} in
	(* Add formals + locals to this scope symbol table *)
	List.iter (fun (t,s) -> (Hashtbl.add new_env.scope.variables s t)) formals_and_locals;
	ignore(check_stmt (A.Block funct.A.body) new_env);	
 ()

(* Entry point to check functions *)
let check_functions functions env = 
	(check_function_names functions); 
	(check_function_not_print functions); 
	(List.iter (fun n -> check_function_body n env) functions); ()

(* Entry point for semantic checking AST. Output should be a SAST *)
let check (globals, functions, structs) =  
	let prog_env:environment = {scope = {parent = None ; variables = Hashtbl.create 10 }; return_type = None} in
	let _ = check_structs structs in
	let _ = check_globals globals prog_env in
	let _ = check_functions functions prog_env in
	let sprogram:(S.sprogram) = program_sast (globals, functions, structs) in
	sprogram
