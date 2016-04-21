(* Semantic checker code. Takes Ast as input and returns a Sast *)

module A = Ast
module S = Sast
module StringMap = Map.Make(String)

type variable_decls = A.bind;;

(* Hashtable of valid structs. This is filled out when we iterate through the user defined structs *)
let struct_types:(string, A.struct_decl) Hashtbl.t = Hashtbl.create 10
let func_names:(string, A.func_decl) Hashtbl.t = Hashtbl.create 10

let built_in_print_string:(A.func_decl) = {A.typ = A.Primitive(A.Void) ; A.fname = "print"; A.formals = [A.Any, "arg1"]; A.vdecls = []; A.body = []; A.tests = None }

let built_in_print_int:(A.func_decl) = {A.typ = A.Primitive(A.Void) ; A.fname = "print_int"; A.formals = [(A.Primitive(A.Int), "arg1")]; A.vdecls = []; A.body = []; A.tests = None }


(* Symbol table used for checking scope *)
type symbol_table = {
	parent : symbol_table option;
	variables : (string, A.typ) Hashtbl.t;
}

(* Environment*)
type environment = {
	scope : symbol_table;
	return_type : A.typ option;
	func_name : string option;
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

(* Helper function to reeturn an identifers type *)
let type_of_identifier var env = 
	find_var env.scope var

(* Returns the type of the arrays elements. E.g. int[10] arr... type_of_array arr would return A.Int *)
let type_of_array arr _ =
	match arr with
	  A.Array_typ(p,_) -> A.Primitive(p)
	| _ -> raise (Exceptions.InvalidArrayVariable)

(* Function is done for creating sast after semantic checking. Should only be called on struct or array access *)
let rec string_identifier_of_expr expr = 
	match expr with
	  A.Id(s) -> s
	| A.Struct_access(e1, _) -> string_identifier_of_expr e1 
	| A.Pt_access(e1, _) -> string_identifier_of_expr e1 
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

let check_ends_in_jt str = 
	let len = String.length str in
	if len < 4 then raise (Exceptions.InvalidHeaderFile str);
	let subs = String.sub str (len - 3) 3 in
	(match subs with
	  ".jt" -> ()
	| _ -> raise (Exceptions.InvalidHeaderFile str)
	)

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

(* Checks the hash table to see if the function exists *)
let check_valid_func_call s = 
	try Hashtbl.find func_names s
	with | Not_found -> raise (Exceptions.InvalidFunctionCall (s ^ " does not exist. Unfortunately you can't just expect functions to magically exist"))


(* Helper function that finds index of first matching element in list *)
let rec index_of_list x l = 
	match l with
	  [] -> raise (Exceptions.InvalidStructField)
	| hd::tl -> let (_,y) = hd in if x = y then 0 else 1 + index_of_list x tl

let index_helper s field env = 
		let struct_var = find_var env.scope s in 
		match struct_var with 
		  A.Struct_typ(struc_name) ->
		(let stru:(A.struct_decl) = check_valid_struct struc_name in 
		try let index = index_of_list field stru.A.attributes in index with | Not_found -> raise (Exceptions.InvalidStructField))
		| A.Pointer_typ(A.Struct_typ(struc_name)) ->
		(let stru:(A.struct_decl) = check_valid_struct struc_name in 
		try let index = index_of_list field stru.A.attributes in index with | Not_found -> raise (Exceptions.InvalidStructField))
		| _ -> raise (Exceptions.BugCatch "struct_contains_field")


(* Function that returns index of the field in a struct. E.g. given: stuct person {int age; int height;};.... index_of_struct_field *str "height" env will return 1 *)
let index_of_struct_field stru expr env = 	
		match stru with
	  		A.Id(s) -> (match expr with A.Id(s1) -> index_helper s s1 env | _ -> raise (Exceptions.InvalidStructField)) 
			| _ -> raise (Exceptions.InvalidStructField)


(* Checks the relevant struct actually has a given field *)
let struct_contains_field s field env = 
		let struct_var = find_var env.scope s in 
		match struct_var with 
		  A.Struct_typ(struc_name) ->
		(let stru:(A.struct_decl) = check_valid_struct struc_name in 
		try let (my_typ,_) = (List.find (fun (_,nm) -> if nm = field then true else false) stru.A.attributes) in my_typ with | Not_found -> raise (Exceptions.InvalidStructField))
		| A.Pointer_typ(A.Struct_typ(struc_name)) ->
		(let stru:(A.struct_decl) = check_valid_struct struc_name in 
		try let (my_typ,_) = (List.find (fun (_,nm) -> if nm = field then true else false) stru.A.attributes) in my_typ with | Not_found -> raise (Exceptions.InvalidStructField))

		| _ -> raise (Exceptions.BugCatch "struct_contains_field")

(*
(* Checks the relevant struct pointer actually a given field *)
let struct_pt_contains_field s field env = 
		let struct_var = find_var env.scope s in 
		match struct_var with 
		  A.Pointer_typ(A.Struct_typ(struc_name)) ->
		(let stru:(A.struct_decl) = check_valid_struct struc_name in 
		try let (my_typ,_) = (List.find (fun (_,nm) -> if nm = field then true else false) stru.A.attributes) in my_typ with | Not_found -> raise (Exceptions.InvalidStructField))
		| _ -> raise (Exceptions.InvalidStruct s) *)

let struct_contains_expr stru expr env = 
	match stru with
	  A.Id(s) -> (match expr with A.Id(s1) -> struct_contains_field s s1 env | _ -> raise (Exceptions.InvalidStructField)) 
	| _ -> raise (Exceptions.InvalidStructField)
	
(**********Start of code to convert SAST to AST**********)

(* convert expr to sast expr *)
let rec expr_sast expr env =
	match expr with
	  A.Lit a -> S.SLit a
	| A.String_lit s -> S.SString_lit s	
	| A.Binop (e1, op, e2) -> S.SBinop (expr_sast e1 env, op, expr_sast e2 env)
	| A.Unop (u, e) -> S.SUnop(u, expr_sast e env)
	| A.Assign (s, e) -> S.SAssign (expr_sast s env, expr_sast e env)
	| A.Noexpr -> S.SNoexpr
	| A.Id s -> S.SId (s)
	| A.Struct_create s -> S.SStruct_create s
	| A.Free e -> let st = (string_identifier_of_expr e) in S.SFree(st)
	| A.Struct_access (e1, e2) -> S.SStruct_access (string_identifier_of_expr e1, string_of_struct_expr e2)
	| A.Pt_access (e1, e2) -> let index = index_of_struct_field e1 e2 env in S.SPt_access (string_identifier_of_expr e1, string_identifier_of_expr e2, index)
	| A.Array_create (i, p) -> S.SArray_create (i, p)
	| A.Array_access (e, i) -> S.SArray_access (string_identifier_of_expr e, i)
	| A.Dereference(e) -> S.SDereference(string_identifier_of_expr e) 
	| A.Call (s, e) -> S.SCall (s, (List.map (fun n -> expr_sast n env) e))
	| A.BoolLit(b) -> S.SBoolLit((match b with true -> 1 | false -> 0))


let struct_sast r = 
	let tmp:(S.sstruct_decl) = {S.ssname = r.A.sname ; S.sattributes = r.A.attributes } in
	tmp

(* Struct semantic checker *)
let check_structs structs = 
	(report_duplicate(fun n -> "duplicate struct " ^ n) (List.map (fun n -> n.A.sname) structs)); 

	ignore (List.map (fun n -> (report_duplicate(fun n -> "duplicate struct field " ^ n) (List.map (fun n -> snd n) n.A.attributes))) structs);

	ignore (List.map (fun n -> (List.iter (check_not_void (fun n -> "Illegal void field" ^ n)) n.A.attributes)) structs);
	ignore(List.iter (fun n -> Hashtbl.add struct_types n.A.sname n) structs);
	structs

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
	globals

(* Main entry pointer for checking the semantics of an expression *)
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
	| A.Unop(uop,e) -> let expr_type = check_expr e env in
			(match uop with
				  A.Not -> expr_type 
				| A.Neg -> expr_type
				| A.Addr -> A.Pointer_typ(expr_type)
			)
	| A.Assign(var,e) -> (let right_side_type = check_expr e env in 
			let left_side_type  = check_expr var env in
				check_assign left_side_type right_side_type Exceptions.IllegalAssignment)
	| A.Noexpr -> A.Primitive(A.Void)
	| A.Id(s) -> type_of_identifier s env 
	| A.Struct_create(s) -> (try let tmp_struct = check_valid_struct s in (A.Pointer_typ(A.Struct_typ(tmp_struct.A.sname))) with | Not_found -> raise (Exceptions.InvalidStruct s))
	| A.Struct_access(e1,e2) -> struct_contains_expr e1 e2 env
	| A.Pt_access(e1,e2) -> let e1' = check_expr e1 env in
			(match e1' with
			  A.Pointer_typ(A.Struct_typ(_)) -> struct_contains_expr e1 e2 env
			| A.Pointer_typ(A.Primitive(p)) -> (let e2' = check_expr e2 env in (check_assign (A.Primitive(p)) e2') (Exceptions.InvalidPointerDereference))
			| _ -> raise (Exceptions.BugCatch "hey")
			)
	| A.Dereference(i) ->  let pointer_type = (check_expr i env)  in (
			 match pointer_type with 
			   A.Pointer_typ(pt) -> pt
			 | _ -> raise (Exceptions.BugCatch "Deference") 
						)
				
	| A.Array_create(size,prim_type) -> A.Array_typ(prim_type, size)
	| A.Array_access(e, _) -> type_of_array (check_expr e env) env
	| A.Free(p) -> let pt = string_identifier_of_expr p in let pt_typ = find_var env.scope pt in (match pt_typ with A.Pointer_typ(_) -> pt_typ | _ -> raise (Exceptions.InvalidFree "not a pointer"))
	| A.Call("print", el) ->  if List.length el != 1 then raise Exceptions.InvalidPrintCall 
				else
				List.iter (fun n -> ignore(check_expr n env); ()) el; A.Primitive(A.Int)
	| A.Call(s,el) -> let func_info = (check_valid_func_call s) in
	let func_info_formals = func_info.A.formals in
		if List.length func_info_formals != List.length el then
		raise (Exceptions.InvalidArgumentsToFunction (s ^ " is supplied with wrong args"))
	else
		List.iter2 (fun (ft,_) e -> let e = check_expr e env in ignore(check_assign ft e (Exceptions.InvalidArgumentsToFunction ("Args to functions " ^ s ^ " don't match up with it's definition")))) func_info_formals el;
	func_info.A.typ
	| A.BoolLit(_) -> A.Primitive(A.Bool)

(* Checks if expr is a boolean expr. Used for checking the predicate of things like if, while statements *)
let check_is_bool expr env = 
	ignore(check_expr expr env);
	match expr with
	 A.Binop(_,A.Equal,_) | A.Binop(_,A.Neq,_) | A.Binop(_,A.Less,_) | A.Binop(_,A.Leq,_) | A.Binop(_,A.Greater,_) | A.Binop(_,A.Geq,_) -> ()

	| _ ->  raise (Exceptions.InvalidBooleanExpression)

(* Checks that return value is the same type as the return type in the function definition*)
let check_return_expr expr env = 
	match env.return_type with
	  Some(rt) -> if rt = check_expr expr env then () else raise (Exceptions.InvalidReturnType "return type doesnt match with function definition")
	| _ -> raise (Exceptions.BugCatch "Should not be checking return type outside a function")

(* Main entry point for checking semantics of statements *)
let rec check_stmt stmt env = 
	match stmt with
(*
	  A.Block(l) -> (let rec check_block b env2=
			match b with
			  [A.Return _ as s] -> check_stmt s env2
			| A.Return _ :: _ -> raise (Exceptions.InvalidReturnType "Can't have any code after return statement")
			| A.Block l :: ss -> check_block (l @ ss) env2
			| l :: ss -> S.SBlock((check_stmt l env2)):: (check_block ss env)
			| l :: [] -> check_stmt l env
			in
			check_block l env) *)
	| A.Block(b) -> S.SBlock (List.map (fun n -> check_stmt n env) b)
	| A.Expr(e) -> ignore(check_expr e env); S.SExpr(expr_sast e env)
	| A.If(e1,s1,s2) ->ignore(check_is_bool e1 env); S.SIf (expr_sast e1 env, check_stmt s1 env, check_stmt s2 env)
	| A.While(e,s) -> ignore(check_is_bool e env); S.SWhile (expr_sast e env, check_stmt s env)
	| A.For(e1,e2,e3,s) -> ignore(e1);ignore(e2);ignore(e3);ignore(s); S.SFor(expr_sast e1 env, expr_sast e2 env, expr_sast e3 env, check_stmt s env) 
	| A.Return(e) -> ignore(check_return_expr e env);S.SReturn (expr_sast e env)

let with_using_sast r env = 
	let tmp:(S.swith_using_decl) = {S.suvdecls = r.A.uvdecls; S.sstmts = (List.map (fun n -> check_stmt n env) r.A.stmts)} in
	 tmp

let with_test_sast r env =
	let tmp:(S.swith_test_decl) = {S.sexprs = (List.map (fun n -> expr_sast n env) r.A.exprs) ; S.susing = (with_using_sast r.A.using env)} in
	tmp 

let convert_test_to_func using_decl test_decl env = 
	let concat_stmts_exprs = List.append using_decl.A.stmts ((List.map (fun n -> A.Expr(n)) test_decl.A.exprs))  in
	(match env.func_name with
	  Some(fn) ->let new_func_name = fn ^ "test" in  let new_func:(A.func_decl) = {A.typ = A.Primitive(A.Void); A.fname = (new_func_name); A.formals = []; A.vdecls =  using_decl.A.uvdecls; A.body = concat_stmts_exprs ; A.tests = None} in new_func

	|  None -> raise (Exceptions.BugCatch "convert_test_to_func")
)

(* Function names (aka can't have two functions with same name) semantic checker *)
let check_function_names functions = 
	ignore(report_duplicate (fun n -> "duplicate function names " ^ n) (List.map (fun n -> n.A.fname) functions));	
	(* Add the built in function(s) here. There shouldnt be too many of these *)
	ignore(Hashtbl.add func_names built_in_print_string.A.fname built_in_print_string);
	ignore(Hashtbl.add func_names built_in_print_int.A.fname built_in_print_int);
	(* Go through the functions and add their names to a global hashtable that stores the whole function as its value -> (key, value) = (func_decl.fname, func_decl) *)
	ignore(List.iter (fun n -> Hashtbl.add func_names n.A.fname n) functions); ()

let check_prog_contains_main funcs =
	let contains_main = List.exists (fun n -> if n.A.fname = "main" then true else false) funcs in
	(match contains_main with
	  true -> ()
	| false -> raise Exceptions.MissingMainFunction
	)

(* Checks programmer hasn't defined function print as it's reserved *)
let check_function_not_print names = 
	ignore(if List.mem "print" (List.map (fun n -> n.A.fname) names ) then raise (Failure ("function print may not be defined")) else ()); ()

(* Check the body of the function here *)
let rec check_function_body funct env =
	let curr_func_name = funct.A.fname in
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
	let new_env = {scope = {parent = Some(env.scope) ; variables = Hashtbl.create 10}; return_type = Some(funct.A.typ) ; func_name = Some(curr_func_name)} in
	(* Add formals + locals to this scope symbol table *)
	List.iter (fun (t,s) -> (Hashtbl.add new_env.scope.variables s t)) formals_and_locals;
	let body_with_env = List.map (fun n -> check_stmt n new_env) funct.A.body in
	(* Compile code for test case iff a function has defined a with test clause *)
	let sast_func_with_test = 
		(match funct.A.tests with
		Some(t) ->  let func_with_test = convert_test_to_func t.A.using t new_env in let new_env = {scope = {parent = None; variables = Hashtbl.create 10}; return_type = Some(A.Primitive(A.Void)) ; func_name = Some(curr_func_name ^ "test") } in
	Some(check_function_body func_with_test new_env) 
		| None -> None
		)
	in	
		
	let tmp:(S.sfunc_decl) = {S.styp = funct.A.typ; S.sfname = funct.A.fname; S.sformals = funct.A.formals; S.svdecls = funct.A.vdecls ; S.sbody = body_with_env; S.stests = (sast_func_with_test)} in
	tmp

(* Entry point to check functions *)
let check_functions functions env includes globals_add structs_add = 
	(check_function_names functions); 
	(check_function_not_print functions); 
	(check_prog_contains_main functions); 
	let sast_funcs = (List.map (fun n -> check_function_body n env) functions) in
	(*let sprogram:(S.sprogram) = program_sast (globals_add, functions, structs_add) in *)
	let sast = (includes, globals_add, sast_funcs, (List.map struct_sast structs_add )) in
	sast
	(* Need to check function test + using code here *)

let check_includes headers = 
	report_duplicate (fun n -> "duplicate header file " ^ n) headers;
	List.iter check_ends_in_jt headers;
	()
	

(* Entry point for semantic checking AST. Output should be a SAST *)
let check (includes, globals, functions, structs) =  
	let prog_env:environment = {scope = {parent = None ; variables = Hashtbl.create 10 }; return_type = None; func_name = None} in
	let _ = check_includes includes in
	let structs_added = check_structs structs in
	let globals_added = check_globals globals prog_env in
	let sast = check_functions functions prog_env includes globals_added structs_added in
	sast
