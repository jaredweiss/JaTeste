module A = Ast
module S = Sast
module StringMap = Map.Make(String)


type variable_decls = A.bind;;

(* Hashtable of valid structs. This is filled out when we iterate through the user defined structs *)
let struct_types:(string, string) Hashtbl.t = Hashtbl.create 10

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

let string_of_typ t =
	match t with
	  A.Primitive(A.Int) -> "Int"
	| A.Primitive(A.String) -> "String"
	| A.Primitive(A.Char) -> "Char"
	| _ -> "not sure"

(* Search symbol tables to see if the given var exists somewhere *)
let rec find_var (scope : symbol_table) var =
	try Hashtbl.find scope.variables var
	with Not_found ->
	match scope.parent with
	  Some(parent) -> find_var parent var
	| _ -> raise (Exceptions.UndeclaredVariable var)	

let type_of_identifier var env = find_var env.scope var

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
     if lvaluet == rvaluet then lvaluet else raise err

(* Search hash table to see if the struct is valid *)
let check_valid_struct s =
	try Hashtbl.find struct_types s
	with | Not_found -> raise (Exceptions.InvalidStruct s)

			
(* convert expr to sast expr *)
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
	ignore(List.iter (fun n -> Hashtbl.add struct_types n.A.sname n.A.sname) structs);
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
	| A.String_Lit(_) -> A.Primitive(A.String)
	| A.Binop(e1,op,e2) -> let e1' = (check_expr e1 env) in let e2' = (check_expr e2 env) in
		(match op with
		  A.Add | A.Sub | A.Mult | A.Div | A.Exp | A.Mod  when e1' = e2' && (e1' = A.Primitive(A.Int) || e1' = A.Primitive(A.Double))-> e1'
		| A.Equal | A.Neq when e1' = e2' -> ignore("got equal");A.Primitive(A.Int)
		| A.Less | A.Leq | A.Greater | A.Geq when e1' = e2' && (e1' = A.Primitive(A.Int) || e1' = A.Primitive(A.Double))-> e1'
		| A.And | A.Or when e1' = e2' && (e1' = A.Primitive(A.Int) || e1' = A.Primitive(A.Double))-> e1'
		| _ -> raise (Exceptions.InvalidExpr "Illegal binary op") 
) 
	| A.Unop(uop,e) -> ignore(uop);ignore(e);A.Primitive(A.Int)
	| A.Assign(s,e) -> ignore(s);ignore(e);A.Primitive(A.Int)
	| A.Noexpr -> A.Primitive(A.Void)
	| A.Id(s) -> type_of_identifier s env 
	| A.Struct_create(s) -> ignore(s);A.Primitive(A.Int)
	| A.Struct_Access(e1,e2) -> ignore(e1);ignore(e2);A.Primitive(A.Int)
	| A.Array_create(i,p) -> ignore(i);ignore(p);A.Primitive(A.Int)
	| A.Array_access(e, i) -> ignore(e); ignore(i); A.Primitive(A.Int)
	| A.Call(s,el) -> ignore(s);ignore(el);A.Primitive(A.Int)



let check_is_bool expr env = 
	ignore(check_expr expr env);
	match expr with
	 A.Binop(_,A.Equal,_) | A.Binop(_,A.Neq,_) | A.Binop(_,A.Less,_) | A.Binop(_,A.Leq,_) | A.Binop(_,A.Greater,_) | A.Binop(_,A.Geq,_) -> ()

	| _ ->  raise (Exceptions.InvalidBooleanExpression)

let check_return_expr expr env = 
	match env.return_type with
	  Some(rt) -> if rt = check_expr expr env then () else raise Exceptions.InvalidReturnType
	| _ -> raise (Exceptions.BugCatch "Should not be checking return type outside a function")


let rec check_stmt stmt env = 
	match stmt with
	  A.Block(l) -> ignore(List.map (fun n -> (check_stmt n env)) l);()
	| A.Expr(e) -> ignore(check_expr e env); ()
	| A.If(e1,s1,s2) ->ignore(check_is_bool e1 env);check_stmt s1 env;check_stmt s2 env; ()
	| A.While(e,s) -> ignore(check_is_bool e env);check_stmt s env;()
	| A.For(e1,e2,e3,s) -> ignore(e1);ignore(e2);ignore(e3);ignore(s);()
	| A.Return(e) -> ignore(check_return_expr e env);()
	
(* Function names (aka can't have two functions with same name) semantic checker *)
let check_function_names names = 
	ignore(report_duplicate (fun n -> "duplicate function names " ^ n) (List.map (fun n -> n.A.fname) names)); ()

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
	ignore(List.map (fun n -> check_stmt n new_env) funct.A.body);	
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
