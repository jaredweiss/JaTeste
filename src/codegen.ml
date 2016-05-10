(* Code generation code. Converts a Sast into LLVM code*)

module L = Llvm
module A = Ast
module S = Sast
module C = Char
module StringMap = Map.Make(String)

let context = L.global_context () 
(* module is what is returned from this file aka the LLVM code *)
let main_module = L.create_module context "Jateste" 
let test_module = L.create_module context "Jateste-test" 

(* Defined so we don't have to type out L.i32_type ... every time *)
let i32_t = L.i32_type context
let i64_t = L.i64_type context
let i8_t = L.i8_type context
let i1_t = L.i1_type context
let d_t = L.double_type context
let void_t = L.void_type context
let str_t = L.pointer_type i8_t 

(* Hash table of the user defined structs *)
let struct_types:(string, L.lltype) Hashtbl.t = Hashtbl.create 10
(* Hash table of global variables *)
let global_variables:(string, L.llvalue) Hashtbl.t = Hashtbl.create 50

(* Helper function that returns L.lltype for a struct. This should never fail as semantic checker should catch invalid structs *)
let find_struct_name name = 
	try Hashtbl.find struct_types name
	with | Not_found -> raise(Exceptions.InvalidStruct name)

let rec index_of_list x l = 
         match l with
           	  [] -> raise (Exceptions.InvalidStructField)
 		| hd::tl -> let (_,y) = hd in if x = y then 0 else 1 + index_of_list x tl


let cut_string s l = let len = String.length s in 
		if l >= len then raise (Exceptions.BugCatch "cut_string")
			    else let string_len = len - l in String.sub s 0 string_len

(* Code to declare struct *)
let declare_struct s =
	let struct_t = L.named_struct_type context s.S.ssname in
	Hashtbl.add struct_types s.S.ssname struct_t


let prim_ltype_of_typ = function
	  A.Int -> i32_t
	| A.Double -> d_t
	| A.Char -> i8_t
	| A.Void -> void_t
	| A.String -> str_t
	| A.Bool -> i1_t


let rec ltype_of_typ = function
	| A.Primitive(s) -> prim_ltype_of_typ s
	| A.Struct_typ(s) ->  find_struct_name s
	| A.Pointer_typ(s) -> L.pointer_type (ltype_of_typ s)
	| A.Array_typ(t,n) -> L.array_type (prim_ltype_of_typ t) n
    	| _ -> void_t 

let type_of_llvalue v = L.type_of v

let string_of_expr e =
	match e with
	  S.SId(s) -> s
	| _  -> raise (Exceptions.BugCatch "string_of_expr")

(* Function that builds LLVM struct *)
let define_struct_body s =
	let struct_t = try Hashtbl.find struct_types s.S.ssname with | Not_found -> raise (Exceptions.BugCatch "defin_struct") in
	let attribute_types = List.map (fun (t, _) -> t) s.S.sattributes in
	let attributes = List.map ltype_of_typ attribute_types in		
	let attributes_array = Array.of_list attributes in 
	L.struct_set_body struct_t attributes_array false

(* Helper function to create an array of size i fille with l values *)
let array_of_zeros i l = 
	Array.make i l

let default_value_for_prim_type t = 
	match t with 
		  A.Int -> L.const_int (prim_ltype_of_typ t) 0
		| A.Double ->L.const_float (prim_ltype_of_typ t) 0.0
		| A.String ->L.const_string context "" 
		| A.Char ->L.const_int (prim_ltype_of_typ t) 0
		| A.Void ->L.const_int (prim_ltype_of_typ t) 0
		| A.Bool ->L.const_int (prim_ltype_of_typ t) 0

(* Here we define and initailize global vars *)
let define_global_with_value (t, n) = 
		match t with 
		  A.Primitive(p) -> 
			(match p with
			  A.Int -> let init = L.const_int (ltype_of_typ t) 0 in (L.define_global n init main_module)
			| A.Double -> let init = L.const_float (ltype_of_typ t) 0.0 in (L.define_global n init main_module)
			| A.String -> let init = L.const_pointer_null (ltype_of_typ t) in (L.define_global n init main_module)		
			| A.Void -> let init = L.const_int (ltype_of_typ t) 0 in (L.define_global n init main_module)
			| A.Char -> let init = L.const_int (ltype_of_typ t) 0 in (L.define_global n init main_module)
			| A.Bool -> let init = L.const_int (ltype_of_typ t) 0 in (L.define_global n init main_module)
		)
		| A.Struct_typ(s) -> let init = L.const_named_struct (find_struct_name s) [||] in (L.define_global n init main_module)		

		| A.Pointer_typ(_) ->let init = L.const_pointer_null (ltype_of_typ t) in (L.define_global n init main_module)		

		| A.Array_typ(p,i) ->let init = L.const_array (prim_ltype_of_typ p) (array_of_zeros i (default_value_for_prim_type ((p)))) in (L.define_global n init main_module)		

		| A.Func_typ(_) ->let init = L.const_int (ltype_of_typ t) 0 in (L.define_global n init main_module)		
		| A.Any -> raise (Exceptions.BugCatch "define_global_with_value")


(* Where we add global variabes to global data section *)
let define_global_var (t, n) =
		match t with
		  A.Primitive(_) -> Hashtbl.add global_variables n (define_global_with_value (t,n))
		| A.Struct_typ(_) -> Hashtbl.add  global_variables n (define_global_with_value (t,n))
		| A.Pointer_typ(_) -> Hashtbl.add  global_variables n (define_global_with_value (t,n))
		| A.Array_typ(_,_) -> Hashtbl.add global_variables n (define_global_with_value (t,n))
		| A.Func_typ(_) -> Hashtbl.add global_variables n (L.declare_global (ltype_of_typ t) n main_module)
		| A.Any -> raise (Exceptions.BugCatch "define_global_with_value")

	
(* Translations functions to LLVM code in text section  *)
let translate_function functions the_module = 

(* Here we define the built in print function *)
let printf_t = L.var_arg_function_type i32_t [||] in
let printf_func = L.declare_function "printf" printf_t the_module in


(* Here we iterate through Ast.functions and add all the function names to a HashMap *)
	let function_decls =
		let function_decl m fdecl =
		let name = fdecl.S.sfname
        	and formal_types =
            	Array.of_list (List.map (fun (t,_) -> ltype_of_typ t) fdecl.S.sformals)
            	in let ftype = L.function_type (ltype_of_typ fdecl.S.styp) formal_types in
             	StringMap.add name (L.define_function name ftype the_module, fdecl) m in
    		List.fold_left function_decl StringMap.empty functions in

		(* Create format strings for printing *)
		let (main_function,_) = try StringMap.find "main" function_decls with | Not_found -> raise (Exceptions.BugCatch "function decls") in
		let builder = L.builder_at_end context (L.entry_block main_function) in
		(*let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder in *)
		let str_format_str = L.build_global_stringptr "%s\n" "fmt_string" builder in
		let int_format_str = L.build_global_stringptr "%d\n" "fmt_int" builder in
		let float_format_str = L.build_global_stringptr "%f\n" "fmt_float" builder in

(* Method to build body of function *)
	let build_function_body fdecl =
	let (the_function, _) = try StringMap.find fdecl.S.sfname function_decls with | Not_found -> raise (Exceptions.BugCatch "build function body") in
	(* builder is the LLVM instruction builder *)
	let builder = L.builder_at_end context (L.entry_block the_function) in

	
	(* This is where we push local variables onto the stack and add them to a local HashMap*)
	let local_vars = 
		let add_formal m(t, n) p = L.set_value_name n p;
		let local = L.build_alloca (ltype_of_typ t) n builder in
		ignore (L.build_store p local builder);
		StringMap.add n local m in

		let add_local m (t, n) =
        	let local_var = L.build_alloca (ltype_of_typ t) n builder
        	in StringMap.add n local_var m in

	(* This is where we push formal arguments onto the stack *)
	let formals = List.fold_left2 add_formal StringMap.empty fdecl.S.sformals
          (Array.to_list (L.params the_function)) in
          List.fold_left add_local formals fdecl.S.svdecls in


	(* Two places to look for a variable 1) local HashMap 2) global HashMap *)
	let find_var n = try StringMap.find n local_vars
		with Not_found -> try Hashtbl.find global_variables n
		with Not_found -> raise (Failure ("undeclared variable " ^ n))
		in

	let print_format_typ t =
			(match t with 
			  A.Primitive(A.Int) -> int_format_str
			 | A.Primitive(A.Double) -> float_format_str
			 | A.Primitive(A.String) -> str_format_str
			 | A.Primitive(A.Char) -> int_format_str
			 | A.Primitive(A.Bool) -> int_format_str
			 | _ -> raise (Exceptions.BugCatch "print format") 
			)
			in

	(* Format to print given arguments in print(...) *)
	let rec print_format e =
		(match e with 
		  (S.SString_lit(_)) -> str_format_str
		| (S.SLit(_)) -> int_format_str
		| (S.SDouble_lit(_)) -> float_format_str
		| S.SBinop(l,_,_,_) -> print_format l
		| S.SUnop(op,e,_) -> 
			(match op with
				A.Neg -> print_format e
				| _ -> raise (Exceptions.BugCatch "print format")
			)
		| S.SAssign(_,_) -> raise (Exceptions.InvalidPrintFormat) 
		| S.SNoexpr -> raise (Exceptions.InvalidPrintFormat) 
		| (S.SId(i)) -> let i_value = find_var i in 
			let i_type = L.type_of i_value in 
			let string_i_type = L.string_of_lltype i_type in 
			(match string_i_type with 
		    "i32*" -> int_format_str 
		  | "i1*" -> int_format_str 
		  | "i8**" -> str_format_str
		  | "float*" -> float_format_str
		  | "double*" -> float_format_str
		  | _ -> raise (Exceptions.InvalidPrintFormat)
			)		
		| S.SStruct_access(_,_,_,t) -> print_format_typ t
		| S.SPt_access(_,_,_,t) -> print_format_typ t
		| S.SArray_create(_,_) -> raise (Exceptions.InvalidPrintFormat) 
		| S.SArray_access(_,_,t) -> print_format_typ t
		| S.SDereference(_,t) -> print_format_typ t
		| S.SFree(_) -> raise (Exceptions.InvalidPrintFormat) 
		| S.SCall(f,_) ->let (_, fdecl) = try StringMap.find f function_decls with | Not_found -> raise (Exceptions.BugCatch "print format") in 
			let tmp_typ = fdecl.S.styp in print_format_typ tmp_typ	
		| S.SBoolLit(_) -> int_format_str
		| S.SNull(_) -> raise (Exceptions.InvalidPrintFormat) 
		| _ -> raise (Exceptions.InvalidPrintFormat) 
		)
		in

	(* Returns address of i. Used for lhs of assignments *)
	let rec addr_of_expr i builder= 
	match i with
	  S.SLit(_) -> raise Exceptions.InvalidLhsOfExpr
	| S.SString_lit (_) -> raise Exceptions.InvalidLhsOfExpr
	| S.SChar_lit (_) -> raise Exceptions.InvalidLhsOfExpr
 	| S.SId(s) -> find_var s
	| S.SBinop(_,_,_,_) ->raise (Exceptions.UndeclaredVariable("Unimplemented addr_of_expr"))
 	| S.SUnop(_,e,_) -> addr_of_expr e builder
	| S.SStruct_access(s,_,index,_) -> let tmp_value = find_var s in 
			let deref = L.build_struct_gep tmp_value index "tmp" builder in deref
	| S.SPt_access(s,_,index,_) -> let tmp_value = find_var s in 
			let load_tmp = L.build_load tmp_value "tmp" builder in 
			let deref = L.build_struct_gep load_tmp index "tmp" builder in deref
	| S.SDereference(s,_) -> let tmp_value = find_var s in 
			let deref = L.build_gep tmp_value [|L.const_int i32_t 0|] "tmp" builder in L.build_load deref "tmp" builder

	| S.SArray_access(ar,index, t) -> let tmp_value = find_var ar in 
		(match t with 
		  A.Array_typ(_) -> let deref = L.build_gep tmp_value [|L.const_int i32_t 0 ; L.const_int i32_t index|] "arrayvalueaddr" builder in deref 
		| A.Pointer_typ(_) -> let loaded_value = L.build_load tmp_value "tmp" builder in 
			let deref = L.build_gep loaded_value [|L.const_int i32_t 0 ; L.const_int i32_t index|] "arrayvalueaddr" builder in deref 
		| _ -> raise Exceptions.InvalidArrayAccess)
	| _ -> raise (Exceptions.UndeclaredVariable("Invalid LHS of assignment"))

	in 
	let add_terminal builder f =
          match L.block_terminator (L.insertion_block builder) with
        	  Some _ -> ()
      		| None -> ignore (f builder) in	

	(* This is where we build LLVM expressions *)
	let rec expr builder = function 
	  S.SLit l -> L.const_int i32_t l
	| S.SString_lit s -> let temp_string = L.build_global_stringptr s "str" builder in temp_string 
	| S.SChar_lit c -> L.const_int i8_t (C.code c)
	| S.SDouble_lit d -> L.const_float d_t d
	| S.SBinop (e1, op, e2,t) -> 
		let e1' = expr builder e1 
		and e2' = expr builder e2 in
		(match t with 
		  A.Primitive(A.Int) | A.Primitive(A.Char) -> (match op with 
		  A.Add -> L.build_add 
		| A.Sub -> L.build_sub
		| A.Mult -> L.build_mul
		| A.Div -> L.build_sdiv
		| A.Mod -> L.build_srem
		| A.Equal -> L.build_icmp L.Icmp.Eq
		| A.Neq -> L.build_icmp L.Icmp.Ne
		| A.Less -> L.build_icmp L.Icmp.Slt
		| A.Leq -> L.build_icmp L.Icmp.Sle
		| A.Greater -> L.build_icmp L.Icmp.Sgt
		| A.Geq -> L.build_icmp L.Icmp.Sge
		| A.And -> L.build_and
		| A.Or -> L.build_or
		| _ -> raise (Exceptions.BugCatch "Prim Binop")		
		)e1' e2' "add" builder
		| A.Primitive(A.Double) ->
		(match op with 
		  A.Add -> L.build_fadd 
		| A.Sub -> L.build_fsub
		| A.Mult -> L.build_fmul
		| A.Div -> L.build_fdiv
		| A.Mod -> L.build_frem
		| A.Equal -> L.build_fcmp L.Fcmp.Oeq
		| A.Neq -> L.build_fcmp L.Fcmp.One
		| A.Less -> L.build_fcmp L.Fcmp.Olt
		| A.Leq -> L.build_fcmp L.Fcmp.Ole
		| A.Greater -> L.build_fcmp L.Fcmp.Ogt
		| A.Geq -> L.build_fcmp L.Fcmp.Oge
		| A.And -> L.build_and
		| A.Or -> L.build_or
		| _ -> raise (Exceptions.BugCatch "Double Binop")
		) e1' e2' "addfloat" builder
		| A.Primitive(A.Bool) -> 
		(
		match op with 
		  A.And -> L.build_and
		| A.Or -> L.build_or
		| A.Equal -> L.build_icmp L.Icmp.Eq
		| _ -> raise (Exceptions.BugCatch "Binop")
		) e1' e2' "add" builder	
		| A.Pointer_typ(_) ->
			(match op with
			  A.Equal -> L.build_is_null
			| A.Neq -> L.build_is_not_null
			| _ -> raise (Exceptions.BugCatch "Binop")
			)e1' "add" builder
		| _ -> raise (Exceptions.BugCatch "Binop")) 		 

	| S.SUnop(u,e, t) -> 
			(match u with
				  A.Neg -> let e1 = expr builder e in 
				(match t with
				  A.Primitive(A.Int) ->  L.build_neg e1 "neg" builder
				| A.Primitive(A.Double) -> L.build_fneg e1 "neg" builder 
				| _ -> raise (Exceptions.BugCatch "expr builder")
				)
				| A.Not -> let e1 = expr builder e in L.build_not e1 "not" builder
				| A.Addr ->let iden = string_of_expr e in 
					   let lvalue = find_var iden in lvalue
			)
	| S.SAssign (l, e) -> let e_temp = expr builder e in 
		ignore(let l_val = (addr_of_expr l builder) in  (L.build_store e_temp l_val builder)); e_temp
	| S.SNoexpr -> L.const_int i32_t 0
	| S.SId (s) -> L.build_load (find_var s) s builder
	| S.SStruct_create(s) -> L.build_malloc (find_struct_name s) "tmp" builder
	| S.SStruct_access(s,_,index,_) -> let tmp_value = find_var s in 
			let deref = L.build_struct_gep tmp_value index "tmp" builder in 
			let loaded_value = L.build_load deref "dd" builder in loaded_value
	| S.SPt_access(s,_,index,_) -> let tmp_value = find_var s in 
			let load_tmp = L.build_load tmp_value "tmp" builder in 
			let deref = L.build_struct_gep load_tmp index "tmp" builder in 
			let tmp_value = L.build_load deref "dd" builder in tmp_value
	| S.SArray_create(i,p) -> let ar_type = L.array_type (prim_ltype_of_typ p) i in L.build_malloc ar_type "ar_create" builder 
	| S.SArray_access(ar,index,t) -> let tmp_value = find_var ar in 
		(match t with 
		  A.Pointer_typ(_) -> let loaded_value = L.build_load tmp_value "loaded" builder in  
			let deref = L.build_gep loaded_value [|L.const_int i32_t 0 ; L.const_int i32_t index|] "arrayvalueaddr" builder in 
			let final_value = L.build_load deref "arrayvalue" builder in final_value 
		| A.Array_typ(_) -> let deref = L.build_gep tmp_value [|L.const_int i32_t 0 ; L.const_int i32_t index|] "arrayvalueaddr" builder in 
			let final_value = L.build_load deref "arrayvalue" builder in final_value 
		| _ -> raise Exceptions.InvalidArrayAccess)
	| S.SDereference(s,_) -> let tmp_value = find_var s in 
			let load_tmp = L.build_load tmp_value "tmp" builder in 
			let deref = L.build_gep load_tmp [|L.const_int i32_t 0|] "tmp" builder in 			  let tmp_value2 = L.build_load deref "dd" builder in tmp_value2

	| S.SFree(s) -> let tmp_value = L.build_load (find_var s) "tmp" builder in L.build_free (tmp_value) builder
	| S.SCall("print", [e]) | S.SCall("print_int", [e])-> L.build_call printf_func [|(print_format e); (expr builder e) |] "printresult" builder
	| S.SCall(f, args) -> let (def_f, fdecl) = try StringMap.find f function_decls with | Not_found -> raise (Exceptions.BugCatch f) in
			      let actuals = List.rev (List.map (expr builder) (List.rev args)) in 				let result = (match fdecl.S.styp with A.Primitive(A.Void) -> "" | _ -> f ^ "_result") in L.build_call def_f (Array.of_list actuals) result builder
	| S.SBoolLit(b) -> L.const_int i1_t b
	| S.SNull(t) -> L.const_null (ltype_of_typ t)
	| S.SDubs -> let tmp_call = S.SCall("print", [(S.SString_lit("dubs!"))]) in expr builder tmp_call 	
	in


	(* This is where we build the LLVM statements *)
	let rec stmt builder = function 
	  S.SBlock b -> List.fold_left stmt builder b
	| S.SExpr e -> ignore (expr builder e); builder
	
	
	| S.SIf(pred, then_stmt, else_stmt) -> 
		(*let curr_block = L.insertion_block builder in *)
		(* the function (of type llvalue that we are currently in *)
		let bool_val = expr builder pred in
		let merge_bb = L.append_block context "merge" the_function in
		(* then block *)
		let then_bb = L.append_block context "then" the_function in

		add_terminal (stmt (L.builder_at_end context then_bb) then_stmt) (L.build_br merge_bb);
		(* else block*)
		let else_bb = L.append_block context "else" the_function in 
		add_terminal (stmt (L.builder_at_end context else_bb) else_stmt) (L.build_br merge_bb);	
		ignore (L.build_cond_br bool_val then_bb else_bb builder);
		L.builder_at_end context merge_bb
	| S.SWhile(pred,body_stmt) ->  
		let pred_bb = L.append_block context "while" the_function in
		ignore (L.build_br pred_bb builder);
		let body_bb = L.append_block context "while_body" the_function in
		add_terminal (stmt (L.builder_at_end context body_bb) body_stmt) (L.build_br pred_bb);
		let pred_builder = L.builder_at_end context pred_bb in
		let bool_val = expr pred_builder pred in
		let merge_bb = L.append_block context "merge" the_function in
		ignore(L.build_cond_br bool_val body_bb merge_bb pred_builder);	
		L.builder_at_end context merge_bb

	| S.SFor(e1,e2,e3,s) -> ignore(expr builder e1); let tmp_stmt = S.SExpr(e3) in 
			let tmp_block = S.SBlock([s] @ [tmp_stmt]) in  
			let tmp_while = S.SWhile(e2, tmp_block) in stmt builder tmp_while 
	| S.SReturn r -> ignore (match fdecl.S.styp with
						  A.Primitive(A.Void) -> L.build_ret_void builder
						| _ -> L.build_ret (expr builder r) builder); builder 
	in
	
	(* Build the body for this function *)
	let builder = stmt builder (S.SBlock fdecl.S.sbody) in
		
	add_terminal builder (match fdecl.S.styp with
          A.Primitive(A.Void) -> L.build_ret_void
        | _ -> L.build_ret (L.const_int i32_t 0) )
	in
	
(* Here we go through each function and build the body of the function *)
List.iter build_function_body functions;
the_module

(* Create a main function in test file - main then calls the respective tests *)
let test_main functions = 
	let tests = List.fold_left (fun l n -> (match n.S.stests with Some(t) -> l @ [t]  | None -> l)) [] functions in 
	let names_of_test_calls = List.fold_left (fun l n -> l @ [(n.S.sfname)]) [] tests in
	let print_stars = S.SExpr(S.SCall("print", [S.SString_lit("*************")])) in 
	let sast_calls = List.fold_left (fun l n -> l @ [S.SExpr(S.SCall("print",[S.SString_lit((cut_string n 4) ^ " results:")]))] @ [S.SExpr(S.SCall(n,[]))]@ [print_stars] ) [] names_of_test_calls in
	let print_stmt = [S.SExpr(S.SCall("print",[S.SString_lit("TEST RESULTS!")]))]@[print_stars] in 
	let tmp_main:(S.sfunc_decl) = { S.styp = A.Primitive(A.Void); S.sfname = "main"; S.sformals = []; S.svdecls = []; S.sbody = print_stmt@sast_calls; S.stests = None; S.sstruc_method = false ; S.sincludes_func = false } in tmp_main


let func_builder f b = 
	(match b with 
	  true -> let tests = List.fold_left (fun l n -> (match n.S.stests with Some(t) -> l @ [n] @ [t]  | None -> if (n.S.sstruc_method = false && n.S.sincludes_func = false) then (l) else (l@[n]))) [] f in (tests @ [(test_main f)]) 
	| false -> f
	)

(***********************************************************)
(* Entry point for translating Sast.program to LLVM module *)
(***********************************************************)
let gen_llvm (_, input_globals, input_functions, input_structs) gen_tests_bool = 
	let _ = List.iter declare_struct input_structs in
	let _ = List.iter define_struct_body input_structs in
	let _ = List.iter define_global_var input_globals in
	let the_module = (match gen_tests_bool with true -> test_module | false -> main_module) in
	let _ = translate_function (func_builder input_functions gen_tests_bool) the_module in
	the_module
