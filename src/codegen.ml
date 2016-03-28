module L = Llvm
module A = Ast

module StringMap = Map.Make(String)

	let context = L.global_context () 
	let the_module = L.create_module context "Jateste" 


(* Overall function to translate Ast.program to LLVM module *)
let translate (globals, functions, structs) = 
	ignore(globals);ignore(functions);ignore(structs);
	let i32_t = L.i32_type context
	and i8_t = L.i8_type context
	and i1_t = L.i1_type context
	and d_t = L.double_type context
	and void_t = L.void_type context in
	let str_t = L.pointer_type i8_t in

	ignore(i32_t);
	ignore(i1_t);
	ignore(i8_t);
	ignore(void_t);

	let typ_of = function
		  "string" -> A.Primitive(A.String)
		| _ -> A.Primitive(A.Int)
		in 

	 let ltype_of_typ = function
		  A.Primitive(A.Int) -> i32_t
		| A.Primitive(A.Double) -> d_t
		| A.Primitive(A.Char) -> i8_t
		| A.Primitive(A.String) -> str_t
    		| _ -> void_t in

	(* HashMap of all global variables *)
 	let global_vars =
         let global_var m (t, n) =
           let init = L.const_int (ltype_of_typ t) 0
            in StringMap.add n (L.define_global n init the_module) m in
        List.fold_left global_var StringMap.empty globals in

	(* Here we define the built in print function *)
 	let printf_t = L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
         let printf_func = L.declare_function "printf" printf_t the_module in

	(* Here we iterate through Ast.functions and add all the function names
	   to a HashMap *)
	let function_decls =
         let function_decl m fdecl =
           let name = fdecl.A.fname
            and formal_types =
        Array.of_list (List.map (fun (t,_) -> ltype_of_typ t) fdecl.A.formals)
      in let ftype = L.function_type (ltype_of_typ fdecl.A.typ) formal_types in
      StringMap.add name (L.define_function name ftype the_module, fdecl) m in
    List.fold_left function_decl StringMap.empty functions in

	(* Method to build body of function *)
	let build_function_body fdecl =
    	 let (the_function, _) = StringMap.find fdecl.A.fname function_decls in
	  (* builder is the LLVM instruction builder *)
          let builder = L.builder_at_end context (L.entry_block the_function) in

	let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder in 
	let str_format_str = L.build_global_stringptr "%s\n" "fmt" builder in

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
	let formals = List.fold_left2 add_formal StringMap.empty fdecl.A.formals
          (Array.to_list (L.params the_function)) in
      List.fold_left add_local formals fdecl.A.vdecls in

	(* Two places to look for a variable 1) local HashMap 2) global HashMap *)
	let find_var n = try StringMap.find n local_vars
                 with Not_found -> try StringMap.find n global_vars
                 with Not_found -> raise (Failure ("undeclared variable " ^ n))
    in

      
	let add_terminal builder f =
          match L.block_terminator (L.insertion_block builder) with
        	  Some _ -> ()
      		| None -> ignore (f builder) in	

	(* This is where we build LLVM expressions *)
	let rec expr builder = function 
	   A.Lit l -> L.const_int i32_t l
	 | A.Id s -> L.build_load (find_var s) s builder
	 | A.Call("print", [e]) -> L.build_call printf_func [|str_format_str; (expr builder e) |] "printf" builder
	 | A.String_Lit s -> let temp_string = L.build_global_stringptr s "str" builder in temp_string 
	 | A.Assign (e1, e2) -> let x = 10 in L.const_int i32_t 0
	 | _ -> L.const_int i32_t 0 
	in


	(* This is where we build the LLVM statements *)
	let rec stmt builder = function 
	  A.Block b -> List.fold_left stmt builder b
	| A.Expr e -> ignore (expr builder e); builder
	| A.Return r -> ignore (match fdecl.A.typ with
						  A.Primitive(A.Void) -> L.build_ret_void builder
						| _ -> L.build_ret (expr builder r) builder); builder
	
	| _ -> builder
	in
	
	(* Build the body for this function *)
	let builder = stmt builder (A.Block fdecl.A.body) in
		
	add_terminal builder (match fdecl.A.typ with
        A.Primitive(A.Void) -> L.build_ret_void
      | _ -> L.build_ret (L.const_int i32_t 0) )
	in
	
	(* Here we go through each function and build the body of the function *)
	List.iter build_function_body functions;


	(* The LLVM module we return that contains the translated code *)
 	the_module
