module L = Llvm

module StringMap = Map.Make(String)


let translate program = 
	(ignore program);
	let context = L.global_context () in
	let the_module = L.create_module context "Jateste" 
	and i32_t = L.i32_type context
	and i8_t = L.i8_type context
	and i1_t = L.i1_type context
	and void_t = L.void_type context in

	(ignore i32_t);(ignore i1_t);(ignore i8_t);(ignore void_t);

	let print_f = L.var_arg_function_type i32_t [| L.pointer_type i8_t|] in
	let printf_func = L.declare_function "print" print_f the_module in

	let add_global p = () in

	List.iter add_global program;

	the_module;
