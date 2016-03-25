module L = Llvm
module A = Ast

module StringMap = Map.Make(String)


let translate (globals, functions, structs) = 
	let context = L.global_context () in
	let the_module = L.create_module context "Jateste" 
	and i32_t = L.i32_type context
	and i8_t = L.i8_type context
	and i1_t = L.i1_type context
	and void_t = L.void_type context in

	
	the_module;
