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

	 let ltype_of_typ = function
      		  A.Primitive(A.Int) -> i32_t
    		| _ -> void_t in

 let global_vars =
    let global_var m (t, n) =
      let init = L.const_int (ltype_of_typ t) 0
      in StringMap.add n (L.define_global n init the_module) m in
    List.fold_left global_var StringMap.empty globals in
	
	the_module;
