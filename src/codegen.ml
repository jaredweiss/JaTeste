module L = Llvm
module A = Ast

module StringMap = Map.Make(String)


let translate program = 
	(ignore program);
	let context = L.global_context () in
	let the_module = L.create_module context "Jateste" in
	the_module;
