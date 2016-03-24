module StringMap = Map.Make(String)

let check program = 
	(ignore program);
	let check_elements elements = (ignore elements); () in
	

List.iter check_elements program;
