open Printf 
module A = Ast
module S = Sast


(* Location of Jateste's standard library *)
let standard_library_path = (Sys.getenv "JATESTE_LIB")
let current_dir_path = "./"

type action = Scan | Parse |  Ast | Sast | Compile | Compile_with_test

(* Determines what action compiler should take based on command line args *)
let determine_action args = 
	let num_args = Array.length args in
	(match num_args with
	  1 -> raise Exceptions.IllegalInputFormat
	| 2 -> Compile
	| 3 -> let arg = Array.get args 1 in 
		(match arg with
		  "-t" -> Compile_with_test
		| "-l" -> Scan
		| "-p" -> Parse
		| "-se" ->Sast
		| "-ast" -> Ast
		| _ -> raise (Exceptions.IllegalArgument arg)
		)
	
	| _ -> raise (Exceptions.IllegalArgument "Can't recognize arguments")
	)

(* Create executable filename *)
let executable_filename filename =
	let len = String.length filename in
	let str = String.sub filename 0 (len - 3) in
	let exec = String.concat "" [str ; ".ll"] in
	exec 

(* Create test executable filename *)
let test_executable_filename filename =
	let len = String.length filename in
	let str = String.sub filename 0 (len - 3) in
	let exec = String.concat "" [str ; "-test.ll"] in
	exec 

(* Just scan input *)
let scan input_raw = 
	let lexbuf = Lexing.from_channel input_raw in (print_string "Scanned\n"); lexbuf

(* Scan, then parse input *)
let parse input_raw =
	let input_tokens = scan input_raw in
	let ast:(A.program) = Parser.program Scanner.token input_tokens in (print_string "Parsed\n"); ast

(* Process include statements. Input is ast, and output is a new ast *)
let process_headers ast:(A.program) =
	let (includes,_,_,_) = ast in
	let gen_header_code (incl,globals, current_func_list, structs) (path, str) = 
		let tmp_path = (match path with A.Curr -> current_dir_path | A.Standard -> standard_library_path) in
		let file = tmp_path ^ str in
		let ic = 
		try open_in file with _ -> raise (Exceptions.InvalidHeaderFile file) in
		let (_,_,funcs,strs) = parse ic in
		let tmp_funcs = List.map (fun n -> let tmp = {A.typ = n.A.typ ; A.fname = n.A.fname ; A.formals = n.A.formals ; A.vdecls = n.A.vdecls ; A.body = n.A.body ; A.tests = n.A.tests ; A.struc_method = false ; A.includes_func = true } in tmp) funcs in
		let new_ast:(A.program) = (incl, globals, current_func_list @ tmp_funcs, structs @ strs) in
		new_ast 	
	in
	let modified_ast:(A.program) = List.fold_left gen_header_code ast includes in 
	modified_ast


(* Scan, parse, and run semantic checking. Returns Sast *)
let semant input_raw = 
	let tmp_ast = parse input_raw in
	let input_ast = process_headers tmp_ast in
	let sast:(S.sprogram) = Semant.check input_ast in (print_string "Semantic check passed\n"); sast

(* Generate code given file. @bool_tests determines whether to create a test file *)
let code_gen input_raw exec_name bool_tests =
	let input_sast = semant input_raw in
	let file = exec_name in
	let oc = open_out file in
	let m = Codegen.gen_llvm input_sast bool_tests in 
	Llvm_analysis.assert_valid_module m;
	fprintf oc "%s\n" (Llvm.string_of_llmodule m); 
	close_out oc;
	()

let get_ast input_raw =
	let ast = parse input_raw in
	ast

(*************************************************************************************)
(* Entry pointer for compiler. Input is a .jt text file, output is LLVM code in a .ll file. *)
(* 
file.jt text file -> 
scanner.mll: convert raw text to tokens according to regexes -> 
parser.mly: creates Ast according to CFG defined in parser.mly -> 
semant.ml: checks the semantics of the program (e.g. type checking), and converts the Ast into an Sast -> 
codege.ml: takes Sast as input and creates LLVM code in a .ll file ->
file.ll file 
*)
(*************************************************************************************)
let _ =
	(* Read in command line args *)
	let arguments = Sys.argv in
	(* Determine what the compiler should do based on command line args *)
	let action = determine_action arguments in
	let source_file = open_in arguments.((Array.length Sys.argv - 1)) in
	(* Create a file to put executable in *)
	let exec_name = executable_filename arguments.((Array.length Sys.argv -1)) in
	(* Create a file to put test executable in *)
	let test_exec_name = test_executable_filename arguments.((Array.length Sys.argv -1)) in
	
	(* Determine what the compiler should do, then do it *)
	let _ = (match action with 
	  Scan -> let _  = scan source_file in ()
 	| Parse -> let _ = parse source_file in ()
 	| Ast ->  let _ = parse source_file in ()
 	| Sast ->  let _ = semant source_file in () 
	| Compile ->  let _ = code_gen source_file exec_name false in ()
	| Compile_with_test -> let _ = code_gen source_file exec_name false in 
			let source_test_file = open_in arguments.((Array.length Sys.argv - 1)) in 			  let _ = code_gen source_test_file test_exec_name true in ()
	) in
	close_in source_file
