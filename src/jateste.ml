open Printf 
module A = Ast
module S = Sast


type action = Scan | Parse |  Ast | Sast | Compile | Compile_with_test

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
		| "-ast" -> Ast
		| _ -> raise (Exceptions.IllegalArgument arg)
		)
	
	| _ -> raise (Exceptions.IllegalArgument "Can't recognize arguments")
	)

let executable_filename filename =
	let len = String.length filename in
	let str = String.sub filename 0 (len - 3) in
	let exec = String.concat "" [str ; ".ll"] in
	exec 

let test_executable_filename filename =
	let len = String.length filename in
	let str = String.sub filename 0 (len - 3) in
	let exec = String.concat "" [str ; "-test.ll"] in
	exec 

let create_header_files file = 
		let ic = open_in file in
  	try 
    	let line = input_line ic in 
    	print_endline line;        
    	flush stdout;             
    	close_in ic                
  
  	with e ->                     
    	close_in_noerr ic;           
    	raise e  

let scan input_raw = 
	let lexbuf = Lexing.from_channel input_raw in (print_string "Scanned\n"); lexbuf

let parse input_raw =
	let input_tokens = scan input_raw in
	let ast:(A.program) = Parser.program Scanner.token input_tokens in (print_string "Parsed\n"); ast

let semant input_raw = 
	let input_ast = parse input_raw in
	let sast:(S.sprogram) = Semant.check input_ast in (print_string "Semantic check passed\n"); sast

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

(******************************)
(* Entry pointer for Compiler *)
(******************************)
let _ =
	(* Read in command line args *)
	let arguments = Sys.argv in
	let action = determine_action arguments in
	let source_file = open_in arguments.((Array.length Sys.argv - 1)) in
	(* Create a file to put executable in *)
	let exec_name = executable_filename arguments.((Array.length Sys.argv -1)) in
	let test_exec_name = test_executable_filename arguments.((Array.length Sys.argv -1)) in
	
	let _ = (match action with 
	  Scan -> let _  = scan source_file in ()
 	| Parse -> let _ = parse source_file in ()
 	| Ast ->  let _ = parse source_file in ()
 	| Sast ->  let _ = semant source_file in () 
	| Compile ->  let _ = code_gen source_file exec_name false in ()
	| Compile_with_test -> let _ = code_gen source_file exec_name false in let source_test_file = open_in arguments.((Array.length Sys.argv - 1)) in let _ = code_gen source_test_file test_exec_name true in ()
	) in
	close_in source_file
