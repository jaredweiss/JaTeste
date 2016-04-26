{ open Parser }

(* Regex shorthands *)
let digit = ['0' - '9']
let my_int = digit+
let double = (digit+) ['.'] digit+
let my_char = '''['a' - 'z' 'A' - 'Z']'''
let newline = '\n'
let my_string = '"' (['a' - 'z'] | [' '] | ['A' - 'Z'] | ['_'] | '!' | ',' )+ '"'

rule token = parse
	   [' ' '\t' '\r' '\n' ] { token lexbuf } (* White space *)
	| "/*"			{ comment lexbuf }
	| '('			{ LPAREN }
	| ')'			{ RPAREN }
	| '{'			{ LBRACE}
	| '}'			{ RBRACE}
	| ','			{ COMMA }
	| ';'			{ SEMI }
	| '#'			{ POUND }
	
	(*Header files *)
	| "include_jtlib"		{ INCLUDE }
		
	(* Operators *)
	| "+"			{ PLUS }
	| "-"			{ MINUS }
	| "*"			{ STAR }
	| "/"			{ DIVIDE }
	| "%"			{ MODULO }
	| "^"			{ EXPO }
	| "="			{ ASSIGN }
	| "=="			{ EQ }
	| "!="			{ NEQ }
	| "!"			{ NOT }
	| "&&"			{ AND }
	| "&"			{ AMPERSAND }
	| "||"			{ OR }
	| "<"			{ LT }
	| ">"			{ GT }
	| "<="			{ LEQ }
	| ">="			{ GEQ }
	| "["			{ LBRACKET }
	| "]"			{ RBRACKET }
	| "."			{ DOT }
	| "->"			{ POINTER_ACCESS }

	(* Control flow *)
	| "if"			{ IF }
	| "else"		{ ELSE }
	| "return" 		{ RETURN }
	| "while" 		{ WHILE }
	| "for"			{ FOR }
	| "assert" 		{ ASSERT }
	
	(* Datatypes *)
	| "void"		{ VOID }
	| "struct"		{ STRUCT }
	| "double"		{ DOUBLE }
	| "int"			{ INT }
	| "char"		{ CHAR }
	| "string"		{ STRING }
	| "bool"		{ BOOL }
	| "true"		{ TRUE }
	| "false"		{ FALSE }
	| "func" 		{ FUNC }
	| "new"			{ NEW }
	| "free"		{ FREE }

	(* Testing keywords *)
	| "with test" 		{ WTEST }
	| "using"		{ USING }

	| ['a' - 'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm)}
	| ['a' - 'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* ".jt" as lxm { INCLUDE_FILE(lxm) }
	| my_int as lxm   		{ INT_LITERAL(int_of_string lxm)}
	| double as lxm 		{ DOUBLE_LITERAL((float_of_string lxm)) }
	| my_char as lxm 		{ CHAR_LITERAL(String.get lxm 1) }
	| '"' {let buffer = Buffer.create 1 in STRING_LITERAL(string_find buffer lexbuf) }

	| eof { EOF }
	| _ as char { raise (Failure ("illegal character " ^
			Char.escaped char))}


(* Whitespace*)
and comment = parse
	"*/" { token lexbuf }
	| _ { comment lexbuf }

and string_find buffer = parse 
	  '"' {Buffer.contents buffer }
	| _ as chr { Buffer.add_char buffer chr; string_find buffer lexbuf }

