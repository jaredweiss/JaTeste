{ open Parser }

(* Regex shorthands *)
let digit = ['0' - '9']
let int = digit+
let double = int | digit*['.']digit+ | digit+['.']digit*
let char = '''['a' - 'z' 'A' - 'Z']'''
let newline = '\n'
let string = '"' (['a' - 'z'] | [' '] | ['A' - 'Z'] | ['_'] | '!' | ',' )+ '"'

rule token = parse
	   [' ' '\t' '\r' '\n' ] { token lexbuf } (* White space *)
	| "/*"			{ comment lexbuf }
	| '('			{ LPAREN }
	| ')'			{ RPAREN }
	| '{'			{ LBRACE}
	| '}'			{ RBRACE}
	| ','			{ COMMA }
	| ';'			{ SEMI }
		
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

	(* Control flow *)
	| "if"			{ IF }
	| "else"		{ ELSE }
	| "return" 		{ RETURN }
	| "while" 		{ WHILE }
	| "for"			{ FOR }
	
	(* Datatypes *)
	| "void"		{ VOID }
	| "struct"		{ STRUCT }
	| "double"		{ DOUBLE }
	| "int"			{ INT }
	| "char"		{ CHAR }
	| "string"		{ STRING }
	| "func" 		{ FUNC }
	| "new"			{ NEW }
	| "[]"			{ ARRAY }

	(* Testing keywords *)
	| "with test" 		{ WTEST }
	| "using"		{ USING }

	| ['a' - 'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm)}
	| int as lxm   		{ INT_LITERAL(int_of_string lxm)}
	| double as lxm 	{ DOUBLE_LITERAL(lxm) }
	| char as lxm 		{ CHAR_LITERAL(String.get lxm 1) }
	| string as lxm 	{ STRING_LITERAL(lxm) } 

	| eof { EOF }
	| _ as char { raise (Failure ("illegal character " ^
			Char.escaped char))}


(* Whitespace*)
and comment = parse
	"*/" { token lexbuf }
	| _ { comment lexbuf }

