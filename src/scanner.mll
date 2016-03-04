{ open Parser }

let digit = ['0' - '9']

rule token = parse
	   [' ' '\t' '\r' '\n' ] { token lexbuf } (* White space *)
	| "/*"			{ comment lexbuf }
	| '('			{ LPAREN }
	| ')'			{ RPAREN }
	| '{'			{ LBRACE}
	| '}'			{ RBRACE}
	| ';'			{ SEMI }
	| '+'			{ PLUS }
	| '-'			{ MINUS }
	| '*'			{ TIMES }
	| '/'			{ DIVIDE }
	| '='			{ ASSIGN }
	| "=="			{ EQ }
	| "!="			{ NEQ }
	| "<"			{ LT }
	| ">"			{ GT }
	| "<="			{ LEQ}
	| ">="			{ GEQ}
	| "void"		{ VOID }
	| "struct"		{ STRUCT }
	| "double"		{ DOUBLE }
	| "int"			{ INT }
	| "char"		{ CHAR }
	| "if"			{ IF }
	| "else"		{ ELSE }
	| "return" 		{ RETURN }
	| "while" 		{ WHILE }
	| "func" 		{ FUNC }
	| "with test" 		{ WTEST }
	| "using"		{ USING }
	| ['a' - 'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm)}
	| digit+ as lxm   { LITERAL(int_of_string lxm)}
	| eof { EOF }
	| _ as char { raise (Failure ("illegal character " ^
			Char.escaped char))}


and comment = parse
	"*/" { token lexbuf }
	| _ { comment lexbuf }

