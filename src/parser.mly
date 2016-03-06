%{ open Ast %}

/*
   Tokens/terminal symbols 
*/
%token LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET COMMA SEMI
%token PLUS MINUS TIMES DIVIDE ASSIGN NOT MODULO EXPO
%token FUNC
%token WTEST USING STRUCT DOT
%token EQ NEQ LT LEQ GT GEQ AND OR 
%token INT DOUBLE VOID CHAR STRING 
%token INT_PT DOUBLE_PT CHAR_PT STRUCT_PT
%token INT_ARRAY DOUBLE_ARRAY CHAR_ARRAY
%token NEW
%token RETURN IF ELSE WHILE FOR

/* 
   Tokens with associated values 
*/
%token <int> INT_LITERAL
%token <string> DOUBLE_LITERAL
%token <char> CHAR_LITERAL
%token <string> STRING_LITERAL
%token <string> ID
%token EOF

/* 
   Precedence rules 
*/
%nonassoc NOELSE 
%nonassoc ELSE 
%right ASSIGN 
%left OR
%left AND
%left EQ NEQ
%left LT GT LEQ GEQ 
%left PLUS MINUS 
%left TIMES DIVIDE MODULO
%right EXPO
%right NOT NEG
%right DOT

/* 
   Start symbol 
*/

%start program

/* 
   Returns AST of type program 
*/

%type<Ast.program> program

%%

/* 
   Use List.rev on any rule that builds up a list in reverse. Lists are built in reverse
   for efficiency reasons 
 */

program: decls EOF { List.rev $1 } 

decls: 			  
	/* nothing */ 	{ [] } 
	| decls fdecl   { Func($2)::$1 }
	| decls vdecl   { Var($2)::$1 }
	| decls sdecl   { Struct($2)::$1 }
	| decls stmt 	{ Stmt($2)::$1 }

prim_typ:
	| STRING 	{ String }
	| DOUBLE 	{ Double }
	| INT 		{ Int }
	| CHAR 		{ Char}

void_typ:
	| VOID 		{ Void }
	
pointer_typ:
	| INT_PT	{ Primitive(Int) }
	| CHAR_PT	{ Primitive(Char) }
	| STRUCT_PT ID	{ Struct_typ($2) }

struct_typ:
	| STRUCT ID { $2 }

array_typ:
	INT_ARRAY 	{ Int }

any_typ:
	  prim_typ 	{ Primitive($1) }
	| struct_typ 	{ Struct_typ($1) }
	| pointer_typ 	{ Pointer_typ($1) }
	| void_typ 	{ Primitive($1) }
	| array_typ	{ Array_typ($1) }


any_typ_not_void:
	  		  prim_typ 	{ Primitive($1) }
			| struct_typ 	{ Struct_typ($1) }
			| pointer_typ 	{ Pointer_typ($1) }
			| array_typ	{ Array_typ($1) }

/* 
Rules for function syntax
*/
fdecl:
	  FUNC any_typ ID LPAREN formal_opts_list RPAREN LBRACE vdecl_list stmt_list RBRACE {{
		typ = $2; fname = $3; formals = $5; vdecls = List.rev $8; body = List.rev $9 }}
	| FUNC any_typ ID LPAREN formal_opts_list RPAREN LBRACE vdecl_list stmt_list RBRACE testdecl {{
		typ = $2; fname = $3; formals = $5; vdecls = List.rev $8; body = List.rev $9 }}
	| FUNC any_typ ID LPAREN formal_opts_list RPAREN LBRACE vdecl_list stmt_list RBRACE testdecl usingdecl {{
		typ = $2; fname = $3; formals = $5; vdecls = List.rev $8; body = List.rev $9 }}

/* 
"with test" rule 
*/
testdecl:
	WTEST LBRACE stmt_list RBRACE usingdecl { }

/* 
"using" rule 
*/
usingdecl:
	USING LBRACE stmt_list RBRACE { }


formal_opts_list:
	  /* nothing */    { [] }
	| formal_opt { $1 }

formal_opt:
	     any_typ_not_void ID 			{[($1,$2)]}
	   | formal_opt COMMA any_typ_not_void ID 	{($3,$4)::$1}

/* 
Rule for declaring a list of variables, including variables of type struct x 
*/
vdecl_list: 
	  /* nothing */ { [] }
	| vdecl_list vdecl { $2::$1 }

vdecl:
	  any_typ_not_void ID SEMI { ($1, $2) }

/* 
Rule for defining a struct 
*/
sdecl:
	STRUCT ID ASSIGN LBRACE vdecl_list RBRACE SEMI {{
		sname = $2; attributes = $5 }}


stmt_list:
	  /* nothing */ { [] }
	| stmt_list stmt { $2::$1 }

/* 
Rule for statements. Statments include expressions 
*/
stmt:
	    expr SEMI 						    { Expr $1 }
	  | LBRACE stmt_list RBRACE				    { Block(List.rev $2) }
	  | RETURN SEMI					            { Return Noexpr}
	  | RETURN expr SEMI				            { Return $2 }
	  | IF LPAREN expr RPAREN stmt ELSE stmt 	            { If($3, $5, $7) }
	  | IF LPAREN expr RPAREN stmt %prec NOELSE 	       	    { If($3, $5, Block([])) }
	  | WHILE LPAREN expr RPAREN stmt 		       	    { While($3, $5) }
  	  | FOR LPAREN expr_opt SEMI expr SEMI expr_opt RPAREN stmt { For($3, $5, $7, $9)}


/* 
Rule for building expressions 
*/
expr:
	  INT_LITERAL 		{ Lit($1)}
	| ID 			{ Id($1) }
	| expr PLUS expr 	{ Binop($1, Add, $3) }
	| expr MINUS expr 	{ Binop($1, Sub, $3) }
	| expr TIMES expr 	{ Binop($1, Mult, $3)}
	| expr DIVIDE expr 	{ Binop($1, Div, $3)}
	| expr EQ  expr 	{ Binop($1, Equal, $3)}
	| expr NEQ  expr 	{ Binop($1, Neq, $3)}
	| expr LT expr 		{ Binop($1, Less, $3)}
	| expr LEQ  expr 	{ Binop($1, Leq, $3)}
	| expr GT expr 		{ Binop($1, Greater, $3)}
	| expr GEQ expr 	{ Binop($1, Geq, $3)}
	| expr AND  expr 	{ Binop($1, And, $3)}
	| expr OR expr 		{ Binop($1, Or, $3)}
	| NOT expr		{ Unop(Not, $2) }
	| expr ASSIGN expr 	{ Assign($1, $3) }
	| expr DOT expr 	{ Struct_Access($1, $3)}
	| NEW prim_typ LBRACKET INT_LITERAL RBRACKET { Array_create($4, $2) }

expr_opt:
	  /* nothing */ { Noexpr }
	| expr 		{ $1 }
