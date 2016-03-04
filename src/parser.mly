%{ open Ast %}

%token LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET COMMA SEMI
%token PLUS MINUS TIMES DIVIDE ASSIGN
%token FUNC
%token WTEST USING STRUCT DOT
%token EQ NEQ LT LEQ GT GEQ
%token INT DOUBLE VOID CHAR STRING
%token RETURN IF ELSE WHILE FOR

%token <int> INT_LITERAL
%token <string> DOUBLE_LITERAL
%token <char> CHAR_LITERAL
%token <string> STRING_LITERAL
%token <string> ID
%token EOF

%left PLUS MINUS
%left TIMES DIVIDE
%right ASSIGN

%start program
%type<Ast.program> program

%%
program: decls EOF { List.rev $1 } 

decls: 			  
	/* nothing */ 	{ [] } 
	| decls fdecl   { Func($2)::$1 }
	| decls vdecl   { Var($2)::$1 }
	| decls sdecl   { Struct($2)::$1 }

typ:
	  | STRING 	{ Primitive(String) }
	  | DOUBLE 	{ Primitive(Double) }
	  | INT 	{ Primitive(Int) }
	  | VOID 	{ Primitive(Void) }
	  | STRUCT  	{ Struct }

fdecl:
	  FUNC typ ID LPAREN RPAREN LBRACE stmt_list RBRACE {{
		typ = $2; fname = $3; body = $7 }}
	| FUNC typ ID LPAREN RPAREN LBRACE stmt_list RBRACE testdecl {{
		typ = $2; fname = $3; body = $7 }}
	| FUNC typ ID LPAREN RPAREN LBRACE stmt_list RBRACE testdecl usingdecl {{
		typ = $2; fname = $3; body = $7 }}

testdecl:
	WTEST LBRACE stmt_list RBRACE usingdecl { }

usingdecl:
	USING LBRACE stmt_list RBRACE { }

vdecl_list: 
	  /* nothing */ { [] }
	| vdecl_list vdecl { $2::$1}

vdecl:
	typ ID SEMI {($1, $2) }

sdecl:
	STRUCT STRING_LITERAL ASSIGN LBRACE vdecl_list RBRACE {{
		sname = $2; attributes = $5 }}

stmt_list:
	  /* nothing */ { [] }
	| stmt_list stmt { $2::$1}

stmt:
	  expr SEMI { Expr $1 }


expr:
	  INT_LITERAL { Lit($1)}
	| expr PLUS expr { Binop($1, Add, $3) }
	| expr MINUS expr { Binop($1, Sub, $3) }
	| ID ASSIGN expr { Assign($1, $3) }
