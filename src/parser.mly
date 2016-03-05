%{ open Ast %}

%token LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET COMMA SEMI
%token PLUS MINUS TIMES DIVIDE ASSIGN NOT
%token FUNC
%token WTEST USING STRUCT DOT
%token EQ NEQ LT LEQ GT GEQ AND OR
%token INT DOUBLE VOID CHAR STRING
%token RETURN IF ELSE WHILE FOR

%token <int> INT_LITERAL
%token <string> DOUBLE_LITERAL
%token <char> CHAR_LITERAL
%token <string> STRING_LITERAL
%token <string> ID
%token EOF

%nonassoc NOELSE 
%nonassoc ELSE 
%right ASSIGN 
%left OR
%left AND
%left EQ NEQ
%left LT GT LEQ GEQ 
%left PLUS MINUS 
%left TIMES DIVIDE 
%right NOT NEG

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
	| INT 		{ Primitive(Int) }
	| VOID 		{ Primitive(Void) }

datatyp:
	| STRUCT { Struct_typ }

fdecl:
	  FUNC typ ID LPAREN RPAREN LBRACE vdecl_list stmt_list RBRACE {{
		typ = $2; fname = $3; vdecls = List.rev $7; body = List.rev $8 }}
	| FUNC typ ID LPAREN RPAREN LBRACE vdecl_list stmt_list RBRACE testdecl {{
		typ = $2; fname = $3; vdecls = List.rev $7; body = List.rev $8 }}
	| FUNC typ ID LPAREN RPAREN LBRACE vdecl_list stmt_list RBRACE testdecl usingdecl {{
		typ = $2; fname = $3; vdecls = List.rev $7; body = List.rev $8 }}

testdecl:
	WTEST LBRACE stmt_list RBRACE usingdecl { }

usingdecl:
	USING LBRACE stmt_list RBRACE { }

vdecl_list: 
	  /* nothing */ { [] }
	| vdecl_list vdecl { $2::$1 }

vdecl:
	typ ID SEMI { ($1, $2) }

sdecl:
	datatyp ID ASSIGN LBRACE vdecl_list RBRACE SEMI {{
		sname = $2; attributes = $5 }}

stmt_list:
	  /* nothing */ { [] }
	| stmt_list stmt { $2::$1 }

stmt:
	    expr SEMI { Expr $1 }
	  | LBRACE stmt RBRACE				       { $2 }
	  | IF LPAREN expr RPAREN stmt ELSE stmt 	       { If($3, $5, $7) }
	  | IF LPAREN expr RPAREN stmt %prec NOELSE 	       { If($3, $5, Block([])) }
	  | WHILE LPAREN expr RPAREN LBRACE vdecl_list stmt RBRACE 	       { While($3, $7) }
  	  | FOR LPAREN expr_opt SEMI expr SEMI expr_opt RPAREN { For($3, $5, $7)}
	  | RETURN SEMI					       { Return Noexpr}
	  | RETURN expr SEMI				       { Return $2 }


expr:
	  INT_LITERAL 		{ Lit($1)}
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
	| ID ASSIGN expr 	{ Assign($1, $3) }
	| LBRACE expr RBRACE    { $2 }
	| ID 			{ Id($1) }

expr_opt:
	  /* nothing */ { Noexpr }
	| expr 		{ $1 }
