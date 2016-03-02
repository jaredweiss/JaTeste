%{ open Ast %}

%token LPAREN RPAREN LBRACE RBRACE SEMI
%token PLUS MINUS TIMES DIVIDE ASSIGN
%token FUNC
%token WTEST USING
%token EQ NEQ LT LEQ GT GEQ
%token INT VOID CHAR
%token RETURN IF ELSE WHILE 

%token <int> LITERAL
%token <string> ID
%token EOF

%left PLUS MINUS
%left TIMES DIVIDE
%right ASSIGN

%start program
%type<Ast.program> program

%%
program: decls EOF { $1 }

decls: 
	/* nothing */ 	{ [] }
	| decls fdecl   { Func($2)::$1 }
	| decls vdecl   { Var($2)::$1 }

typ:
	    INT { Int }
	  | VOID { Void }

fdecl:
	  FUNC typ ID LPAREN RPAREN LBRACE stmt_list RBRACE {{
		typ = $2; fname = $3; body = $7 }}
	| FUNC typ ID LPAREN RPAREN LBRACE stmt_list RBRACE WTEST LBRACE stmt_list RBRACE{{
		typ = $2; fname = $3; body = $7 }}
	| FUNC typ ID LPAREN RPAREN LBRACE stmt_list RBRACE WTEST LBRACE
	stmt_list RBRACE USING LBRACE stmt_list RBRACE{{
		typ = $2; fname = $3; body = $7 }}

vdecl:
	typ ID SEMI {($1, $2) }

stmt_list:
	  /* nothing */ { [] }
	| stmt_list stmt { $2::$1}

stmt:
	  expr SEMI { Expr $1 }

expr:
	  LITERAL { Lit($1)}
	| expr PLUS expr { Binop($1, Add, $3) }
	| expr MINUS expr { Binop($1, Sub, $3) }
	| ID ASSIGN expr { Assign($1, $3) }
