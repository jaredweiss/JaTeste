%{ open Ast %}

/*
   Tokens/terminal symbols 
*/
%token LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET COMMA SEMI POUND INCLUDE
%token PLUS MINUS STAR DIVIDE ASSIGN NOT MODULO EXPO AMPERSAND
%token FUNC
%token WTEST USING STRUCT DOT POINTER_ACCESS
%token EQ NEQ LT LEQ GT GEQ AND OR TRUE FALSE
%token INT DOUBLE VOID CHAR STRING BOOL
%token INT_PT DOUBLE_PT CHAR_PT STRUCT_PT
%token ARRAY
%token NEW FREE
%token RETURN IF ELSE WHILE FOR ASSERT

/* 
   Tokens with associated values 
*/
%token <int> INT_LITERAL
%token <string> DOUBLE_LITERAL
%token <char> CHAR_LITERAL
%token <string> STRING_LITERAL
%token <string> ID
%token <string> INCLUDE_FILE
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
%left STAR DIVIDE MODULO
%right EXPO
%right NOT NEG AMPERSAND
%right RBRACKET
%left LBRACKET
%right DOT POINTER_ACCESS

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

program: includes var_decls func_decls struc_decls  EOF { ($1, List.rev $2, List. rev $3, List.rev $4) } 

includes:
	  /* noting */ { [] }
	| includes include_file { $2 :: $1 }

include_file:
	  POUND INCLUDE STRING_LITERAL { (Curr, $3) } 
	| POUND INCLUDE LT INCLUDE_FILE GT       { (Standard,$4) }

var_decls: 			  
	/* nothing */ { [] }
	| var_decls vdecl   { $2::$1 }

func_decls:	
	 fdecl {[$1]}
	| func_decls fdecl  {$2::$1}

struc_decls:
	  /*nothing*/ { [] }
	| struc_decls sdecl {$2::$1}

prim_typ:
	| STRING 	{ String }
	| DOUBLE 	{ Double }
	| INT 		{ Int }
	| CHAR 		{ Char }
	| BOOL		{ Bool }

void_typ:
	| VOID 		{ Void }
	
struct_typ:
	| STRUCT ID { $2 }

array_typ:
	prim_typ LBRACKET INT_LITERAL RBRACKET  	{ ($1, $3) }

pointer_typ:
	| prim_typ STAR 		{ Primitive($1) }
	| struct_typ STAR 		{ Struct_typ($1) }
	| array_typ STAR 		{ Array_typ(fst $1, snd $1) }

double_pointer_typ:
	| pointer_typ STAR 		{ Pointer_typ($1)  }



any_typ:
	  prim_typ 		{ Primitive($1) }
	| struct_typ 		{ Struct_typ($1) }
	| pointer_typ 		{ Pointer_typ($1) }
	| double_pointer_typ 	{ Pointer_typ($1) }
	| void_typ 		{ Primitive($1) }
	| array_typ		{ Array_typ(fst $1, snd $1) }


any_typ_not_void:
	  	  prim_typ 	{ Primitive($1) }
		| struct_typ 	{ Struct_typ($1) }
		| pointer_typ 	{ Pointer_typ($1) }
		| double_pointer_typ 	{ Pointer_typ($1) }
		| array_typ	{ Array_typ(fst $1, snd $1) }

/* 
Rules for function syntax
*/
fdecl:
	  FUNC any_typ ID LPAREN formal_opts_list RPAREN LBRACE vdecl_list func_body RBRACE {{
		typ = $2; fname = $3; formals = $5; vdecls = List.rev $8; body = List.rev
		$9; tests = None }}
	| FUNC any_typ ID LPAREN formal_opts_list RPAREN LBRACE vdecl_list func_body RBRACE testdecl {{
		typ = $2; fname = $3; formals = $5; vdecls = List.rev $8; body = List.rev
		$9; tests = Some({asserts = $11;  using = { uvdecls = []; stmts = [] }})  }}
	| FUNC any_typ ID LPAREN formal_opts_list RPAREN LBRACE vdecl_list func_body RBRACE testdecl usingdecl {{
		typ = $2; fname = $3; formals = $5; vdecls = List.rev $8; body = List.rev
		$9; tests = Some({asserts = $11;  using = { uvdecls = (fst $12); stmts = (snd $12)}}) }}

/* 
"with test" rule 
*/
testdecl:
	WTEST LBRACE stmt_list RBRACE { $3 }

/* 
"using" rule 
*/
usingdecl:
	USING LBRACE vdecl_list stmt_list RBRACE { (List.rev $3, List.rev $4) }


/*
Formal parameter rules
*/
formal_opts_list:
	  /* nothing */    { [] }
	| formal_opt { $1 }

formal_opt:
	     any_typ_not_void ID 			{[($1,$2)]}
	   | formal_opt COMMA any_typ_not_void ID 	{($3,$4)::$1}

actual_opts_list:
	  /* nothing */ { [] }
	| actual_opt 	{ $1 }

actual_opt:
	     expr { [$1] }
	   | actual_opt COMMA expr {$3::$1}

/* 
Rule for declaring a list of variables, including variables of type struct x 
*/
vdecl_list: 
	  /* nothing */ { [] }
	| vdecl_list vdecl { $2::$1 }

/*
Includes declaring a struct
*/

vdecl:
	  any_typ_not_void ID SEMI { ($1, $2) }

/* 
Rule for defining a struct 
*/
sdecl:
	STRUCT ID LBRACE vdecl_list RBRACE SEMI {{
		sname = $2; attributes = List.rev $4 }}


func_body: 
	stmt_list 	{[Block(List.rev $1)]}

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
	  | ASSERT LPAREN expr RPAREN SEMI 			    { Assert($3) }

/* 
Rule for building expressions 
*/
expr:
	  INT_LITERAL 		{ Lit($1)}
	| STRING_LITERAL	{ String_lit($1) }  
	| TRUE			{ BoolLit(true) }
	| FALSE			{ BoolLit(false) }
	| ID 			{ Id($1) }
	| LPAREN expr RPAREN 	{ $2 }
	| expr PLUS expr 	{ Binop($1, Add, $3) }
	| expr MINUS expr 	{ Binop($1, Sub, $3) }
	| expr STAR expr 	{ Binop($1, Mult, $3)}
	| expr DIVIDE expr 	{ Binop($1, Div, $3)}
	| expr EQ  expr 	{ Binop($1, Equal, $3)}
	| expr EXPO  expr 	{ Binop($1, Exp, $3)}
	| expr MODULO  expr 	{ Binop($1, Mod, $3)}
	| expr NEQ  expr 	{ Binop($1, Neq, $3)}
	| expr LT expr 		{ Binop($1, Less, $3)}
	| expr LEQ  expr 	{ Binop($1, Leq, $3)}
	| expr GT expr 		{ Binop($1, Greater, $3)}
	| expr GEQ expr 	{ Binop($1, Geq, $3)}
	| expr AND  expr 	{ Binop($1, And, $3)}
	| expr OR expr 		{ Binop($1, Or, $3)}
	| NOT expr		{ Unop(Not, $2) }
	| AMPERSAND expr	{ Unop(Addr, $2) }
	| expr ASSIGN expr 	{ Assign($1, $3) }
	| expr DOT expr 	{ Struct_access($1, $3)}
	| expr POINTER_ACCESS expr 	{ Pt_access($1, $3)}
	| STAR expr 			{ Dereference($2) }
	| expr LBRACKET INT_LITERAL RBRACKET 	     { Array_access($1, $3)}
	| NEW prim_typ LBRACKET INT_LITERAL RBRACKET { Array_create($4, $2) }
	| NEW STRUCT ID 			     { Struct_create($3)}
	| FREE LPAREN expr RPAREN		     { Free($3) }
	| ID LPAREN actual_opts_list RPAREN          { Call($1, $3)}

expr_opt:
	  /* nothing */ { Noexpr }
	| expr 		{ $1 }
