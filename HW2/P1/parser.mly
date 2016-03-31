%{ open Ast %}

%token EOF
%token <string> FLOAT

%start program
%type <Ast.program> program

%%

program: 
	decls EOF { $1 }

decls:
	/* nothing */  { [] }
	| decls FLOAT { Literal($2)::$1 }
