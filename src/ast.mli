type op = Add | Sub

type prim = Int | Void
type typ = Primitive of prim | Struct
type bind = typ * string

type expr =
	  Lit 		of int
	| Binop 	of expr * op * expr
	| Assign 	of string * expr

type stmt =
	Expr of expr

type func_decl = {
	typ		:	typ;
	fname	:	string;
	body	: 	stmt list;
}

type struct_decl = {
	sname		:	string;
	attributes	:	bind list;
}

type flow = 
	| Var 		of bind
	| Struct 	of struct_decl
	| Func 		of func_decl

type program = flow list 
