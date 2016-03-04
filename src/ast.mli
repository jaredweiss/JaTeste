type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq | And | Or
type uop = Neg | Not
type prim = Int | Double | String | Void
type typ = Primitive of prim | Struct_typ | Func_typ
type bind = typ * string

type expr =
	  Lit 		of int
	| Binop 	of expr * op * expr
	| Unop 		of uop * expr
	| Assign 	of string * expr
	| Noexpr
	| Id of string

type stmt =
	  Block of stmt list   
	| Expr of expr
	| If of expr * stmt * stmt
	| While of expr * stmt
	| For of expr * expr * expr
	| Return of expr

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
