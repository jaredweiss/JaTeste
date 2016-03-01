type op = Add | Sub

type typ = Int | Void
type bind = typ * string

type expr =
	  Lit of int
	| Binop of expr * op * expr
	| Assign of string * expr

type stmt =
	Expr of expr

type func_decl = {
	typ	:	typ;
	fname	:	string;
	body	: 	stmt list;
}

type program = func_decl