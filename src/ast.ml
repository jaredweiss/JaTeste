type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq | And | Or
type uop = Neg | Not
type prim = Int | Double | String | Char | Void
type typ = Primitive of prim | Struct_typ of string | Func_typ of string | Pointer_typ of typ | Array_typ of prim
type bind = typ * string

type expr =
	  Lit 		of int
	| Binop 	of expr * op * expr
	| Unop 		of uop * expr
	| Assign 	of expr * expr
	| Noexpr
	| Id of string
	| Struct_Access of expr * expr
	| Array_create of int * prim

type stmt =
	  Block of stmt list   
	| Expr of expr
	| If of expr * stmt * stmt
	| While of expr * stmt
	| For of expr * expr * expr * stmt
	| Return of expr

type with_using_decl = {
	stmts : stmt list;
}

type with_test_decl = {
	exprs : expr list;
	using : with_using_decl;
}



(* Node that describes a function *)
type func_decl = {
	typ	:	typ;
	fname	:	string;
	formals :	bind list;
	vdecls	:	bind list;
	body	: 	stmt list;
	tests   : 	with_test_decl;
}



(* Node that describes a given struct *)
type struct_decl = {
	sname		:	string;
	attributes	:	bind list;
}

(* Program is made up of a list of these *)
type flow = 
	| Var 		of bind
	| Struct 	of struct_decl
	| Func 		of func_decl
	| Stmt 		of stmt

(* Root of tree *)
type program = flow list

let string_of_op = function
    Add -> "+"
  | Sub -> "-"
  | Mult -> "*"
  | Div -> "/"
  | Equal -> "=="
  | Neq -> "!="
  | Less -> "<"
  | Leq -> "<="
  | Greater -> ">"
  | Geq -> ">="
  | And -> "&&"
  | Or -> "||"

let string_of_uop = function
    Neg -> "-"
  | Not -> "!"

let rec string_of_expr = function
    Lit(l) -> string_of_int l
  | Id(s) -> s
  | Binop(e1, o, e2) ->
      string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Unop(o, e) -> string_of_uop o ^ string_of_expr e
  | Assign(v, e) -> string_of_expr v ^ " = " ^ string_of_expr e
  | Noexpr -> ""

let rec string_of_stmt = function
    Block(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n";
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n";
  | If(e, s, Block([])) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | For(e1, e2, e3, s) ->
      "for (" ^ string_of_expr e1  ^ " ; " ^ string_of_expr e2 ^ " ; " ^
      string_of_expr e3  ^ ") " ^ string_of_stmt s
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s

let string_of_typ = function
    Primitive(s) -> "prim"
  | Struct_typ(s) -> "struct"
  | Func_typ(s) -> "func"
  | Pointer_typ(s) -> "ptr"
  | Array_typ(s) -> "array"

let string_of_vdecl (t, id) = string_of_typ t ^ " " ^ id ^ ";\n"

let string_of_fdecl fdecl =
  string_of_typ fdecl.typ ^ " " ^
  fdecl.fname ^ "(" ^ String.concat ", " (List.map snd fdecl.formals) ^
  ")\n{\n" ^
  String.concat "" (List.map string_of_vdecl fdecl.formals) ^
  String.concat "" (List.map string_of_stmt fdecl.body) ^
  "}\n"

let string_of_program (vars, funcs) =
  String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_fdecl funcs)
