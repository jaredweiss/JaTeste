type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq | And | Or | Mod | Exp
type uop = Neg | Not | Addr
type prim = Int | Double | String | Char | Void | Bool
type typ = Primitive of prim | Struct_typ of string | Func_typ of string | Pointer_typ of typ | Array_typ of prim * int
type bind = typ * string

type expr =
    Lit     of int
  | String_lit of string
  | Binop   of expr * op * expr
  | Unop    of uop * expr
  | Assign  of expr * expr
  | Noexpr
  | Id of string
  | Struct_create of string
  | Struct_access of expr * expr
  | Pt_access of expr * expr 
  | Dereference of expr
  | Array_create of int * prim
  | Array_access of expr * int
  | Free of expr
  | Call of string * expr list
  | BoolLit of bool

type stmt =
    Block of stmt list   
  | Expr of expr
  | If of expr * stmt * stmt
  | While of expr * stmt
  | For of expr * expr * expr * stmt
  | Return of expr

type with_using_decl = {
  uvdecls : bind list;
  stmts : stmt list;
}

type with_test_decl = {
  exprs : expr list;
  using : with_using_decl; 
}

(* Node that describes a function *)
type func_decl = {
  typ : typ;
  fname : string;
  formals : bind list;
  vdecls  : bind list;
  body  :   stmt list;
  tests   :   with_test_decl option; 
}

(* Node that describes a given struct *)
type struct_decl = {
  sname   : string;
  attributes  : bind list;
}

(* Root of tree. Our program is made up three things 1) list of global variables 2) list of functions 3) list of struct definition *)
type program = bind list * func_decl list * struct_decl list
