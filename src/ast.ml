type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq | And | Or | Mod | Exp
type uop = Neg | Not | Addr
type prim = Int | Double | String | Char | Void
type typ = Primitive of prim | Struct_typ of string | Func_typ of string | Pointer_typ of typ | Array_typ of prim
type bind = typ * string

type expr =
    Lit     of int
  | Binop   of expr * op * expr
  | Unop    of uop * expr
  | Assign  of expr * expr
  | Noexpr
  | Id of string
  | Struct_create of string
  | Struct_Access of expr * expr
  | Array_create of int * prim
  | Array_access of expr * int
  | Call of string * expr list

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
  tests   :   with_test_decl;
}

(* Node that describes a given struct *)
type struct_decl = {
  sname   : string;
  attributes  : bind list;
}

(* Program is made up of a list of these *)
type flow = 
  | Var     of bind
  | Struct  of struct_decl 
  | Func    of func_decl 

(* Root of tree *)
type program = bind list * func_decl list * struct_decl list
