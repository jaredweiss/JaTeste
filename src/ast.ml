type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq | And | Or | Mod | Exp
type uop = Neg | Not | Addr
type prim = Int | Double | String | Char | Void | Bool
type typ = Primitive of prim | Struct_typ of string | Func_typ of string | Pointer_typ of typ | Array_typ of prim * int | Any 
type bind = typ * string

type dir_location = Curr | Standard

(* include files node *)
type header = dir_location * string

(* Jateste expressions *)
type expr =
    Lit     of int
  | String_lit of string
  | Char_lit of char
  | Double_lit of float
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
  | Null of typ
  | Dubs 

(* Jateste statements *)
type stmt =
    Block of stmt list   
  | Expr of expr
  | If of expr * stmt * stmt
  | While of expr * stmt
  | For of expr * expr * expr * stmt
  | Return of expr
  | Assert of expr

(* Node that describes the envoirnment for with_test_decl node *)
type with_using_decl = {
  uvdecls : bind list;
  stmts : stmt list;
}

(* Node the describes test cases *)
type with_test_decl = {
  asserts : stmt list;
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
  struc_method : bool;
  includes_func : bool;
}

(* Node that describes a given struct *)
type struct_decl = {
  sname   : string;
  attributes  : bind list;
  methods  : func_decl list;
}

(* Root of tree. Our program is made up four things 1) list of header/include files 2) list of global variables 3) list of function definitions 4) list of struct definitions *)
type program = header list * bind list * func_decl list * struct_decl list
