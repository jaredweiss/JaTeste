(* Struct exceptions*)
exception InvalidStruct of string

(* Variable exceptions*)
exception UndeclaredVariable of string

(*Expression exceptions *)
exception InvalidExpr of string
exception InvalidBooleanExpression 

(* Statement exceptions*)
exception InvalidReturnType of string

exception BugCatch of string


