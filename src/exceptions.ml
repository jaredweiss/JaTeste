(* Program structure exceptions *)
exception MissingMainFunction

(* Struct exceptions*)
exception InvalidStruct of string

(* Variable exceptions*)
exception UndeclaredVariable of string

(*Expression exceptions *)
exception InvalidExpr of string
exception InvalidBooleanExpression 
exception IllegalAssignment
exception InvalidFunctionCall of string
exception InvalidArgumentsToFunction of string
exception InvalidArrayVariable
exception InvalidStructField
exception InvalidFree of string
exception InvalidPointerDereference

(* Statement exceptions*)
exception InvalidReturnType of string
exception InvalidLhsOfExpr

(* Bug catcher *)
exception BugCatch of string

(* Input *)
exception IllegalInputFormat
exception IllegalArgument of string
