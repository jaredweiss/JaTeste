(* Program structure exceptions *)
exception MissingMainFunction

exception InvalidHeaderFile of string

(* Struct exceptions*)
exception InvalidStruct of string
exception InvalidStructField
exception InvalidStructMethodCall

(* Array exceptions*)
exception InvalidArrayVariable
exception InvalidArrayAccess
exception InvalidArrayType

(* Variable exceptions*)
exception UndeclaredVariable of string

(* Expression exceptions *)
exception InvalidExpr of string
exception InvalidBooleanExpression 
exception IllegalAssignment
exception InvalidFunctionCall of string
exception InvalidArgumentsToFunction of string
exception InvalidFree of string
exception InvalidPointerDereference
exception InvalidDereference
exception InvalidPointerAccess
exception NotBoolExpr
exception InvalidLhsOfExpr
exception InvalidNegativeType

(* Print exceptions *)
exception InvalidPrintCall
exception InvalidPrintFormat

(* Statement exceptions*)
exception InvalidReturnType of string

(* Bug catcher *)
exception BugCatch of string

(* Input *)
exception IllegalInputFormat
exception IllegalArgument of string

(* Test cases *)
exception InvalidTestAsserts
exception InvalidAssert of string
