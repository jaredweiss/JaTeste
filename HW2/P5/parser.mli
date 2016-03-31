type token =
  | IF
  | ELSE
  | NULL

val s :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> int
