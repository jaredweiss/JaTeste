type token =
  | IF
  | ELSE
  | NULL

open Parsing;;
let _ = parse_error;;
let yytransl_const = [|
  257 (* IF *);
  258 (* ELSE *);
  259 (* NULL *);
    0|]

let yytransl_block = [|
    0|]

let yylhs = "\255\255\
\001\000\001\000\002\000\002\000\000\000"

let yylen = "\002\000\
\003\000\001\000\000\000\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\002\000\005\000\000\000\000\000\001\000\
\004\000"

let yydgoto = "\002\000\
\005\000\008\000"

let yysindex = "\001\000\
\000\255\000\000\000\255\000\000\000\000\003\255\000\255\000\000\
\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\006\000\000\000\000\000\
\000\000"

let yygindex = "\000\000\
\253\255\000\000"

let yytablesize = 6
let yytable = "\006\000\
\003\000\001\000\004\000\009\000\007\000\003\000"

let yycheck = "\003\000\
\001\001\001\000\003\001\007\000\002\001\000\000"

let yynames_const = "\
  IF\000\
  ELSE\000\
  NULL\000\
  "

let yynames_block = "\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : int) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 't) in
    Obj.repr(
# 8 "parser.mly"
          ( 0 )
# 66 "parser.ml"
               : int))
; (fun __caml_parser_env ->
    Obj.repr(
# 9 "parser.mly"
        ( 0 )
# 72 "parser.ml"
               : int))
; (fun __caml_parser_env ->
    Obj.repr(
# 12 "parser.mly"
               ( 0 )
# 78 "parser.ml"
               : 't))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 13 "parser.mly"
           ( 0 )
# 85 "parser.ml"
               : 't))
(* Entry s *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let s (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : int)
