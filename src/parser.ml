type token =
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | SEMI
  | PLUS
  | MINUS
  | TIMES
  | DIVIDE
  | ASSIGN
  | FUNC
  | WTEST
  | USING
  | INT
  | VOID
  | LITERAL of (int)
  | ID of (string)
  | EOF

open Parsing;;
let _ = parse_error;;
# 1 "parser.mly"
 open Ast 
# 26 "parser.ml"
let yytransl_const = [|
  257 (* LPAREN *);
  258 (* RPAREN *);
  259 (* LBRACE *);
  260 (* RBRACE *);
  261 (* SEMI *);
  262 (* PLUS *);
  263 (* MINUS *);
  264 (* TIMES *);
  265 (* DIVIDE *);
  266 (* ASSIGN *);
  267 (* FUNC *);
  268 (* WTEST *);
  269 (* USING *);
  270 (* INT *);
  271 (* VOID *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  272 (* LITERAL *);
  273 (* ID *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\003\000\003\000\003\000\004\000\004\000\005\000\
\005\000\006\000\007\000\007\000\007\000\007\000\000\000"

let yylen = "\002\000\
\002\000\001\000\008\000\012\000\016\000\001\000\001\000\000\000\
\002\000\002\000\001\000\003\000\003\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\015\000\000\000\002\000\006\000\007\000\
\000\000\001\000\000\000\000\000\000\000\008\000\000\000\000\000\
\011\000\000\000\009\000\000\000\000\000\000\000\010\000\000\000\
\000\000\008\000\014\000\012\000\013\000\000\000\000\000\000\000\
\008\000\000\000\005\000"

let yydgoto = "\002\000\
\004\000\005\000\006\000\009\000\015\000\019\000\020\000"

let yysindex = "\006\000\
\007\255\000\000\005\255\000\000\021\000\000\000\000\000\000\000\
\008\255\000\000\023\255\024\255\025\255\000\000\252\254\015\255\
\000\000\019\255\000\000\004\255\027\255\006\255\000\000\006\255\
\006\255\000\000\000\000\000\000\000\000\254\254\018\255\029\255\
\000\000\000\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\033\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\034\000\000\000\
\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\000\000\000\000\231\255\000\000\237\255"

let yytablesize = 34
let yytable = "\016\000\
\030\000\031\000\027\000\035\000\028\000\029\000\001\000\034\000\
\023\000\024\000\025\000\017\000\018\000\017\000\018\000\017\000\
\018\000\003\000\007\000\008\000\010\000\017\000\018\000\012\000\
\011\000\013\000\021\000\014\000\022\000\026\000\032\000\033\000\
\003\000\004\000"

let yycheck = "\004\001\
\026\000\004\001\022\000\004\001\024\000\025\000\001\000\033\000\
\005\001\006\001\007\001\016\001\017\001\016\001\017\001\016\001\
\017\001\011\001\014\001\015\001\000\000\016\001\017\001\001\001\
\017\001\002\001\012\001\003\001\010\001\003\001\013\001\003\001\
\000\000\000\000"

let yynames_const = "\
  LPAREN\000\
  RPAREN\000\
  LBRACE\000\
  RBRACE\000\
  SEMI\000\
  PLUS\000\
  MINUS\000\
  TIMES\000\
  DIVIDE\000\
  ASSIGN\000\
  FUNC\000\
  WTEST\000\
  USING\000\
  INT\000\
  VOID\000\
  EOF\000\
  "

let yynames_block = "\
  LITERAL\000\
  ID\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    Obj.repr(
# 21 "parser.mly"
                   ( _1 )
# 132 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'fdecl) in
    Obj.repr(
# 24 "parser.mly"
           ( _1 )
# 139 "parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 6 : 'typ) in
    let _3 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 27 "parser.mly"
                                                     ({
		typ = _2; fname = _3; body = _7 })
# 149 "parser.ml"
               : 'fdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 10 : 'typ) in
    let _3 = (Parsing.peek_val __caml_parser_env 9 : string) in
    let _7 = (Parsing.peek_val __caml_parser_env 5 : 'stmt_list) in
    let _11 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 29 "parser.mly"
                                                                                  ({
		typ = _2; fname = _3; body = _7 })
# 160 "parser.ml"
               : 'fdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 14 : 'typ) in
    let _3 = (Parsing.peek_val __caml_parser_env 13 : string) in
    let _7 = (Parsing.peek_val __caml_parser_env 9 : 'stmt_list) in
    let _11 = (Parsing.peek_val __caml_parser_env 5 : 'stmt_list) in
    let _15 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 32 "parser.mly"
                                               ({
		typ = _2; fname = _3; body = _7 })
# 172 "parser.ml"
               : 'fdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 38 "parser.mly"
         ( Int )
# 178 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 39 "parser.mly"
          ( Void )
# 184 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 42 "parser.mly"
                 ( [] )
# 190 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 43 "parser.mly"
                  ( _2::_1)
# 198 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 46 "parser.mly"
             ( Expr _1 )
# 205 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 49 "parser.mly"
           ( Lit(_1))
# 212 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 50 "parser.mly"
                  ( Binop(_1, Add, _3) )
# 220 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 51 "parser.mly"
                   ( Binop(_1, Sub, _3) )
# 228 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 52 "parser.mly"
                  ( Assign(_1, _3) )
# 236 "parser.ml"
               : 'expr))
(* Entry program *)
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
let program (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.program)
