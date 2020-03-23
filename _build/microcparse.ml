type token =
  | SEMI
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | PLUS
  | MINUS
  | ASSIGN
  | MOD
  | EQ
  | NEQ
  | LT
  | AND
  | OR
  | IF
  | ELSE
  | WHILE
  | INT
  | BOOL
  | RETURN
  | COMMA
  | LITERAL of (int)
  | BLIT of (bool)
  | ID of (string)
  | EOF

open Parsing;;
let _ = parse_error;;
# 4 "microcparse.mly"
open Ast
# 33 "microcparse.ml"
let yytransl_const = [|
  257 (* SEMI *);
  258 (* LPAREN *);
  259 (* RPAREN *);
  260 (* LBRACE *);
  261 (* RBRACE *);
  262 (* PLUS *);
  263 (* MINUS *);
  264 (* ASSIGN *);
  265 (* MOD *);
  266 (* EQ *);
  267 (* NEQ *);
  268 (* LT *);
  269 (* AND *);
  270 (* OR *);
  271 (* IF *);
  272 (* ELSE *);
  273 (* WHILE *);
  274 (* INT *);
  275 (* BOOL *);
  276 (* RETURN *);
  277 (* COMMA *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  278 (* LITERAL *);
  279 (* BLIT *);
  280 (* ID *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\005\000\005\000\003\000\006\000\
\006\000\004\000\007\000\007\000\009\000\009\000\008\000\008\000\
\010\000\010\000\010\000\010\000\010\000\011\000\011\000\011\000\
\011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
\011\000\011\000\011\000\012\000\012\000\013\000\013\000\000\000"

let yylen = "\002\000\
\002\000\000\000\003\000\002\000\000\000\003\000\002\000\001\000\
\001\000\008\000\000\000\001\000\001\000\003\000\000\000\002\000\
\002\000\003\000\007\000\005\000\003\000\001\000\001\000\001\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\004\000\000\000\001\000\001\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\008\000\009\000\040\000\000\000\000\000\000\000\
\000\000\001\000\000\000\000\000\004\000\007\000\003\000\000\000\
\000\000\012\000\000\000\000\000\014\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\022\000\023\000\
\000\000\000\000\000\000\000\000\006\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\010\000\016\000\017\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\034\000\
\018\000\000\000\000\000\021\000\000\000\000\000\037\000\000\000\
\025\000\026\000\027\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\035\000\000\000\020\000\039\000\000\000\
\019\000"

let yydgoto = "\002\000\
\005\000\006\000\007\000\008\000\024\000\009\000\017\000\034\000\
\018\000\035\000\036\000\062\000\063\000"

let yysindex = "\001\000\
\085\255\000\000\000\000\000\000\000\000\007\000\079\255\085\255\
\251\254\000\000\085\255\085\255\000\000\000\000\000\000\026\255\
\051\255\000\000\085\255\055\255\000\000\085\255\065\255\038\255\
\085\255\014\255\038\255\083\255\091\255\014\255\000\000\000\000\
\109\255\073\255\038\255\114\255\000\000\140\255\113\255\014\255\
\014\255\128\255\014\255\014\255\000\000\000\000\000\000\014\255\
\014\255\014\255\014\255\014\255\014\255\014\255\014\255\000\000\
\000\000\152\255\164\255\000\000\255\254\111\255\000\000\173\255\
\000\000\000\000\000\000\124\255\124\255\197\255\190\255\182\255\
\038\255\038\255\014\255\000\000\106\255\000\000\000\000\038\255\
\000\000"

let yyrindex = "\000\000\
\132\000\000\000\000\000\000\000\000\000\000\000\000\000\132\000\
\000\000\000\000\132\000\141\255\000\000\000\000\000\000\142\255\
\000\000\000\000\000\000\000\000\000\000\028\255\000\000\143\255\
\028\255\000\000\143\255\000\000\000\000\000\000\000\000\000\000\
\062\255\000\000\143\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\153\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\154\255\000\000\000\000\000\255\
\000\000\000\000\000\000\081\255\095\255\076\255\043\255\098\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000"

let yygindex = "\000\000\
\000\000\059\000\088\000\000\000\135\000\000\000\000\000\004\000\
\149\000\217\255\230\255\000\000\094\000"

let yytablesize = 206
let yytable = "\038\000\
\033\000\001\000\033\000\042\000\048\000\049\000\010\000\050\000\
\051\000\052\000\053\000\054\000\055\000\058\000\059\000\026\000\
\061\000\064\000\014\000\075\000\033\000\065\000\066\000\067\000\
\068\000\069\000\070\000\071\000\072\000\005\000\039\000\005\000\
\005\000\077\000\078\000\031\000\032\000\033\000\046\000\026\000\
\081\000\027\000\005\000\031\000\005\000\031\000\019\000\005\000\
\061\000\005\000\005\000\005\000\028\000\020\000\029\000\031\000\
\031\000\030\000\022\000\031\000\032\000\033\000\024\000\031\000\
\024\000\025\000\013\000\024\000\024\000\015\000\024\000\024\000\
\024\000\024\000\024\000\024\000\030\000\045\000\030\000\011\000\
\012\000\028\000\024\000\028\000\040\000\030\000\030\000\030\000\
\030\000\030\000\028\000\028\000\041\000\028\000\028\000\029\000\
\030\000\029\000\032\000\016\000\032\000\028\000\003\000\004\000\
\029\000\029\000\016\000\029\000\029\000\023\000\043\000\032\000\
\023\000\076\000\047\000\029\000\044\000\057\000\032\000\048\000\
\049\000\080\000\050\000\051\000\052\000\053\000\054\000\055\000\
\060\000\048\000\049\000\002\000\050\000\048\000\049\000\053\000\
\050\000\051\000\052\000\053\000\054\000\055\000\056\000\011\000\
\013\000\048\000\049\000\015\000\050\000\051\000\052\000\053\000\
\054\000\055\000\073\000\036\000\038\000\048\000\049\000\037\000\
\050\000\051\000\052\000\053\000\054\000\055\000\074\000\021\000\
\079\000\048\000\049\000\000\000\050\000\051\000\052\000\053\000\
\054\000\055\000\048\000\049\000\000\000\050\000\051\000\052\000\
\053\000\054\000\055\000\048\000\049\000\000\000\050\000\051\000\
\052\000\053\000\054\000\048\000\049\000\000\000\050\000\051\000\
\052\000\053\000\048\000\049\000\000\000\050\000"

let yycheck = "\026\000\
\001\001\001\000\003\001\030\000\006\001\007\001\000\000\009\001\
\010\001\011\001\012\001\013\001\014\001\040\000\041\000\002\001\
\043\000\044\000\024\001\021\001\021\001\048\000\049\000\050\000\
\051\000\052\000\053\000\054\000\055\000\002\001\027\000\004\001\
\005\001\073\000\074\000\022\001\023\001\024\001\035\000\002\001\
\080\000\004\001\015\001\001\001\017\001\003\001\021\001\020\001\
\075\000\022\001\023\001\024\001\015\001\003\001\017\001\013\001\
\014\001\020\001\004\001\022\001\023\001\024\001\001\001\021\001\
\003\001\001\001\008\000\006\001\007\001\011\000\009\001\010\001\
\011\001\012\001\013\001\014\001\001\001\005\001\003\001\001\001\
\002\001\001\001\021\001\003\001\002\001\010\001\011\001\012\001\
\013\001\014\001\010\001\011\001\002\001\013\001\014\001\001\001\
\021\001\003\001\001\001\012\000\003\001\021\001\018\001\019\001\
\010\001\011\001\019\000\013\001\014\001\022\000\002\001\014\001\
\025\000\003\001\001\001\021\001\008\001\005\001\021\001\006\001\
\007\001\016\001\009\001\010\001\011\001\012\001\013\001\014\001\
\001\001\006\001\007\001\000\000\009\001\006\001\007\001\012\001\
\009\001\010\001\011\001\012\001\013\001\014\001\003\001\003\001\
\003\001\006\001\007\001\005\001\009\001\010\001\011\001\012\001\
\013\001\014\001\003\001\003\001\003\001\006\001\007\001\025\000\
\009\001\010\001\011\001\012\001\013\001\014\001\003\001\019\000\
\075\000\006\001\007\001\255\255\009\001\010\001\011\001\012\001\
\013\001\014\001\006\001\007\001\255\255\009\001\010\001\011\001\
\012\001\013\001\014\001\006\001\007\001\255\255\009\001\010\001\
\011\001\012\001\013\001\006\001\007\001\255\255\009\001\010\001\
\011\001\012\001\006\001\007\001\255\255\009\001"

let yynames_const = "\
  SEMI\000\
  LPAREN\000\
  RPAREN\000\
  LBRACE\000\
  RBRACE\000\
  PLUS\000\
  MINUS\000\
  ASSIGN\000\
  MOD\000\
  EQ\000\
  NEQ\000\
  LT\000\
  AND\000\
  OR\000\
  IF\000\
  ELSE\000\
  WHILE\000\
  INT\000\
  BOOL\000\
  RETURN\000\
  COMMA\000\
  EOF\000\
  "

let yynames_block = "\
  LITERAL\000\
  BLIT\000\
  ID\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    Obj.repr(
# 31 "microcparse.mly"
            ( _1)
# 221 "microcparse.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    Obj.repr(
# 34 "microcparse.mly"
                 ( ([], [])               )
# 227 "microcparse.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'decls) in
    Obj.repr(
# 35 "microcparse.mly"
                    ( ((_1 :: fst _3), snd _3) )
# 235 "microcparse.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'fdecl) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'decls) in
    Obj.repr(
# 36 "microcparse.mly"
               ( (fst _2, (_1 :: snd _2)) )
# 243 "microcparse.ml"
               : 'decls))
; (fun __caml_parser_env ->
    Obj.repr(
# 39 "microcparse.mly"
              ( [] )
# 249 "microcparse.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl_list) in
    Obj.repr(
# 40 "microcparse.mly"
                           (  _1 :: _3 )
# 257 "microcparse.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 44 "microcparse.mly"
         ( (_1, _2) )
# 265 "microcparse.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 47 "microcparse.mly"
          ( Int   )
# 271 "microcparse.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 48 "microcparse.mly"
          ( Bool  )
# 277 "microcparse.ml"
               : 'typ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 7 : 'vdecl) in
    let _3 = (Parsing.peek_val __caml_parser_env 5 : 'formals_opt) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl_list) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 53 "microcparse.mly"
  (
    {
      rtyp=fst _1;
      fname=snd _1;
      formals=_3;
      locals=_6;
      body=_7
    }
  )
# 295 "microcparse.ml"
               : 'fdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 65 "microcparse.mly"
              ( [] )
# 301 "microcparse.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formals_list) in
    Obj.repr(
# 66 "microcparse.mly"
                 ( _1 )
# 308 "microcparse.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 69 "microcparse.mly"
        ( [_1] )
# 315 "microcparse.ml"
               : 'formals_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'formals_list) in
    Obj.repr(
# 70 "microcparse.mly"
                             ( _1::_3 )
# 323 "microcparse.ml"
               : 'formals_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 73 "microcparse.mly"
                ( [] )
# 329 "microcparse.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt_list) in
    Obj.repr(
# 74 "microcparse.mly"
                    ( _1::_2 )
# 337 "microcparse.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 77 "microcparse.mly"
                                            ( Expr _1      )
# 344 "microcparse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 78 "microcparse.mly"
                                            ( Block _2 )
# 351 "microcparse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'stmt) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 81 "microcparse.mly"
                                            ( If(_3, _5, _7) )
# 360 "microcparse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 82 "microcparse.mly"
                                            ( While (_3, _5)  )
# 368 "microcparse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 84 "microcparse.mly"
                                            ( Return _2      )
# 375 "microcparse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 87 "microcparse.mly"
                     ( Literal(_1)            )
# 382 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 88 "microcparse.mly"
                     ( BoolLit(_1)            )
# 389 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 89 "microcparse.mly"
                     ( Id(_1)                 )
# 396 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 90 "microcparse.mly"
                     ( Binop(_1, Add,   _3)   )
# 404 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 91 "microcparse.mly"
                     ( Binop(_1, Sub,   _3)   )
# 412 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 92 "microcparse.mly"
                     ( Binop (_1, Mod, _3)    )
# 420 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 93 "microcparse.mly"
                     ( Binop(_1, Equal, _3)   )
# 428 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 94 "microcparse.mly"
                     ( Binop(_1, Neq, _3)     )
# 436 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 95 "microcparse.mly"
                     ( Binop(_1, Less,  _3)   )
# 444 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 96 "microcparse.mly"
                     ( Binop(_1, And,   _3)   )
# 452 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 97 "microcparse.mly"
                     ( Binop(_1, Or,    _3)   )
# 460 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 98 "microcparse.mly"
                     ( Assign(_1, _3)         )
# 468 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 99 "microcparse.mly"
                       ( _2                   )
# 475 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'args_opt) in
    Obj.repr(
# 101 "microcparse.mly"
                              ( Call (_1, _3)  )
# 483 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 105 "microcparse.mly"
              ( [] )
# 489 "microcparse.ml"
               : 'args_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'args) in
    Obj.repr(
# 106 "microcparse.mly"
         ( _1 )
# 496 "microcparse.ml"
               : 'args_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 109 "microcparse.mly"
        ( [_1] )
# 503 "microcparse.ml"
               : 'args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'args) in
    Obj.repr(
# 110 "microcparse.mly"
                    ( _1::_3 )
# 511 "microcparse.ml"
               : 'args))
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
