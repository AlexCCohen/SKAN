/* Ocamlyacc parser for MicroC */

%{
open Ast
%}

%token SEMI LPAREN RPAREN LBRACE RBRACE PLUS MINUS ASSIGN MOD
%token EQ NEQ LT AND OR
%token IF ELSE WHILE INT BOOL IMG STRING VOID

/* %token IMG */

/* return, COMMA token */
%token RETURN COMMA
%token <int> LITERAL
%token <bool> BLIT
%token <string> ID
%token <string> STR_LITERAL
%token EOF

%start program
%type <Ast.program> program

%right ASSIGN
%left OR
%left AND
%left EQ NEQ
%left LT
%left PLUS MINUS MOD

%%

/* add function declarations*/
program:
  decls EOF { $1}

decls:
    /* nothing */ { []      }
  | decls fdecl   {$2 :: $1 }

typ:
    INT    { Int    }
  | BOOL   { Bool   }
  | IMG    { Img    }
  | STRING { String }
  | VOID   { Void   }

/* fdecl */
fdecl:
  typ ID LPAREN formals_opt RPAREN LBRACE stmt_list RBRACE
  {
    {
      rtyp = $1;
      fname = $2;
      formals=$4;
      body=$7
    }
  }

/* formals_opt */
formals_opt:
    /*nothing*/  { [] }
  | formals_list { $1 }

formals_list:
    typ ID                    { [($1, $2)]     }
  | typ ID COMMA formals_list { ($1, $2) :: $4 }

stmt_list:
    /* nothing */   { []     }
  | stmt stmt_list  { $1::$2 }

stmt:                            
  | typ ID SEMI                             { Local($1, $2,  NoExpr) }
  | typ ID ASSIGN expr SEMI                 { Local($1, $2, $4)      } 
  | expr SEMI                               { Expr $1                }
  | RETURN expr SEMI                        { Return $2              }
  | LBRACE stmt_list RBRACE                 { Block $2               }
  /* if (condition) { block1} else {block2} */
  /* if (condition) stmt else stmt */
  | IF LPAREN expr RPAREN stmt ELSE stmt    { If($3, $5, $7)         }
  | WHILE LPAREN expr RPAREN stmt           { While($3, $5)          }

expr:
    LITERAL            { Literal($1)            }
  | BLIT               { BoolLit($1)            }
  | ID                 { Id($1)                 }
  | STR_LITERAL        { StringLit($1)          }
  | expr PLUS   expr   { Binop($1, Add,   $3)   }
  | expr MINUS  expr   { Binop($1, Sub,   $3)   }
  | expr MOD    expr   { Binop ($1, Mod, $3)    }
  | expr EQ     expr   { Binop($1, Equal, $3)   }
  | expr NEQ    expr   { Binop($1, Neq, $3)     }
  | expr LT     expr   { Binop($1, Less,  $3)   }
  | expr AND    expr   { Binop($1, And,   $3)   }
  | expr OR     expr   { Binop($1, Or,    $3)   }
  | ID ASSIGN expr     { Assign($1, $3)         }
  | LPAREN expr RPAREN { $2                     }
  /* call */
  | ID LPAREN args_opt RPAREN { Call ($1, $3)  }

/* args_opt*/
args_opt:
  /*nothing*/ { [] }
  | args      { $1 }

args:
    expr            { [$1]   }
  | expr COMMA args { $1::$3 }
