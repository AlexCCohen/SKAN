(*TODO:
  Imglit and data it holds *)
(* Abstract Syntax Tree and functions for printing it *)

type op =
    Add
  | Sub
  | Equal
  | Neq
  | Less
  | And
  | Or
  | Mod

(* New Type Img *)
type typ = 
    Int
  | Bool
  | String
  | Img
  | Void

type expr =
    Literal of int
  | BoolLit of bool
  (* Img *)
  (*| ImgLit of string*)
  (* String *)
  | StringLit of string
  | Id of string
  | Binop of expr * op * expr
  (*| Assign of string * expr*)
  (* function call *)
  | Call of string * expr list
  | NoExpr

type stmt =
    Block of stmt list
  | Expr of expr
  | If of expr * stmt * stmt
  | While of expr * stmt
  (* return *)
  | Return of expr
  | Local of typ * string * expr
  | Infer of string * expr

(* int x: name binding *)
type bind = typ * string

(* func_def: ret_typ fname formals locals body *)
type func_def = {
  rtyp: typ;
  fname: string;
  formals: bind list;
  body: stmt list;
}

type program = func_def list

(* Pretty-printing functions *)
let string_of_op = function
    Add -> "+"
  | Sub -> "-"
  | Equal -> "=="
  | Neq -> "!="
  | Less -> "<"
  | And -> "&&"
  | Or -> "||"
  | Mod -> "mod"


let string_of_typ = function
    Int -> "int"
  | Bool -> "bool"
  | Img -> "img"
  | String -> "string"
  | Void -> "void"


let rec string_of_expr = function
    Literal(l) -> string_of_int l
  | BoolLit(true) -> "true"
  | BoolLit(false) -> "false"
  (*| ImgLit(l) -> "Image: " ^ l*)
  | StringLit(l) -> "String: " ^ l
  | Id(s) -> s
  | Binop(e1, o, e2) ->
    string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  (*| Assign(v, e) -> v ^ " = " ^ string_of_expr e*)
  | Call(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | NoExpr -> "NoExpr"


let rec string_of_stmt = function
    Block(stmts) ->
    "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n"
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n"
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
                      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s
  | Local(t, s, e) ->
    if e = NoExpr then
    (* Noassign case *)
      string_of_typ t ^ " " ^ s ^ ";\n"
    (* Local assign case *)
    else string_of_typ t ^ " " ^ s ^ " = " ^ string_of_expr e ^ ";\n"
  | Infer(s, e) -> s ^ " = " ^ string_of_expr e ^ ";\n"


let string_of_fdecl fdecl =
  string_of_typ fdecl.rtyp ^ " " ^
  fdecl.fname ^ "(" ^ String.concat ", " (List.map snd fdecl.formals) ^
  ")\n{\n" ^
  String.concat "" (List.map string_of_stmt fdecl.body) ^
  "}\n"


let string_of_program funcs =
  "\n\nParsed program: \n\n" ^
  String.concat "\n" (List.map string_of_fdecl funcs)
