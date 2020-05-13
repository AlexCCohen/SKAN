(* Semantically-checked Abstract Syntax Tree and functions for printing it *)

open Ast

type sexpr = typ * sx
and sx =
    SLiteral of int
  | SBoolLit of bool
  (*| SImgLit of string*)
  | SStringLit of string
  | SId of string
  | SBinop of sexpr * op * sexpr
  (*| SAssign of string * sexpr*)
  (* call *)
  | SCall of string * sexpr list
  | SNoExpr
  | SBrighten of string * sexpr

type sstmt =
    SBlock of sstmt list
  | SExpr of sexpr
  | SIf of sexpr * sstmt * sstmt
  | SWhile of sexpr * sstmt
  (* return *)
  | SReturn of sexpr
  | SLocal of typ * string * sexpr
  | SAssign of typ * string * sexpr

(* func_def: ret_typ fname formals locals body *)
type sfunc_def = {
  srtyp: typ;
  sfname: string;
  sformals: bind list;
  sbody: sstmt list;
}

type sprogram = sfunc_def list



(* Pretty-printing functions *)
let rec string_of_sexpr (t, e) =
  "(" ^ string_of_typ t ^ " : " ^ (match e with
        SLiteral(l) -> string_of_int l
      | SBoolLit(true) -> "true"
      | SBoolLit(false) -> "false"
      (*| SImgLit(l) -> "Image: " ^ l*)
      | SStringLit(l) -> "String: " ^ l
      | SId(s) -> s
      | SBinop(e1, o, e2) ->
        string_of_sexpr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_sexpr e2
      (*| SAssign(v, e) -> v ^ " = " ^ string_of_sexpr e*)
      | SCall(f, el) ->
          f ^ "(" ^ String.concat ", " (List.map string_of_sexpr el) ^ ")"
      | SNoExpr -> "NoExpr"
      | SBrighten(v, e) -> v ^ " = " ^ string_of_sexpr e
    ) ^ ")"


let rec string_of_sstmt = function
    SBlock(stmts) ->
    "{\n" ^ String.concat "" (List.map string_of_sstmt stmts) ^ "}\n"
  | SExpr(expr) -> string_of_sexpr expr ^ ";\n"
  | SReturn(expr) -> "return " ^ string_of_sexpr expr ^ ";\n"
  | SIf(e, s1, s2) ->  "if (" ^ string_of_sexpr e ^ ")\n" ^
                       string_of_sstmt s1 ^ "else\n" ^ string_of_sstmt s2
  | SWhile(e, s) -> "while (" ^ string_of_sexpr e ^ ") " ^ string_of_sstmt s
  | SLocal(t, id, v) -> string_of_typ t ^ " " ^ id ^ " = " ^ string_of_sexpr v ^ ";\n"
  | SAssign(t, v, e) -> "(" ^ string_of_typ t ^ " : " ^ v ^ " = " ^ string_of_sexpr e ^ ";\n"

let string_of_sfdecl fdecl =
  string_of_typ fdecl.srtyp ^ " " ^
  fdecl.sfname ^ "(" ^ String.concat ", " (List.map snd fdecl.sformals) ^
  ")\n{\n" ^
  String.concat "" (List.map string_of_sstmt fdecl.sbody) ^
  "}\n"

  
let string_of_sprogram funcs =
  "\n\nSementically checked program: \n\n" ^
  String.concat "\n" (List.map string_of_sfdecl funcs)  