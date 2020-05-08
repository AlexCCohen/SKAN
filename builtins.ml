open Ast

let built_ins = [{ rtyp = Int; fname = "print"; formals = [(Int, "x")]; body = [] };
{ rtyp = Int; fname= "print_int"; formals = [(Int, "x")]; body = [] };
{ rtyp = String; fname= "print_str"; formals = [(String, "x")]; body = [] };
{ rtyp = Img; fname= "load"; formals = [(String, "x")]; body = [] };
(* return 0 if successful, should be void but placeholder till make void type *)
{ rtyp = Int; fname= "save"; formals = [(String, "name"); (Img, "x")]; body = [] };
{ rtyp = Int; fname= "cleanup"; formals = [(Img, "x")]; body = [] }; ]