(* TODO:
  Built-in functions (load) *)
  (* expand "built_in_decls" for img types a d string*)
(* Semantic checking for the MicroC compiler *)

open Ast
open Sast
open Builtins

module StringMap = Map.Make(String)

(* Semantic checking of the AST. Returns an SAST if successful,
   throws an exception if something is wrong.

   Check each function *)

let check functions =

  (* Verify a list of bindings has no duplicate names *)
  let check_binds (kind : string) (binds : (typ * string) list) =
    let rec dups = function
        [] -> ()
      |	((_,n1) :: (_,n2) :: _) when n1 = n2 ->
        raise (Failure ("duplicate " ^ kind ^ " " ^ n1))
      | _ :: t -> dups t
    in dups (List.sort (fun (_,a) (_,b) -> compare a b) binds)
  in

  (* Collect function declarations for built-in functions: no bodies *)

  let built_in_decls =
    let add_bind map func_def = StringMap.add func_def.fname func_def map
    in List.fold_left add_bind StringMap.empty built_ins
  in

  (* Add function name to symbol table *)
  let add_func map fd =
    let built_in_err = "function " ^ fd.fname ^ " may not be defined"
    and dup_err = "duplicate function " ^ fd.fname
    and make_err er = raise (Failure er)
    and n = fd.fname (* Name of the function *)
    in match fd with (* No duplicate functions or redefinitions of built-ins *)
      _ when StringMap.mem n built_in_decls -> make_err built_in_err
    | _ when StringMap.mem n map -> make_err dup_err
    | _ ->  StringMap.add n fd map
  in

  (* Collect all function names into one symbol table *)
  let function_decls = List.fold_left add_func built_in_decls functions
  in

  (* Return a function from our symbol table *)
  let find_func s =
    try StringMap.find s function_decls
    with Not_found -> raise (Failure ("unrecognized function " ^ s))
  in

  let _ = find_func "main" in (* Ensure "main" is defined *)

  let check_func func =
    (* Make sure no formals or locals are void or duplicates *)
    check_binds "formal" func.formals;

    (* Raise an exception if the given rvalue type cannot be assigned to
       the given lvalue type *)
    let check_assign lvaluet rvaluet err =
      (*if lvaluet = rvaluet then lvaluet else raise (Failure err)*)
      match (lvaluet, rvaluet) with
        (Int, Int)-> lvaluet
      | (Bool, Bool) -> lvaluet
      | (String, String) -> lvaluet
      | (Img, Img) -> lvaluet
      | (_, Void) -> lvaluet
      | (AnyType, x) | (x, AnyType) -> x
      | _ -> raise (Failure err)
    in

    (* Build local symbol table of variables for this function *)
    let formals = List.fold_left (fun m (ty, name) -> if ty != AnyType then StringMap.add name ty m else raise (Failure "Unused function"))
        StringMap.empty func.formals
    in

    (* Return a variable from our local symbol table *)
    let type_of_identifier s symbols =
      try StringMap.find s symbols
      with Not_found -> raise (Failure ("undeclared identifier " ^ s))
    in

    (* Return a semantically-checked expression, i.e., with a type *)
    let rec check_expr symbols = function
        Literal l -> (Int, SLiteral l)
      | BoolLit l -> (Bool, SBoolLit l)
      | StringLit l -> (String, SStringLit l)
      | Id var -> (type_of_identifier var symbols, SId var)
      (*| Assign(var, e) as ex ->
        let lt = type_of_identifier var symbols
        and (rt, e') = check_expr symbols e in
        let err = "illegal assignment " ^ string_of_typ lt ^ " = " ^
                  string_of_typ rt ^ " in " ^ string_of_expr ex
        in
        (check_assign lt rt err, SAssign(var, (rt, e')))*)

      | Binop(e1, op, e2) as e ->
        let (t1, e1') = check_expr symbols e1
        and (t2, e2') = check_expr symbols e2 in
        let err = "illegal binary operator " ^
                  string_of_typ t1 ^ " " ^ string_of_op op ^ " " ^
                  string_of_typ t2 ^ " in " ^ string_of_expr e
        in
        (* All binary operators require operands of the same type*)
        if t1 = t2 then
          (* Determine expression type based on operator and operand types *)
          let t = match op with
              Add | Sub | Mod when t1 = Int -> Int
            | Equal | Neq -> Bool
            | Less when t1 = Int -> Bool
            | And | Or when t1 = Bool -> Bool
            | _ -> raise (Failure err)
          in
          (t, SBinop((t1, e1'), op, (t2, e2')))
        else raise (Failure err)
      | Call(fname, args) as call ->
        let fd = find_func fname in
        let param_length = List.length fd.formals in
        if List.length args != param_length then
          raise (Failure ("expecting " ^ string_of_int param_length ^
                          " arguments in " ^ string_of_expr call))
        else let check_call (ft, _) e =
               let (et, e') = check_expr symbols e in
               let err = "illegal argument found " ^ string_of_typ et ^
                         " expected " ^ string_of_typ ft ^ " in " ^ string_of_expr e
               in (check_assign ft et err, e')
          in
          let args' = List.map2 check_call fd.formals args
          in (fd.rtyp, SCall(fname, args'))
      | NoExpr -> (Void, SNoExpr)
     in  

    let check_bool_expr symbols e =
      let (t, e') = check_expr symbols e in
      match t with
      | Bool -> (t, e')
      |  _ -> raise (Failure ("expected Boolean expression in " ^ string_of_expr e))
    in

    let rec check_stmt_list symbols = function
        [] -> []
      | Block sl :: sl'  -> check_stmt_list symbols (sl @ sl') (* Flatten blocks *)
      | s :: sl ->
          let (checked_stmts, new_symbols) = check_stmt symbols s in
            checked_stmts :: check_stmt_list new_symbols sl

    (* Return a semantically-checked statement i.e. containing sexprs *)
    and check_stmt symbols = function
      (* A block is correct if each statement is correct and nothing
         follows any Return statement.  Nested blocks are flattened. *)
        Block sl -> (SBlock (check_stmt_list symbols sl), symbols ) 
      | Expr e -> (SExpr (check_expr symbols e), symbols)
      | If(e, st1, st2) ->
        (SIf(check_bool_expr symbols e, (match check_stmt symbols st1 with (x,_)->x), (match check_stmt symbols st2 with (y,_)->y)), symbols)
      | While(e, st) ->
        (SWhile(check_bool_expr symbols e, match check_stmt symbols st with (x,_)->x), symbols)
      | Return e ->
        let (t, e') = check_expr symbols e in
        if t = func.rtyp then (SReturn (t, e'), symbols)
        else raise (
            Failure ("return gives " ^ string_of_typ t ^ " expected " ^
                     string_of_typ func.rtyp ^ " in " ^ string_of_expr e))
      | Local (typ, varname, e) ->
        if StringMap.mem varname symbols then
          raise (Failure ("Local var " ^ varname ^ " already declared"))
        else
          let (t, e') = check_expr symbols e in
          let new_table = StringMap.add varname typ symbols in
          if typ = t || t = Void then
            match e with
              NoExpr ->
                (* Initialize variables *)
                let init_noexpr vartype =
                  (match vartype with
                    Int -> (Int, SLiteral 0)
                  | Bool -> (Bool, SBoolLit true)
                  | String -> (String, SStringLit "")
                  | Img -> (Img, SNoExpr) (***** FIX *****)
                  | Void -> (Void, SNoExpr)
                  | AnyType -> (AnyType, SNoExpr)) in
                  (SLocal (typ, varname, init_noexpr typ), new_table)
            | _ -> (SLocal (typ, varname, check_expr symbols e), new_table)
          else raise (Failure ("Variable type " ^ string_of_typ typ ^ " does not match expression type " ^ string_of_typ t))
        (* Need to check typ = type of e, then add new var to symbols table returned *)    
      | Infer (varname, e) ->
        if StringMap.mem varname symbols then
          (let lt = type_of_identifier varname symbols
            and (rt, e') = check_expr symbols e in
            let err = "illegal assignment " ^ string_of_typ lt ^ " = " ^
                      string_of_typ rt ^ " in " ^ string_of_expr e
            in
            (SAssign (check_assign lt rt err, varname, (rt, e')), symbols))
        else
         let (t,_) = check_expr symbols e in
         check_stmt symbols (Local(t, varname, e))
  in
  (* body of check_func *)
  { srtyp = func.rtyp;
    sfname = func.fname;
    sformals = func.formals;
    sbody = check_stmt_list formals func.body
  }
  in
  (List.map check_func functions)