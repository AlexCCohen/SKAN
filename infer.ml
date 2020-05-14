open Ast
open Builtins

module StringMap = Map.Make(String)

(* First semantic pass. Takes in and returns an AST if successful,
   replacing AnyType's, and throws an exception if error. *)

let resolve functions =

  let built_in_decls =
    let add_bind map func_def = StringMap.add func_def.fname func_def map
    in List.fold_left add_bind StringMap.empty built_ins
  in

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

  let find_func s =
    let function_decls = List.fold_left add_func built_in_decls functions in
    try StringMap.find s function_decls
    with Not_found -> raise (Failure ("unrecognized function " ^ s))
  in

  (* Function to add new variable to map *)
  let add_var map fname var typ =
    let func_map =
        try (StringMap.find fname map) with Not_found -> StringMap.empty in
    let updated_func_map = StringMap.add var typ func_map in
    StringMap.add fname updated_func_map map
  in

  (* Return type of expr *)
  let rec get_expr_typ f_map v_map = function
      Literal l -> Int
    | BoolLit l -> Bool
    | StringLit l -> String
    | Id var -> (try (StringMap.find var v_map)
        with Not_found -> raise (Failure ("variable " ^ var ^ " not defined")))
    | Binop(e1, op, e2) as e ->
      let t1 = get_expr_typ f_map v_map e1
      and t2 = get_expr_typ f_map v_map e2 in
      let err = "inf: illegal binary operator " ^
                string_of_typ t1 ^ " " ^ string_of_op op ^ " " ^
                string_of_typ t2 ^ " in " ^ string_of_expr e
      in
      (* All binary operators require operands of the same type*)
      if t1 = t2 then
        (* Determine expression type based on operator and operand types *)
        let t = match op with
            Add | Sub | Mod | Divide | Mult when t1 = Int -> Int
          | Equal | Neq -> Bool
          | Less when t1 = Int -> Bool
          | And | Or when t1 = Bool -> Bool
          | _ when t1 = AnyType || t2 = AnyType -> AnyType
          | _ -> raise (Failure err)
        in
        t
      else raise (Failure err)
    | Call(fname, args) ->
      (find_func fname).rtyp
    | NoExpr -> Void
    | Brighten (_,_) -> Img
  in

  (* Check expr *)
  (* Returns (func_table, var_table) *)
  let rec check_expr f_map v_map = function
    | Binop(e1, op, e2) ->
        let (f1, v1) = check_expr f_map v_map e1 in
        check_expr f1 v1 e2
    | Call(fname, args) ->
        let rec get_f_map f_map v_map n l =
          match l with
          | hd :: tl ->
              let arg_typ = get_expr_typ f_map v_map hd in
              let (f1, v1) = check_expr f_map v_map hd in
              let new_f = add_var f1 fname n arg_typ in
              get_f_map new_f v1 (string_of_int (int_of_string n+1)) tl
          | _ -> (f_map, v_map)
        in
        get_f_map f_map v_map "0" args
    | _ -> (f_map, v_map)
  in

  (* Check stmt list *)
  (* Returns (func_table, var_table) *)
  let rec check_stmt_list f_map v_map = function
      [] -> (f_map, v_map)
    | Block sl :: sl' -> check_stmt_list f_map v_map (sl @ sl')
    | s :: sl ->
        let (new_f_map, new_v_map) = check_stmt f_map v_map s in
        check_stmt_list new_f_map new_v_map sl
  
  (* Check stmt *)
  (* Returns (f_map, v_map) *)
  and check_stmt f_map v_map = function
      Block sl -> check_stmt_list f_map v_map sl 
    | Expr e -> check_expr f_map v_map e
    | If(e, st1, st2) ->
        let (f2, v2) = check_expr f_map v_map e in
        let (f3, v3) = check_stmt f2 v2 st1 in
        check_stmt f3 v3 st2
    | While(e, st) ->
        let (new_f_map, new_v_map) = check_expr f_map v_map e in
        check_stmt new_f_map new_v_map st
    | Return e ->
        check_expr f_map v_map e
    | Local (typ, varname, e) ->
        let (f_new, v_new) = check_expr f_map v_map e in
        let v_out = StringMap.add varname typ v_new in
        (f_new, v_out)    
    | Infer (varname, e) ->
      (* Assignment, don't care *)
      if StringMap.mem varname v_map then
        check_expr f_map v_map e
      (* Inferred initialization *)
      else
        let (new_f, new_v) = check_expr f_map v_map e in
        let typ = get_expr_typ new_f new_v e in
        let v_out = StringMap.add varname typ new_v in
        (new_f, v_out)
  in
  (* Add formals into symbol table, if AT then function unused and skip it *)
  let formal_table map func =
    let find (l, n) formal =
      let (t, v) = match formal with (x,y)->(x,y) in
      let next_num = string_of_int (int_of_string n + 1) in
        try (l @ [Some (StringMap.find n map, v)], next_num)
        with Not_found -> (l @ [None], next_num)
    in
  (* Get a list of the types of the formals, in order *)
    let typl = match (List.fold_left find ([], "0") func.formals) with
    | (None :: tl, _) -> None (* None means unused or not AT *)
    | (l, _) ->
      Some (List.map (fun x -> match x with Some (t, v) -> (t,v) | _ -> raise (Failure "ERROR")) l)
    in
  match typl with
  | None -> None
  | Some l ->
    let update_tbl tbl entry =
      let (t,v) = match entry with (x,y)->(x,y) in
      StringMap.add v t tbl in
    Some (List.fold_left update_tbl map l)
  in

  (* func_tbl: fn -> (var, typ) list, where var = 1, 2, ...*)
  (* get_vars returns updated func_tbl *)
  let get_vars func_table func =
    (* Create variable symbol table starting with formals *)
    let tbl = try(StringMap.find func.fname func_table) with Not_found -> StringMap.empty in
    let var_table = formal_table tbl func in
    match var_table with
    | None -> func_table
    | Some x -> match (check_stmt_list func_table x func.body) with (f, _)->f
  in

  (* Create map: function_name -> {map: var -> { type } } *)
  let symbol_table = List.fold_left get_vars StringMap.empty functions
  in

  let check_func func =
    let map = try (StringMap.find func.fname symbol_table) with Not_found -> StringMap.empty
    in
    let update_formals (l, n) formal =
      let (t, v) = match formal with (x,y)->(x,y) in
      if t != AnyType then (l @ [(t,v)], n)
      else
        let new_formal = try(StringMap.find n map)
        with Not_found -> t (* Undefined/unused *)
        in
        let next_num = string_of_int ((int_of_string n) + 1) in
        (l @ [(new_formal, v)], next_num)
    in
    let new_formals formals =
      match (List.fold_left update_formals ([], "0") formals)
      with (f, _) -> f
    in
  { rtyp = func.rtyp;
  fname = func.fname;
  formals = new_formals func.formals;
  body = func.body
  }
  in

  (* Go through functions and make new formals, then reverse *)
  (List.rev (List.map check_func functions))