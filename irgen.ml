(* IR generation: translate takes a semantically checked AST and
   produces LLVM IR

   LLVM tutorial: Make sure to read the OCaml version of the tutorial

   http://llvm.org/docs/tutorial/index.html

   Detailed documentation on the OCaml LLVM library:

   http://llvm.moe/
   http://llvm.moe/ocaml/

*)

module L = Llvm
module A = Ast
open Sast

module StringMap = Map.Make(String)

(* translate : Sast.program -> Llvm.module *)
let translate functions =
  let context    = L.global_context () in

  (* Create the LLVM compilation module into which
     we will generate code *)
  let the_module = L.create_module context "SKAN" in

  (* Get types from the context *)
  let i32_t      = L.i32_type    context
  and i8_t       = L.i8_type     context
  and i1_t       = L.i1_type     context
  and void_t     = L.void_type   context in
  
  let str_t = L.pointer_type i8_t in

  (* Return the LLVM type for a MicroC type *)
  let struct_img_t : L.lltype = 
    L.named_struct_type context "Img" in

  let _ =
    L.struct_set_body struct_img_t
    [| str_t |] false in

  let ltype_of_typ = function
      A.Int    -> i32_t
    | A.Bool   -> i1_t
    | A.Img    -> struct_img_t
    | A.String -> str_t
    | A.Void   -> void_t
    | A.AnyType -> raise (Failure ("AnyType not allowed"))
  in

 (* Built-in functions *)

(* let read_image = L.function_type (L.pointer_type i32_t) [| L.pointer_type i8_t |] in
let read_image_func = L.declare_function "read_image" read_image the_module in

let write_image = L.function_type i32_t [| L.pointer_type i32_t; i32_t; i32_t; L.pointer_type i8_t |] in
let write_image_func = L.declare_function "write_image" write_image the_module in *)(* replace this with print big to make sure linking is correct*)

  let printf_t : L.lltype =
    L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_func : L.llvalue =
    L.declare_function "printf" printf_t the_module in

  let print_int : L.lltype =
    L.function_type i32_t
    [| i32_t|] in
    
  let print_int_func : L.llvalue =
    L.declare_function "print_int" print_int the_module in

  let print_str : L.lltype =
    L.function_type str_t
    [| str_t|] in
    
  let print_str_func : L.llvalue =
    L.declare_function "print_str" print_str the_module in

  (*let initImg_t : L.lltype =
    L.function_type (L.void_type context)
    [| L.pointer_type struct_img_t |] in

  let initImg : L.llvalue =
    L.declare_function "initImg" initImg_t the_module in*)
  
  let emptyInitImg_t : L.lltype =
    L.function_type (struct_img_t)
    [| |] in

  let emptyInitImg : L.llvalue =
    L.declare_function "emptyInitImg" emptyInitImg_t the_module in

  let load_t : L.lltype =
    L.function_type (struct_img_t)
    [| str_t |] in
  
  let load_func: L.llvalue =
    L.declare_function "load" load_t the_module in

  let save_t : L.lltype =
    L.function_type i32_t
    [| str_t; struct_img_t |] in

  let save_func : L.llvalue =
    L.declare_function "save" save_t the_module in

  let dilation_t : L.lltype = 
    L.function_type (struct_img_t)
    [| struct_img_t; i32_t; i32_t |] in
  
  let dilation_func : L.llvalue = 
    L.declare_function "dilation" dilation_t the_module in
  
  let sobel_t : L.lltype = 
    L.function_type (struct_img_t)
    [| struct_img_t|] in
    
  let sobel_func : L.llvalue = 
    L.declare_function "sobel" sobel_t the_module in
  
  let threshold_t : L.lltype = 
    L.function_type (struct_img_t)
    [| struct_img_t; i32_t |] in
      
  let threshold_func : L.llvalue = 
    L.declare_function "threshold" threshold_t the_module in
  
  let gaussian_t : L.lltype = 
    L.function_type (struct_img_t)
    [| struct_img_t; i32_t |] in
        
  let gaussian_func : L.llvalue = 
    L.declare_function "gaussian" gaussian_t the_module in
  
  let color_t : L.lltype = 
    L.function_type (struct_img_t)
    [| struct_img_t; i32_t |] in
          
  let color_func : L.llvalue = 
    L.declare_function "color" color_t the_module in

  let sharpen_t : L.lltype = 
    L.function_type (struct_img_t)
    [| struct_img_t; i32_t |] in
          
  let sharpen_func : L.llvalue = 
    L.declare_function "sharpen" sharpen_t the_module in

  let median_t : L.lltype = 
    L.function_type (struct_img_t)
    [| struct_img_t; i32_t |] in
          
  let median_func : L.llvalue = 
    L.declare_function "median" median_t the_module in
  
  let cleanup_t : L.lltype =
    L.function_type i32_t
    [| struct_img_t |] in
  
  let cleanup_func : L.llvalue =
    L.declare_function "cleanup" cleanup_t the_module in

  let display_func : L.llvalue =
    L.declare_function "display" cleanup_t the_module in
  
  let brighten_t : L.lltype =
    L.function_type (struct_img_t)
    [| struct_img_t; i32_t |] in
  
  let brighten_func : L.llvalue =
    L.declare_function "brighten" brighten_t the_module in

(*  SECTION 4b: Calling C's print function 
let print_func : L.llvalue =
  L.declare_function "printf" print_t the_module
  in let print_by_type ltyp e' builder mat_dim_map img_dim_map m_row m_col =
    (match ltyp with
        "i32" ->
          ((L.build_call print_func [| int_format_str builder ; e' |]
          "printf" builder), (mat_dim_map, img_dim_map))
      | "i1" -> (* prints 1 for true, 0 for false *)
          ((L.build_call print_func [| int_format_str builder ; e' |]
          "printf" builder), (mat_dim_map, img_dim_map))
      | "double" ->
          ((L.build_call print_func [| double_format_str builder ; e' |]
          "printf" builder), (mat_dim_map, img_dim_map))
      | "i8*" ->  (* string *)
          ((L.build_call print_func [| str_format_str builder ; e' |]
          "printf" builder), (mat_dim_map, img_dim_map))
      | "void" -> raise (Failure ("Can't print void type"))
      | _ ->  (* must be matrix *)
          ((L.build_call print_func [| (mat_format_str builder m_row m_col) ; e' |]
          "printf" builder), (mat_dim_map, img_dim_map)))
  in*)


  (* Define each function (arguments and return type) so we can
     call it even before we've created its body *)
  let function_decls : (L.llvalue * sfunc_def) StringMap.t =
    let function_decl m fdecl =
      let name = fdecl.sfname
      and formal_types =
        Array.of_list (List.map (fun (t,_) -> ltype_of_typ t) fdecl.sformals)
      in let ftype = L.function_type (ltype_of_typ fdecl.srtyp) formal_types in
      StringMap.add name (L.define_function name ftype the_module, fdecl) m in
    List.fold_left function_decl StringMap.empty functions in

  (* Fill in the body of the given function *)
  let build_function_body fdecl =
    let (the_function, _) = StringMap.find fdecl.sfname function_decls in
    let builder = L.builder_at_end context (L.entry_block the_function) in

    let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder in

    (* Construct the function's formal arguments.
       Allocate each on the stack, initialize their
       value, if appropriate, and remember their values in the "formals_map" map *)
    let add_formal m (t, n) p =
      L.set_value_name n p;

    let local = L.build_alloca (ltype_of_typ t) n builder in
      ignore (L.build_store p local builder);
      StringMap.add n local m in
    
    (* Construct formal variables, remember values in "formals_map" *)
    let formals_map = List.fold_left2 add_formal StringMap.empty fdecl.sformals
      (Array.to_list (L.params the_function)) (*in
      List.fold_left add_local formals fdecl.slocals*)
    in

    (* Return the value for a variable or formal argument.
       Check local names. *)
    let lookup n map = StringMap.find n map
    in

    (* Construct code for an expression; return its value *)
    let rec build_expr builder locals_map ((t, e) : sexpr) = match e with
        SLiteral i  -> L.const_int i32_t i
      | SBoolLit b  -> L.const_int i1_t (if b then 1 else 0)
      | SStringLit s -> L.build_global_stringptr s "str" builder
      | SId s       -> L.build_load (lookup s locals_map) s builder
      (*| SAssign (s, e) -> let e' = build_expr builder locals_map e in
        ignore(L.build_store e' (lookup s locals_map) builder); e'*)
      | SBinop (e1, op, e2) ->
        let e1' = build_expr builder locals_map e1
        and e2' = build_expr builder locals_map e2 in
        (match op with
           A.Add     -> L.build_add
         | A.Sub     -> L.build_sub
         | A.And     -> L.build_and
         | A.Or      -> L.build_or
         | A.Equal   -> L.build_icmp L.Icmp.Eq
         | A.Neq     -> L.build_icmp L.Icmp.Ne
         | A.Less    -> L.build_icmp L.Icmp.Slt
         | A.Mod     -> L.build_urem
         | A.Divide  -> L.build_udiv
         | A.Mult    -> L.build_mul
        ) e1' e2' "tmp" builder
      | SCall ("print", [e]) ->
        L.build_call printf_func [| int_format_str ; (build_expr builder locals_map e) |]
          "printf" builder
      | SCall ("print_int", [e]) ->
        L.build_call print_int_func [| (build_expr builder locals_map e) |]
          "print_int" builder
      | SCall ("print_str", [e]) ->
        L.build_call print_str_func [| (build_expr builder locals_map e) |]
          "print_str" builder
      | SCall ("load", [e]) ->
        L.build_call load_func [| (build_expr builder locals_map e) |]
          "load" builder
      | SCall ("save", [e1; e2]) ->
        L.build_call save_func [| (build_expr builder locals_map e1); (build_expr builder locals_map e2) |]
          "save" builder
      | SCall ("dilation", [e1; e2; e3]) ->
        L.build_call dilation_func [| (build_expr builder locals_map e1); (build_expr builder locals_map e2); (build_expr builder locals_map e3) |]
          "dilation" builder 
      | SCall ("sobel", [e]) ->
        L.build_call sobel_func [| (build_expr builder locals_map e) |]
          "sobel" builder 
      | SCall ("threshold", [e1; e2]) ->
        L.build_call threshold_func [| (build_expr builder locals_map e1); (build_expr builder locals_map e2) |]
          "threshold" builder
      | SCall ("gaussian", [e1; e2]) ->
        L.build_call gaussian_func [| (build_expr builder locals_map e1); (build_expr builder locals_map e2) |]
          "gaussian" builder 
      | SCall ("color", [e1; e2]) ->
        L.build_call color_func [| (build_expr builder locals_map e1); (build_expr builder locals_map e2) |]
          "color" builder
      | SCall ("sharpen", [e1; e2]) ->
        L.build_call sharpen_func [| (build_expr builder locals_map e1); (build_expr builder locals_map e2) |]
          "sharpen" builder
      | SCall ("median", [e1; e2]) ->
        L.build_call median_func [| (build_expr builder locals_map e1); (build_expr builder locals_map e2) |]
          "median" builder
      | SCall ("cleanup", [e]) ->
        L.build_call cleanup_func [| (build_expr builder locals_map e) |]
          "cleanup" builder
      | SCall ("display", [e]) ->
        L.build_call display_func [| (build_expr builder locals_map e) |]
          "display" builder
      | SCall (f, args) ->
        let (fdef, fdecl) = StringMap.find f function_decls in
        let llargs = List.rev (List.map (build_expr builder locals_map) (List.rev args)) in
        let result = f ^ "_result" in
        L.build_call fdef (Array.of_list llargs) result builder
      | SNoExpr ->
        (match t with
           Int -> L.const_int i32_t 0
         | Bool -> L.const_int i1_t 1
         | String -> L.build_global_stringptr "" "str" builder
         | Img ->  L.build_call emptyInitImg [| |] "" builder
        |  _ -> L.const_int i32_t 0)
      | SBrighten (s, e) ->
        let s' = L.build_load (lookup s locals_map) s builder in
        ignore(L.build_call brighten_func [| s'; (build_expr builder locals_map e) |]
          "brighten" builder); s'
    in

    (* LLVM insists each basic block end with exactly one "terminator"
       instruction that transfers control.  This function runs "instr builder"
       if the current block does not already have a terminator.  Used,
       e.g., to handle the "fall off the end of the function" case. *)
    let add_terminal builder instr =
      match L.block_terminator (L.insertion_block builder) with
        Some _ -> ()
      | None -> ignore (instr builder) in

    (* Build the code for the given statement; return the builder for
       the statement's successor (i.e., the next instruction will be built
       after the one generated by this call) *)
    (* Returns (builder, locals_map) tuple *)
    let rec build_stmt (builder, locals_map) = function
        SBlock sl -> List.fold_left build_stmt (builder, locals_map) sl
      | SExpr e -> ignore(build_expr builder locals_map e); (builder, locals_map)
      | SReturn e -> ignore(L.build_ret (build_expr builder locals_map e) builder); (builder, locals_map)
      | SIf (predicate, then_stmt, else_stmt) ->
        let bool_val = build_expr builder locals_map predicate in

        let then_bb = L.append_block context "then" the_function in
        ignore (build_stmt ((L.builder_at_end context then_bb), locals_map) then_stmt);
        let else_bb = L.append_block context "else" the_function in
        ignore (build_stmt ((L.builder_at_end context else_bb), locals_map) else_stmt);

        let end_bb = L.append_block context "if_end" the_function in
        let build_br_end = L.build_br end_bb in (* partial function *)
        add_terminal (L.builder_at_end context then_bb) build_br_end;
        add_terminal (L.builder_at_end context else_bb) build_br_end;

        ignore(L.build_cond_br bool_val then_bb else_bb builder);
        (L.builder_at_end context end_bb, locals_map)

      | SWhile (predicate, body) ->
        let while_bb = L.append_block context "while" the_function in
        let build_br_while = L.build_br while_bb in (* partial function *)
        ignore (build_br_while builder);
        let while_builder = L.builder_at_end context while_bb in
        let bool_val = build_expr while_builder locals_map predicate in

        let body_bb = L.append_block context "while_body" the_function in
        add_terminal (match (build_stmt ((L.builder_at_end context body_bb), locals_map) body) with (b,_)->b) build_br_while;

        let end_bb = L.append_block context "while_end" the_function in

        ignore(L.build_cond_br bool_val body_bb end_bb while_builder);
        (L.builder_at_end context end_bb, locals_map)
      
      | SLocal(typ, varname, e) ->
          let local_var = L.build_alloca (ltype_of_typ typ) varname builder in
          let _ = (match typ with
                    (*| A.Img -> L.build_call initImg [| local_var |] "" builder*)
                    | _ -> local_var) in
          let new_local_map = StringMap.add varname local_var locals_map in
          ignore (L.build_store (build_expr builder new_local_map e) local_var builder);
          (builder, new_local_map)
      | SAssign(typ, var, e) ->
        let e' = build_expr builder locals_map e in
        ignore(L.build_store e' (lookup var locals_map) builder);
        (builder, locals_map)
    in
    (* Build the code for each statement in the function *)
    let func_builder = build_stmt (builder, formals_map) (SBlock fdecl.sbody) in

    (* Add a return if the last block falls off the end *)
    add_terminal (match func_builder with (b,_)->b) (L.build_ret (L.const_int i32_t 0))
  in

  List.iter build_function_body functions;
  the_module