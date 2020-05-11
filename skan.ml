(* Top-level of the SKAN compiler: scan & parse the input,
   check the resulting AST and generate an SAST from it, generate LLVM IR,
   and dump the module *)

open Printf
open Sys

type action = Ast | Sast | LLVM_IR | Exec

let () =
  let action = ref Exec in
  let set_action a () = action := a in
  let speclist = [
    ("-a", Arg.Unit (set_action Ast), "Print the AST");
    ("-s", Arg.Unit (set_action Sast), "Print the SAST");
    ("-l", Arg.Unit (set_action LLVM_IR), "Print the generated LLVM IR");
  ] in
  let usage_msg = "usage: ./skan.native [-a|-s|-l] [file.sk]" in
  let channel = ref stdin in
  Arg.parse speclist (fun filename -> channel := open_in filename) usage_msg;

  let lexbuf = Lexing.from_channel !channel in

  let ast = Skanparse.program Scanner.token lexbuf in
  let rast = Infer.resolve ast in
  match !action with
    Ast -> print_string (Ast.string_of_program ast)
  | _ -> let sast = Semant.check rast in
    match !action with
      Ast     -> ()
    | Sast    -> print_string (Sast.string_of_sprogram sast)
    | LLVM_IR -> print_string (Llvm.string_of_llmodule (Irgen.translate sast))
    | Exec    -> let llvm_module = Llvm.string_of_llmodule (Irgen.translate sast) in
        let out = open_out "llvm.out" in
          fprintf out "%s\n" llvm_module; close_out out;
          ignore(command "/usr/local/opt/llvm/bin/llc -relocation-model=pic llvm.out");
          ignore(command "g++ -g -Wall `pkg-config --cflags --libs opencv` -std=c++11 llvm.out.s utils.cpp -o a.out"); ()