open Base
open Errors
module Errors = Errors
open Passes.Result_pass_syntax
open Stella_parser

let type_reconstruction = "#type-reconstruction"

let get_type_checker prog =
  let open Extentions in
  let exts = get_extentions prog in
  if is_extention_enabled exts type_reconstruction then (
    Logs.debug (fun m ->
        m
          "Detected #type-reconstruction extentiont -- using the appropriate \
           typechecker");
    Check_types_with_reconstruction.check_program)
  else (
    Logs.debug (fun m ->
        m "No extra extentions detected -- using basic typechecker");

    Check_types.check_program)

let check (prog : Stella_parser.Parsetree.program) : (unit, error) Result.t =
  Logs.debug (fun m -> m "AST: %s" (show_program prog));
  let* () = Check_main.check_main prog in
  let prog = Replace_auto.replace_auto prog in
  Logs.debug (fun m -> m "REPLACED AUTO: %s" (show_program prog));
  let typechecker = get_type_checker prog in
  let* () = typechecker prog in
  return ()
