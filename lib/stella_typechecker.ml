open Base
open Errors
module Errors = Errors
open Passes.Result_pass_syntax
open Stella_parser

let check (prog : Stella_parser.Parsetree.program) : (unit, error) Result.t =
  Logs.debug (fun m -> m "AST: %s" (show_program prog));
  let* () = Check_main.check_main prog in
  let prog = Replace_auto.replace_auto prog in
  Logs.debug (fun m -> m "REPLACED AUTO: %s" (show_program prog));
  let* () = Check_types_with_reconstruction.check_program prog in
  return ()
