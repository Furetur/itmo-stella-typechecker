open Base
open Errors
module Errors = Errors

open Passes.Result_pass_syntax

let check (prog : Stella_parser.Parsetree.program) : (unit, error) Result.t =
  let* () = Check_main.check_main prog in
  let* () =  Check_types.check_program prog in
  return ()
