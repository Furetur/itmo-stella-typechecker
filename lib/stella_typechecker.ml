open Base
open Errors
module Errors = Errors

let check (prog : Stella_parser.Parsetree.program) : (unit, error) Result.t =
  Check_main.check_main prog
