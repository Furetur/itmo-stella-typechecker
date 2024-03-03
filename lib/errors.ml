open Base
open Printf
open Stella_parser
open Stella_parser.Parsetree

type error_kind =
  | Error_missing_main
  | Error_undefined_variable of stellaIdent
  | Error_unexpected_type_for_expression of { expected : typeT; actual : typeT }
  | Error_not_a_function
  | Error_incorrect_number_of_arguments

type error = { kind : error_kind; stacktrace : string list }
type 'a pass_result = ('a, error) Result.t

let whole_file_error kind = { kind; stacktrace = [] }

let show_kind = function
  | Error_missing_main -> "ERROR_MISSING_MAIN"
  | Error_undefined_variable (StellaIdent name) ->
      sprintf "ERROR_UNDEFINED_VARIABLE: '%s'" name
  | Error_unexpected_type_for_expression { expected; actual } ->
      sprintf "ERROR_UNEXPECTED_TYPE_FOR_EXPRESSION: Expected '%s' but got '%s'"
        (pretty_print_type expected)
        (pretty_print_type actual)
  | Error_not_a_function -> "ERROR_NOT_A_FUNCTION"
  | Error_incorrect_number_of_arguments -> "ERROR_INCORRECT_NUMBER_OF_ARGUMENTS"

let show { kind; stacktrace } =
  let trace =
    stacktrace
    |> List.mapi ~f:(fun i frame ->
           Printf.sprintf "%s. %s" (Int.to_string (i + 1)) frame)
    |> String.concat_lines
  in
  sprintf "%s:\n\n=== Stacktrace ===\n%s" (show_kind kind) trace
