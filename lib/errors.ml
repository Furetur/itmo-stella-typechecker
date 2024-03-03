open Base
open Printf
open Stella_parser
open Stella_parser.Parsetree

type error_kind =
  | Error_missing_main
  | Error_undefined_variable of stellaIdent
  | Error_unexpected_type_for_expression of { expected : typeT; actual : typeT }
  | Error_incorrect_number_of_arguments
  | Error_not_a_function
  | Error_not_a_record
  | Error_not_a_tuple
  | Error_unexpected_tuple of { expected_type : typeT }
  | Error_tuple_index_out_of_bounds of { max_index : int; actual_index : int }
  | Error_unexpected_tuple_length of { expected : int }

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
  | Error_not_a_tuple -> "ERROR_NOT_A_TUPLE"
  | Error_not_a_record -> "ERROR_NOT_A_RECORD"
  | Error_tuple_index_out_of_bounds { max_index; actual_index } ->
      sprintf
        "ERROR_TUPLE_INDEX_OUT_OF_BOUNDS: Tuple index must be in range 1..%d, \
         but got %d"
        max_index actual_index
  | Error_unexpected_tuple_length { expected } ->
      sprintf
        "ERROR_UNEXPECTED_TUPLE_LENGTH: Expected the tuple to have length %d"
        expected
  | Error_unexpected_tuple { expected_type } ->
      sprintf
        "ERROR_UNEXPECTED_TUPLE: Expected a value of type '%s' but got a tuple"
        (pretty_print_type expected_type)

let show { kind; stacktrace } =
  let trace =
    stacktrace
    |> List.mapi ~f:(fun i frame -> Printf.sprintf "%d. %s" (i + 1) frame)
    |> String.concat_lines
  in
  sprintf "%s\n\n=== Stacktrace ===\n%s" (show_kind kind) trace
