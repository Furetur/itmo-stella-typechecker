open Base
open Printf
open Stella_parser.Parsetree
open Utils

type error_kind =
  | Error_missing_main
  | Error_undefined_variable of stellaIdent
  | Error_unexpected_type_for_expression of { expected : typeT; actual : typeT }
  | Error_incorrect_number_of_arguments
  | Error_not_a_function
  (* Tuples *)
  | Error_not_a_tuple of typeT
  | Error_unexpected_tuple of { expected_type : typeT }
  | Error_tuple_index_out_of_bounds of { max_index : int; actual_index : int }
  | Error_unexpected_tuple_length of { expected : int }
  (* Records *)
  | Error_not_a_record of typeT
  | Error_unexpected_field_access of { typeT : typeT; field_name : stellaIdent }
  | Error_unexpected_record_fields of {
      record_type : typeT;
      field_name : stellaIdent;
    }
  | Error_missing_record_fields of {
      record_type : typeT;
      field_name : stellaIdent;
    }
  (* Lists *)
  | Error_unexpected_list of { expected : typeT }
  | Error_ambiguous_list
  | Error_not_a_list of typeT

type error = { kind : error_kind; stacktrace : string list }
type 'a pass_result = ('a, error) Result.t

let whole_file_error kind = { kind; stacktrace = [] }

let show_kind = function
  | Error_missing_main -> "ERROR_MISSING_MAIN"
  | Error_undefined_variable (StellaIdent name) ->
      sprintf "ERROR_UNDEFINED_VARIABLE: '%s'" name
  | Error_unexpected_type_for_expression { expected; actual } ->
      sprintf "ERROR_UNEXPECTED_TYPE_FOR_EXPRESSION: Expected '%s' but got '%s'"
        (pp_type expected) (pp_type actual)
  | Error_not_a_function -> "ERROR_NOT_A_FUNCTION"
  | Error_incorrect_number_of_arguments -> "ERROR_INCORRECT_NUMBER_OF_ARGUMENTS"
  | Error_not_a_tuple t ->
      sprintf "ERROR_NOT_A_TUPLE: Expected a tuple but got %s" (pp_type t)
  | Error_not_a_record t ->
      sprintf "ERROR_NOT_A_RECORD: Expected a record but got %s" (pp_type t)
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
        (pp_type expected_type)
  | Error_unexpected_field_access { typeT; field_name = StellaIdent name } ->
      sprintf
        "ERROR_UNEXPECTED_FIELD_ACCESS: Field with name '%s' does not exist on \
         type %s"
        name (pp_type typeT)
  | Error_unexpected_record_fields
      { record_type; field_name = StellaIdent name } ->
      sprintf
        "ERROR_UNEXPECTED_RECORD_FIELDS: Field with name '%s' does not exist \
         on type %s"
        name (pp_type record_type)
  | Error_missing_record_fields { record_type; field_name = StellaIdent name }
    ->
      sprintf
        "ERROR_MISSING_RECORD_FIELDS: Field '%s' is missing from a record of \
         type %s"
        name (pp_type record_type)
  | Error_unexpected_list { expected } ->
      sprintf
        "ERROR_UNEXPECTED_LIST: Expected a value of type %s but got a list"
        (pp_type expected)
  | Error_ambiguous_list -> "ERROR_AMBIGUOUS_LIST"
  | Error_not_a_list t ->
      sprintf "ERROR_NOT_A_LIST: Expected a list but got %s" (pp_type t)

let show { kind; stacktrace } =
  let trace =
    stacktrace
    |> List.map ~f:format_inline_code
    |> List.mapi ~f:(fun i frame -> Printf.sprintf "%d. %s" (i + 1) frame)
    |> String.concat_lines
  in
  sprintf "%s\n\n=== Stacktrace ===\n%s" (show_kind kind) trace
