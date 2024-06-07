open Base
open Printf
open Stella_parser.Parsetree
open Utils

type error_kind =
  | Error_unexpected_subtype of { expected : typeT; actual : typeT }
  | Error_missing_main
  | Error_undefined_variable of stellaIdent
  | Error_unexpected_type_for_expression of { expected : typeT; actual : typeT }
  | Error_incorrect_number_of_arguments
  | Error_not_a_function
  | Error_unexpected_lambda of { expected : typeT }
  | Error_unexpected_number_of_parameters_in_lambda of { expected : int }
  | Error_unexpected_type_for_parameter of { expected : typeT; actual : typeT }
  | Error_unknown
  | Error_incorrect_arity_of_main
  | Error_unexpected_type_for_expressions_fix
  (* Tuples *)
  | Error_not_a_tuple of typeT
  | Error_unexpected_tuple of { expected_type : typeT }
  | Error_tuple_index_out_of_bounds of { max_index : int; actual_index : int }
  | Error_unexpected_tuple_length of { expected : int }
  (* Records *)
  | Error_not_a_record of typeT
  | Error_unexpected_field_access of { typeT : typeT; field_name : stellaIdent }
  | Error_unexpected_record of typeT
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
  (* Sum types *)
  | Error_unexpected_injection of { expected : typeT }
  | Error_ambiguous_sum_type
  (* Pattern matching *)
  | Error_illegal_empty_matching
  | Error_nonexhaustive_match_patterns
  | Error_unexpected_pattern_for_type
  (* Errors *)
  | Error_ambiguous_panic_type
  | Error_ambiguous_throw_type
  | Error_exception_type_not_declared
  (* References *)
  | Error_not_a_reference of typeT
  | Error_unexpected_reference of typeT
  | Error_ambiguous_reference_type
  | Error_unexpected_memory_address of typeT
  (* Type reconstruction *)
  | Error_occurs_check_infinite_type of { type' : typeT; type_value : typeT }
  (* Universal types *)
  | Error_not_a_generic_function of typeT
  | Error_incorrect_number_of_type_arguments of {
      type' : typeT;
      expected : int;
      actual : int;
    }
  | Error_undefined_type_variable of stellaIdent
  (* Variants *)
  | Error_ambiguous_variant
  | Error_unexpected_variant
  | Error_unexpected_variant_label of typeT * stellaIdent

type error = { kind : error_kind; stacktrace : string list }
type 'a pass_result = ('a, error) Result.t

let whole_file_error kind = { kind; stacktrace = [] }

let show_kind = function
  | Error_unexpected_subtype { expected; actual } ->
      sprintf "ERROR_UNEXPECTED_SUBTYPE: '%s' is not a subtype of '%s'"
        (pp_type actual) (pp_type expected)
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
  | Error_ambiguous_list -> "ERROR_AMBIGUOUS_LIST_TYPE"
  | Error_not_a_list t ->
      sprintf "ERROR_NOT_A_LIST: Expected a list but got %s" (pp_type t)
  | Error_unexpected_injection { expected } ->
      sprintf
        "ERROR_UNEXPECTED_INJECTION: Expected a value of type %s but got \
         injection"
        (pp_type expected)
  | Error_ambiguous_sum_type -> "ERROR_AMBIGUOUS_SUM_TYPE"
  | Error_illegal_empty_matching -> "ERROR_ILLEGAL_EMPTY_MATCHING"
  | Error_nonexhaustive_match_patterns -> "ERROR_NONEXHAUSTIVE_MATCH_PATTERNS"
  | Error_unexpected_pattern_for_type -> "ERROR_UNEXPECTED_PATTERN_FOR_TYPE"
  | Error_unexpected_lambda { expected } ->
      sprintf "ERROR_UNEXPECTED_LAMBDA: Expected %s but got a lambda"
        (pp_type expected)
  | Error_unknown -> "ERROR_UNKNOWN"
  | Error_unexpected_type_for_parameter { actual; expected } ->
      sprintf "ERROR_UNEXPECTED_TYPE_FOR_PARAMETER: Expected %s but got %s"
        (pp_type expected) (pp_type actual)
  | Error_incorrect_arity_of_main -> "ERROR_INCORRECT_ARITY_OF_MAIN"
  | Error_unexpected_number_of_parameters_in_lambda { expected } ->
      sprintf
        "ERROR_UNEXPECTED_NUMBER_OF_PARAMETERS_IN_LAMBDA: Expected %d \
         parameters"
        expected
  | Error_unexpected_record t ->
      sprintf
        "ERROR_UNEXPECTED_RECORD: Expected a value of type %s but got a record"
        (pp_type t)
  | Error_unexpected_type_for_expressions_fix ->
      "ERROR_UNEXPECTED_TYPE_FOR_EXPRESSION: Fixpoint combinator expects \
       function of type fn(T) -> T"
  | Error_ambiguous_panic_type -> "ERROR_AMBIGUOUS_PANIC_TYPE"
  | Error_not_a_reference t ->
      sprintf "ERROR_NOT_A_REFERENCE: Expected a reference but got %s"
        (pp_type t)
  | Error_ambiguous_reference_type -> "ERROR_AMBIGUOUS_REFERENCE_TYPE"
  | Error_unexpected_memory_address t ->
      sprintf
        "ERROR_UNEXPECTED_MEMORY_ADDRESS: Expected %s but got a memory address"
        (pp_type t)
  | Error_unexpected_reference t ->
      sprintf "ERROR_UNEXPECTED_REFERENCE: Expected %s but got a reference"
        (pp_type t)
  | Error_exception_type_not_declared -> "ERROR_EXCEPTION_TYPE_NOT_DECLARED"
  | Error_ambiguous_throw_type -> "ERROR_AMBIGUOUS_THROW_TYPE"
  | Error_occurs_check_infinite_type { type'; type_value } ->
      Printf.sprintf "ERROR_OCCURS_CHECK_INFINITE_TYPE: %s = %s" (pp_type type')
        (pp_type type_value)
  | Error_not_a_generic_function t ->
      Printf.sprintf "ERROR_NOT_A_GENERIC_FUNCTION: %s" (pp_type t)
  | Error_incorrect_number_of_type_arguments { type'; expected; actual } ->
      Printf.sprintf
        "ERROR_INCORRECT_NUMBER_OF_TYPE_ARGUMENTS: Type '%s' expects %d type \
         arguments, but got %d"
        (pp_type type') expected actual
  | Error_undefined_type_variable (StellaIdent name) ->
      Printf.sprintf "ERROR_UNDEFINED_TYPE_VARIABLE: %s" name
  | Error_ambiguous_variant -> "ERROR_AMBIGUOUS_VARIANT_TYPE"
  | Error_unexpected_variant -> "ERROR_UNEXPECTED_VARIANT"
  | Error_unexpected_variant_label (t, StellaIdent lab) ->
      Printf.sprintf
        "ERROR_UNEXPECTED_VARIANT_LABEL: Variant type %s does not have a label \
         %s"
        (pp_type t) lab

let show_stacktrace stacktrace =
  stacktrace
  |> List.map ~f:format_inline_code
  |> List.mapi ~f:(fun i frame -> Printf.sprintf "%d. %s" (i + 1) frame)
  |> String.concat_lines

let show { kind; stacktrace } =
  let trace = show_stacktrace stacktrace in
  sprintf "%s\n\n=== Stacktrace ===\n%s" (show_kind kind) trace
