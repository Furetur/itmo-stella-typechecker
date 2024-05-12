open Base
open Stella_parser.Parsetree
open Utils
open Errors

type state = { stacktrace : string list; typemap : Type_map.t }

module ThisPass = Passes.SingleError (struct
  type pass_state = state
  type pass_error = error
end)

open ThisPass

(* --- Errors --- *)

let in_error_context context f =
  let* old_state = get in
  set { old_state with stacktrace = context :: old_state.stacktrace }
  *>
  let* result = f in
  set old_state *> return result

let in_expr_error_context context expected_type f =
  let context = format_inline_code context in
  let type' = pp_expected_type expected_type in
  let context = Printf.sprintf "%s <--- Expected: %s" context type' in
  in_error_context context f

let error kind =
  let* { stacktrace; _ } = get in
  fail { kind; stacktrace }

let zip_or_error error_kind list1 list2 =
  match List.zip list1 list2 with
  | Unequal_lengths -> error error_kind
  | Ok zipped -> return zipped

(* --- Typemap --- *)

let get_var_type ident =
  let* s = get in
  match Type_map.get_type s.typemap ident with
  | Some t -> return t
  | None -> error (Error_undefined_variable ident)

let set_typemap new_typemap =
  let* s = get in
  set { s with typemap = new_typemap }

let with_updated_typemap pass update_typemap =
  let* s = get in
  let new_typemap = update_typemap s.typemap in
  set_typemap new_typemap
  *> let* result = pass in
     set_typemap s.typemap *> return result

let with_new_binding ident type' pass =
  with_updated_typemap pass @@ fun typemap ->
  Type_map.set_type typemap ident type'

let with_new_bindings ident_type_pairs pass =
  with_updated_typemap pass @@ fun typemap ->
  Type_map.set_types typemap ident_type_pairs

let with_param_bindings param_decls pass =
  with_updated_typemap pass @@ fun typemap ->
  Type_map.add_params typemap param_decls

(* --- Type checkers --- *)

let check_equal_types expected_type actual_type =
  return Stdlib.(expected_type = actual_type)

let expect_equal_type expected_type actual_type =
  match expected_type with
  | None -> return actual_type
  | Some expected_type ->
      let* ok = check_equal_types expected_type actual_type in
      if ok then return actual_type
      else
        error
          (Error_unexpected_type_for_expression
             { expected = expected_type; actual = actual_type })

let expect_a_function t err =
  match t with
  | TypeFun (arg_types, ret_type) -> return (arg_types, ret_type)
  | t ->
      let error_kind = err t in
      error error_kind

let expect_a_tuple_with_element t index =
  match t with
  | TypeTuple component_types -> (
      match List.nth component_types (index - 1) with
      | None ->
          error
            (Error_tuple_index_out_of_bounds
               { max_index = List.length component_types; actual_index = index })
      | Some component_type -> return component_type)
  | _ -> error (Error_not_a_tuple t)

let expect_a_tuple_of_length t length err =
  match t with
  | TypeTuple component_types ->
      let wrong_length_error =
        Error_unexpected_tuple_length { expected = length }
      in
      if List.length component_types <> length then error wrong_length_error
      else return component_types
  | expected_type -> error (err expected_type)

let expect_a_record_with_field t field_ident =
  match t with
  | TypeRecord field_types -> (
      let fields_typemap = Type_map.of_record_fields field_types in
      match Type_map.get_type fields_typemap field_ident with
      | None ->
          error
            (Error_unexpected_field_access
               { typeT = t; field_name = field_ident })
      | Some result_type -> return result_type)
  | _ -> error (Error_not_a_record t)

let expect_a_record_with_fields t _ =
  match t with
  | TypeRecord t_field_types -> return t_field_types
  | t -> error (Error_unexpected_record t)

let expect_a_list t err =
  match t with
  | TypeList element_type -> return element_type
  | _ -> error err

let expect_a_sum t err =
  match t with
  | TypeSum (l, r) -> return (l, r)
  | _ -> error err




