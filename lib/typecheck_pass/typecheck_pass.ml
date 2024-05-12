open Base
open Stella_parser.Parsetree
open Utils
open Errors
open Has_type_vars

type constraint' = {
  expected : typeT;
  actual : typeT;
  stacktrace : string list;
}

let show_constraint { expected; actual; _ } =
  Printf.sprintf "%s = %s" (pp_type expected) (pp_type actual)

type state = {
  stacktrace : string list;
  typemap : Type_map.t;
  next_typevar_id : int;
  constraints : constraint' list;
}

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
  let* result = f () in
  let* s = get in
  set { s with stacktrace = old_state.stacktrace } *> return result

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
  *> let* result = pass () in
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

(* --- Type constraints --- *)

let typevar_prefix = "__weak"

let new_typevar =
  let* s = get in
  let id = s.next_typevar_id in
  set { s with next_typevar_id = id + 1 }
  *>
  let typevar = Utils.make_typevar typevar_prefix id in
  return typevar

let new_typevars n =
  let stub = List.range 0 n in
  let pass _ = new_typevar in
  many stub ~f:pass

let add_constraint expected actual =
  let* s = get in
  let stacktrace = s.stacktrace in
  let c = { expected; actual; stacktrace } in
  let c_index = List.length s.constraints in
  Logs.debug (fun m ->
      m "add_constraint: %d: %s, from:\n%s" c_index (show_constraint c)
        (Errors.show_stacktrace stacktrace));
  set { s with constraints = c :: s.constraints }
  *> let* s = get in
     Logs.debug (fun m ->
         m "Now there are %d constraints!" (List.length s.constraints));
     return ()

let add_function_constraint t n_args =
  let* args_t = new_typevars n_args in
  let* ret_t = new_typevar in
  let func_t = TypeFun (args_t, ret_t) in
  add_constraint func_t t *> return (args_t, ret_t, func_t)

let add_tuple_constraint t length =
  let* els_t = new_typevars length in
  let tuple_t = TypeTuple els_t in
  add_constraint tuple_t t *> return (els_t, t)

let add_list_constraint t =
  let* el_t = new_typevar in
  let list_t = TypeList el_t in
  add_constraint list_t t *> return (el_t, list_t)

let add_sum_constraint t =
  let* l_t = new_typevar in
  let* r_t = new_typevar in
  let sum_t = TypeSum (l_t, r_t) in
  add_constraint sum_t t *> return (l_t, r_t, sum_t)

(* --- Type checkers --- *)

let check_equal_types expected_type actual_type =
  let have_type_vars =
    check_has_typevars expected_type || check_has_typevars actual_type
  in
  if not have_type_vars then return Stdlib.(expected_type = actual_type)
  else add_constraint expected_type actual_type *> return true

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

let expect_a_function t n_args not_a_func_err invalid_number_of_args_err =
  match t with
  | TypeFun (arg_types, ret_type) ->
      if List.length arg_types <> n_args then error invalid_number_of_args_err
      else return (arg_types, ret_type)
  | TypeVar _ as t ->
      let* arg_types, ret_type, _ = add_function_constraint t n_args in
      return (arg_types, ret_type)
  | _ -> error not_a_func_err

let expect_a_tuple_with_element t index =
  let unpack_element component_types =
    match List.nth component_types (index - 1) with
    | None ->
        error
          (Error_tuple_index_out_of_bounds
             { max_index = List.length component_types; actual_index = index })
    | Some component_type -> return component_type
  in
  match t with
  | TypeTuple component_types -> unpack_element component_types
  | TypeVar _ as t ->
      let* component_types, _ = add_tuple_constraint t index in
      unpack_element component_types
  | _ -> error (Error_not_a_tuple t)

let expect_a_tuple_of_length t length err =
  match t with
  | TypeTuple component_types ->
      let wrong_length_error =
        Error_unexpected_tuple_length { expected = length }
      in
      if List.length component_types <> length then error wrong_length_error
      else return component_types
  | TypeVar _ as t ->
      let* component_types, _ = add_tuple_constraint t length in
      return component_types
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
  | TypeVar _ -> Utils.not_implemented ()
  | _ -> error (Error_not_a_record t)

let expect_a_record_with_fields t _ =
  match t with
  | TypeRecord t_field_types -> return t_field_types
  | TypeVar _ -> Utils.not_implemented ()
  | t -> error (Error_unexpected_record t)

let expect_a_list t err =
  match t with
  | TypeList element_type -> return element_type
  | TypeVar _ as t ->
      let* el_t, _ = add_list_constraint t in
      return el_t
  | _ -> error err

let expect_a_sum t err =
  match t with
  | TypeSum (l, r) -> return (l, r)
  | TypeVar _ as t ->
      let* l, r, _ = add_sum_constraint t in
      return (l, r)
  | _ -> error err

(* --- Solve --- *)

let print_constraints_system cs =
  let print_constraint index c =
    Logs.debug (fun m -> m "%d: %s" index (show_constraint c))
  in
  List.iteri cs ~f:print_constraint;
  ()

let solve_constraints =
  let* s = get in
  Logs.debug (fun m -> m "SOLVING %d CONSTRAINTS" (List.length s.constraints));
  print_constraints_system s.constraints;
  return ()
