open Base
open Stella_parser.Parsetree
open Utils
open Errors
open Passes.Result_pass_syntax

(*
and diagnose_strict_record_type_mismatch record_field_types record_bindings =
  let record_type = TypeRecord record_field_types in
  let check_each_binding remaining_fields_typemap (ABinding (field_ident, expr))
      =
    match Type_map.get_type remaining_fields_typemap field_ident with
    | None ->
        error
          (Error_unexpected_record_fields
             { record_type; field_name = field_ident })
    | Some field_t ->
        check_expr (Some field_t) expr
        *>
        let new_typemap =
          Type_map.remove remaining_fields_typemap field_ident
        in
        return new_typemap
  in
  let field_typemap = Type_map.of_record_fields record_field_types in
  let* remaining_required_fields =
    fold record_bindings ~init:field_typemap ~f:check_each_binding
  in
  match Map.to_alist remaining_required_fields with
  | [] -> return record_type
  | (field_name, _) :: _ ->
      error
        (Error_missing_record_fields
           { record_type; field_name = StellaIdent field_name })
*)

let check_missing_record_fields ~s_fields ~t_fields =
  let s = TypeRecord s_fields in
  let t = TypeRecord t_fields in
  let s_field_names = Type_map.keys_set (Type_map.of_record_fields s_fields) in
  let t_field_names = Type_map.keys_set (Type_map.of_record_fields t_fields) in
  Logs.debug (fun m ->
      m "default_type_model.check_missing_record_fields: enter s=%s t=%s"
        (pp_type s) (pp_type t));
  let diff = Set.diff t_field_names s_field_names in
  match Set.choose diff with
  | Some field ->
      Logs.debug (fun m ->
          m
            "default_type_model.check_missing_record_fields: field '%s' is \
             missing"
            field);
      fail
        (Error_missing_record_fields
           { field_name = StellaIdent field; record_type = t })
  | None ->
      Logs.debug (fun m ->
          m "default_type_model.check_missing_record_fields: ok");
      ok

let rec check_each_record_field ~s_fields ~t_fields =
  let t = TypeRecord t_fields in
  let s_map = Type_map.of_record_fields s_fields in
  let t_map = Type_map.of_record_fields t_fields in
  let field_is_subtype t_field_name t_field_type =
    match Map.find s_map t_field_name with
    | Some s_field_type -> is_subtype ~s:s_field_type ~t:t_field_type
    | None ->
        Logs.debug (fun m ->
            m "check_each_record_field: field %s is missing in S" t_field_name);
        fail
          (Error_missing_record_fields
             { field_name = StellaIdent t_field_name; record_type = t })
  in
  let check_each_field (key, data) = field_is_subtype key data in
  t_map |> Map.to_alist |> all_unit ~f:check_each_field

and is_subtype_record ~s_fields ~t_fields =
  check_missing_record_fields ~s_fields ~t_fields
  *> check_each_record_field ~s_fields ~t_fields

and is_subtype ~s ~t =
  match (s, t) with
  (* | TypeRecord s_fields, TypeRecord t_fields ->
      is_subtype_record ~s_fields ~t_fields *)
  | _ ->
      if Stdlib.(s = t) then ok
      else
        fail (Error_unexpected_type_for_expression { expected = t; actual = s })
