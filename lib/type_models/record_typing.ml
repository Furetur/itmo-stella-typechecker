open Base
open Stella_parser.Parsetree
open Type_model
open Utils
open Errors
open Passes.Result_pass_syntax

(* let check_missing_record_fields ~s_fields ~t_fields =
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
       ok *)

let check_each_record_field m ~s_fields ~t_fields =
  let t = TypeRecord t_fields in
  let s_map = Type_map.of_record_fields s_fields in
  let t_map = Type_map.of_record_fields t_fields in
  let field_is_subtype t_field_name t_field_type =
    match Map.find s_map t_field_name with
    | Some s_field_type -> m.is_subtype ~s:s_field_type ~t:t_field_type
    | None ->
        Logs.debug (fun m ->
            m "check_each_record_field: field %s is missing in S" t_field_name);
        fail
          (Error_missing_record_fields
             { field_name = StellaIdent t_field_name; record_type = t })
  in
  let check_each_field (key, data) = field_is_subtype key data in
  t_map |> Map.to_alist |> all_unit ~f:check_each_field

let print_result ~s_fields ~t_fields result =
  let s = TypeRecord s_fields in
  let t = TypeRecord t_fields in
  let bool_result = Result.is_ok result in
  Logs.debug (fun m ->
      m "Is %s a subtype of %s ==> %b" (pp_type s) (pp_type t) bool_result);
  result

let is_subtype_record_structural m ~s_fields ~t_fields =
  print_result ~s_fields ~t_fields
  @@ check_each_record_field m ~s_fields ~t_fields
