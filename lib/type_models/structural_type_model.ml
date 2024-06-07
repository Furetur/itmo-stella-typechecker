open Base
open Stella_parser.Parsetree
open Type_model
open Utils
open Errors
open Passes.Result_pass_syntax

let make_error ~s ~t =
  Error_unexpected_subtype { expected = t; actual = s }

let unexpected_type_error ~s ~t = fail (make_error ~s ~t)

let replace_error_if_needed ~s ~t =
  let broader_error = make_error ~s ~t in
  Result.map_error ~f:(function
    | Error_unexpected_subtype _ -> broader_error
    | e -> e)

let is_subtype_fun m ~s_params ~s_ret ~t_params ~t_ret =
  (* Params are covariant *)
  let check_each_param (s_param, t_param) =
    m.is_subtype ~s:t_param ~t:s_param
  in
  let check_params = all_unit ~f:check_each_param in
  let check_ret_t = m.is_subtype ~s:s_ret ~t:t_ret in
  match List.zip s_params t_params with
  | Unequal_lengths -> fail Error_unknown
  | Ok param_pairs -> check_params param_pairs *> check_ret_t

let is_subtype_tuple m ~(s : typeT list) ~(t : typeT list) =
  match List.zip s t with
  | Ok type_pairs -> all_unit type_pairs ~f:(fun (s, t) -> m.is_subtype ~s ~t)
  | _ ->
      let expected_length = List.length t in
      fail (Error_unexpected_tuple_length { expected = expected_length })

let is_subtype_sum m ~s_left ~s_right ~t_left ~t_right =
  m.is_subtype ~s:s_left ~t:t_left *> m.is_subtype ~s:s_right ~t:t_right

let is_subtype_list m ~s_el ~t_el = m.is_subtype ~s:s_el ~t:t_el

let is_subtype_ref m ~s_el ~t_el =
  m.is_subtype ~s:s_el ~t:t_el *> m.is_subtype ~s:t_el ~t:s_el

let is_subtype' (m : type_model) ~(s : typeT) ~(t : typeT) : subtype_result =
  Logs.debug (fun m -> m "is_subtype: s=%s t=%s" (pp_type s) (pp_type t));
  match (s, t) with
  | TypeFun (s_params, s_ret), TypeFun (t_params, t_ret) ->
      m.is_subtype_fun m ~s_params ~s_ret ~t_params ~t_ret
  | TypeSum (s_left, s_right), TypeSum (t_left, t_right) ->
      m.is_subtype_sum m ~s_left ~s_right ~t_left ~t_right
  | TypeTuple s, TypeTuple t -> m.is_subtype_tuple m ~s ~t
  | TypeRecord s_fields, TypeRecord t_fields ->
      m.is_subtype_record m ~s_fields ~t_fields
  | TypeList s_el, TypeList t_el -> m.is_subtype_list m ~s_el ~t_el
  | TypeRef s_el, TypeRef t_el -> m.is_subtype_ref m ~s_el ~t_el
  | TypeVariant s_fields, TypeVariant t_fields ->
      m.is_subtype_variant m ~s_fields ~t_fields
  | TypeNat, TypeNat | TypeBool, TypeBool | TypeUnit, TypeUnit -> ok
  | _, TypeTop -> ok
  | TypeBottom, _ -> ok
  | _ -> unexpected_type_error ~s ~t

let rec is_subtype ~s ~t = is_subtype' default_type_model ~s ~t

and default_type_model =
  {
    is_subtype;
    is_subtype_list;
    is_subtype_sum;
    is_subtype_tuple;
    is_subtype_fun;
    is_subtype_variant = Variant_typing.is_subtype_variant_structural;
    is_subtype_ref;
    is_subtype';
    is_subtype_record = Record_typing.is_subtype_record_structural;
  }
