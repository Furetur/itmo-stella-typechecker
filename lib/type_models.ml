open Base
open Stella_parser.Parsetree

type subtype_checker = s:typeT -> t:typeT -> bool
(* Checks that S is subtype of T *)

type type_model = { is_subtype : subtype_checker }

module type Type_model = sig
  val is_subtype : subtype_checker
end

module type Type_model_core = sig
  val is_subtype_record :
    subtype_checker -> s:recordFieldType list -> t:recordFieldType list -> bool
  (* Checks that record S is subtype of record T *)

  val is_subtype_tuple : subtype_checker -> s:typeT list -> t:typeT list -> bool
  (* Checks that tuple S is subtype of tuple T *)

  val is_subtype_bottom : subtype_checker -> typeT -> bool
  (* Checks whether Bottom is a subtype of the given type *)

  val is_subtype_top : subtype_checker -> typeT -> bool
  (* Checks whether the given type is a subtype of Top *)
end

module Make_type_model (Core : Type_model_core) : Type_model = struct
  let rec is_subtype_fun ~s_params ~s_ret ~t_params ~t_ret =
    (* Params are covariant *)
    let check_each_param (s_param, t_param) =
      is_subtype ~s:t_param ~t:s_param
    in
    let check_params = List.for_all ~f:check_each_param in
    let check_ret_t = is_subtype ~s:s_ret ~t:t_ret in
    match List.zip s_params t_params with
    | Unequal_lengths -> false
    | Ok param_pairs -> check_params param_pairs && check_ret_t

  and is_subtype_sum ~s_left ~s_right ~t_left ~t_right =
    is_subtype ~s:s_left ~t:t_left && is_subtype ~s:s_right ~t:t_right

  and is_subtype_list ~s_el ~t_el = is_subtype ~s:s_el ~t:t_el

  and is_subtype_ref ~s_el ~t_el =
    is_subtype ~s:s_el ~t:t_el && is_subtype ~s:t_el ~t:s_el

  and is_subtype ~s ~t =
    match (s, t) with
    | TypeFun (s_params, s_ret), TypeFun (t_params, t_ret) ->
        is_subtype_fun ~s_params ~s_ret ~t_params ~t_ret
    | TypeSum (s_left, s_right), TypeSum (t_left, t_right) ->
        is_subtype_sum ~s_left ~s_right ~t_left ~t_right
    | TypeTuple s, TypeTuple t -> Core.is_subtype_tuple is_subtype ~s ~t
    | TypeRecord s, TypeRecord t -> Core.is_subtype_record is_subtype ~s ~t
    | TypeList s_el, TypeList t_el -> is_subtype_list ~s_el ~t_el
    | TypeRef s_el, TypeRef t_el -> is_subtype_ref ~s_el ~t_el
    | TypeNat, TypeNat | TypeBool, TypeBool | TypeUnit, TypeUnit -> true
    | s, TypeTop -> Core.is_subtype_top is_subtype s
    | TypeBottom, t -> Core.is_subtype_bottom is_subtype t
    | _ -> false
end

let syntax_equality = Stdlib.( = )

module Syntax_equality_type_model : Type_model = Make_type_model (struct
  let is_subtype_tuple _ ~s ~t = syntax_equality s t
  let is_subtype_record _ ~s ~t = syntax_equality s t
  let is_subtype_top _ _ = false
  let is_subtype_bottom _ _ = false
end)

module Syntax_equality_with_top_bottom_type_model : Type_model =
Make_type_model (struct
  let is_subtype_tuple _ ~s ~t = syntax_equality s t
  let is_subtype_record _ ~s ~t = syntax_equality s t
  let is_subtype_top _ _ = true
  let is_subtype_bottom _ _ = true
end)

let f (is_subtype : subtype_checker) ~(s : recordFieldType list)
    ~(t : recordFieldType list) =
  let s_map = Type_map.of_record_fields s in
  let t_map = Type_map.of_record_fields t in
  let field_is_subtype s_field_name s_field_type =
    match Map.find t_map s_field_name with
    | Some t_field_type -> is_subtype ~s:s_field_type ~t:t_field_type
    | None -> false
  in
  let check_each_field ~key ~data = field_is_subtype key data in
  Map.for_alli s_map ~f:check_each_field

module Structural_subtyping_model : Type_model = Make_type_model (struct
  let is_subtype_top _ _ = true
  let is_subtype_bottom _ _ = true

  let is_subtype_record (is_subtype : subtype_checker)
      ~(s : recordFieldType list) ~(t : recordFieldType list) =
    let s_map = Type_map.of_record_fields s in
    let t_map = Type_map.of_record_fields t in
    let field_is_subtype s_field_name s_field_type =
      match Map.find t_map s_field_name with
      | Some t_field_type -> is_subtype ~s:s_field_type ~t:t_field_type
      | None -> false
    in
    let check_each_field ~key ~data = field_is_subtype key data in
    Map.for_alli s_map ~f:check_each_field

  let is_subtype_tuple (is_subtype : subtype_checker) ~(s : typeT list)
      ~(t : typeT list) =
    let convert_each_field index field_type =
      let ident = StellaIdent (Int.to_string index) in
      ARecordFieldType (ident, field_type)
    in
    let to_record_type = List.mapi ~f:convert_each_field in
    let s_record = to_record_type s in
    let t_record = to_record_type t in
    is_subtype_record is_subtype ~s:s_record ~t:t_record
end)
