open Base
open Stella_parser.Parsetree
open Errors


type subtype_result = (unit, error_kind) Result.t
(* Checks that S is subtype of T *)

type is_subtype_checker = s:typeT -> t:typeT -> subtype_result

type type_model = {
  is_subtype : s:typeT -> t:typeT -> subtype_result;
  is_subtype' : type_model -> s:typeT -> t:typeT -> subtype_result;
  is_subtype_record :
    type_model ->
    s_fields:recordFieldType list ->
    t_fields:recordFieldType list ->
    subtype_result;
  is_subtype_variant :
    type_model ->
    s_fields:variantFieldType list ->
    t_fields:variantFieldType list ->
    subtype_result;
  is_subtype_fun :
    type_model ->
    s_params:typeT list ->
    s_ret:typeT ->
    t_params:typeT list ->
    t_ret:typeT ->
    subtype_result;
  is_subtype_tuple :
    type_model -> s:typeT list -> t:typeT list -> subtype_result;
  is_subtype_sum :
    type_model ->
    s_left:typeT ->
    s_right:typeT ->
    t_left:typeT ->
    t_right:typeT ->
    subtype_result;
  is_subtype_list : type_model -> s_el:typeT -> t_el:typeT -> subtype_result;
  is_subtype_ref : type_model -> s_el:typeT -> t_el:typeT -> subtype_result;
}
