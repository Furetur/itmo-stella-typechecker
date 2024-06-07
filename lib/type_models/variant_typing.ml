open Base
open Stella_parser.Parsetree
open Errors
open Passes.Result_pass_syntax
open Type_model
open Utils

let is_subtype_variant_structural m ~s_fields ~t_fields =
  let s = TypeVariant s_fields in
  let t = TypeVariant t_fields in
  let broad_error = Error_unexpected_subtype { expected = t; actual = s } in
  let variant_s = Variant_type.make s_fields in
  let variant_t = Variant_type.make t_fields in
  let is_subtype_optional_typing s_typing t_typing =
    Logs.debug (fun m ->
        m "is_subtype_optional_typing: s=%s t=%s" (pp_typing s_typing)
          (pp_typing t_typing));
    match (s_typing, t_typing) with
    | NoTyping, NoTyping -> ok
    | SomeTyping s, SomeTyping t -> m.is_subtype ~s ~t
    | _ -> fail broad_error
  in
  (* let check_all_constructors_match () =
       let s_constrs = Variant_type.set_of_constructors variant_s in
       let t_constrs = Variant_type.set_of_constructors variant_t in
       match Set.choose (Set.diff s_constrs t_constrs) with
       | None -> ok
       | Some label -> fail (Error_unexpected_variant_label (t, label))
     in *)
  let check_each_s_constructor (s_name, s_typing) =
    let (StellaIdent s_name') = s_name in
    Logs.debug (fun m ->
        m "is_subtype_variant: check label='%s' typing='%s' of s=%s t=%s"
          s_name' (pp_typing s_typing) (pp_type s) (pp_type t));
    match Variant_type.lookup_field variant_t s_name with
    | None ->
        Logs.debug (fun m ->
            m "Label '%s' not found in t=%s" s_name' (pp_type t));
        fail (Error_unexpected_variant_label (t, s_name))
    | Some t_typing -> is_subtype_optional_typing s_typing t_typing
  in
  variant_s |> Map.to_alist |> all_unit ~f:check_each_s_constructor
