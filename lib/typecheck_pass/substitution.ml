open Base
open Stella_parser.Parsetree
open Utils

type substitution = (string, typeT, String.comparator_witness) Map.t

let show_substitution sub =
  let show_each_var (var, value) =
    Printf.sprintf "%s => %s" var (pp_type value)
  in
  let list = Map.to_alist sub in
  list |> List.map ~f:show_each_var |> String.concat_lines

let empty_substitution = Map.empty (module String)

let simple_substitution (StellaIdent name) (t : typeT) : substitution =
  Map.set empty_substitution ~key:name ~data:t

(** Composition operator for substitutions.
    Equivalent to the function composision: 'a . b'  *)
let ( @@@ ) (a : substitution) (b : substitution) : substitution =
  let prioritize_a ~key:_ (el : _ Map.Merge_element.t) =
    match el with
    | `Left b -> Some b
    | `Right a -> Some a
    | `Both (_, a) -> Some a
  in
  Map.merge b a ~f:prioritize_a

let apply_substitution_for_var (sub : substitution) (StellaIdent varname) =
  match Map.find sub varname with
  | Some t -> t
  | None -> TypeVar (StellaIdent varname)

let rec apply_substitution (sub : substitution) = function
  | TypeVar name -> apply_substitution_for_var sub name
  | TypeList t ->
      let t' = apply_substitution sub t in
      TypeList t'
  | TypeRef t ->
      let t' = apply_substitution sub t in
      TypeRef t'
  | TypeFun (args, ret) ->
      let args' = apply_substitution_for_types sub args in
      let ret' = apply_substitution sub ret in
      TypeFun (args', ret')
  | TypeSum (a, b) ->
      let a' = apply_substitution sub a in
      let b' = apply_substitution sub b in
      TypeSum (a', b')
  | TypeTuple ts ->
      let ts' = apply_substitution_for_types sub ts in
      TypeTuple ts'
  | TypeRecord fields ->
      let fields =
        List.map fields ~f:(fun (ARecordFieldType (name, t)) ->
            ARecordFieldType (name, apply_substitution sub t))
      in
      TypeRecord fields
  | t -> t

and apply_substitution_for_types sub types =
  types |> List.map ~f:(apply_substitution sub)
