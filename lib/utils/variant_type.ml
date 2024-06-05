open Base
open Stella_parser.Parsetree
open Utils

type t =
  ( stellaIdent,
    optionalTyping,
    Stella_ident_comparator.comparator_witness )
  Map.t

let map_of_list_take_first = Map.of_alist_multi

let make (field_types : variantFieldType list) : t =
  field_types
  |> List.map ~f:(fun (AVariantFieldType (ident, typing)) -> (ident, typing))
  |> Map.of_alist_multi (module Stella_ident_comparator)
  |> Map.map ~f:(function x :: _ -> x | [] -> failwith "Unexpected")

let lookup_field (t : t) (name : stellaIdent) = Map.find t name

let set_of_constructors (t : t) =
  t |> Map.keys |> Set.of_list (module Stella_ident_comparator)
