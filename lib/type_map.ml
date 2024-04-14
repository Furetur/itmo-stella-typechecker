open Base
open Stella_parser.Parsetree

type t = (string, typeT, String.comparator_witness) Map.t

let set_type map (StellaIdent name) type' = Map.set map ~key:name ~data:type'

let set_types map ident_type_pairs =
  List.fold ident_type_pairs ~init:map ~f:(fun map (ident, type') ->
      set_type map ident type')

let get_type map (StellaIdent name) = Map.find map name
let remove map (StellaIdent name) = Map.remove map name

let of_list (pairs_list : (stellaIdent * typeT) list) =
  let init = Map.empty (module String) in
  set_types init pairs_list

let of_globals = of_list

let of_record_fields (field_types : recordFieldType list) =
  let ident_type_pairs =
    List.map field_types ~f:(fun (ARecordFieldType (ident, t)) -> (ident, t))
  in
  of_list ident_type_pairs

let add_params map params =
  let ident_type_pairs =
    List.map params ~f:(fun (AParamDecl (id, t)) -> (id, t))
  in
  set_types map ident_type_pairs
