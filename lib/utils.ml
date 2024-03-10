open Base
open Stella_parser.Parsetree

let any bools = List.fold bools ~init:false ~f:( || )
let not_implemented () = failwith "ERROR_NOT_IMPLEMENTED"

let expr_not_implemented expr =
  let open Stella_parser.Show_tree in
  Printf.failwithf
    "ERROR_NOT_IMPLEMENTED: This type of expressions is not implemented: %s"
    (show (showExpr expr))
    ()

module Type_map = struct
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
end

let type_of_returnType = function
  | NoReturnType -> TypeUnit
  | SomeReturnType t -> t

let format_inline_code =
  let is_not_newline char = Char.( <> ) char '\n' && Char.( <> ) char '\t' in
  String.filter ~f:is_not_newline

let pp_type type' =
  Stella_parser.Show_tree.show (Stella_parser.Show_tree.showTypeT type')
(* format_inline_code (Stella_parser.pretty_print_type type') *)

let extract_first_inl_and_inr match_cases =
  let is_inl = function AMatchCase (PatternInl _, _) -> true | _ -> false in
  let is_inr = function AMatchCase (PatternInr _, _) -> true | _ -> false in
  (List.find match_cases ~f:is_inl, List.find match_cases ~f:is_inr)
