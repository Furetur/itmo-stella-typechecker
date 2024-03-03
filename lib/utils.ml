open Base
open Stella_parser.Parsetree

let any bools = List.fold bools ~init:false ~f:( || )
let not_implemented () = failwith "Not implemented"

module Type_map = struct
  type t = (string, typeT, String.comparator_witness) Map.t

  let set_type map (StellaIdent name) type' = Map.set map ~key:name ~data:type'

  let set_types map ident_type_pairs =
    List.fold ident_type_pairs ~init:map ~f:(fun map (ident, type') ->
        set_type map ident type')

  let get_type map (StellaIdent name) = Map.find map name

  let of_globals (globals_list : (stellaIdent * typeT) list) =
    let init = Map.empty (module String) in
    set_types init globals_list

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

let pp_type type' = format_inline_code (Stella_parser.pretty_print_type type')
