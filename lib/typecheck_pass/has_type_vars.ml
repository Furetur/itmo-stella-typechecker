open Base
open Stella_parser.Parsetree

let rec _check_type = function
  | TypeVar name -> [ name ]
  | TypeNat | TypeBool | TypeBottom | TypeTop | TypeUnit -> []
  | TypeList t | TypeRef t -> _check_type t
  | TypeFun (args, ret) ->
      let types = ret :: args in
      _check_types types
  | TypeSum (a, b) -> _check_types [ a; b ]
  | TypeTuple ts -> _check_types ts
  | TypeRecord fields ->
      let types = List.map fields ~f:(fun (ARecordFieldType (_, t)) -> t) in
      _check_types types
  | _ -> Utils.not_implemented ()

and _check_types types = types |> List.map ~f:_check_type |> List.concat

let collect_typevars (t : typeT) : string list =
  t |> _check_type |> List.map ~f:(fun (StellaIdent x) -> x)

let has_typevar (StellaIdent name) (t : typeT) : bool =
  let typevars = collect_typevars t in
  List.mem typevars name ~equal:String.equal

let check_has_typevars (t : typeT) : bool =
  let typevars = collect_typevars t in
  not (List.is_empty typevars)
