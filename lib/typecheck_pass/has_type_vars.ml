open Base
open Stella_parser.Parsetree

let rec _check_type = function
  | TypeVar _ -> true
  | TypeNat | TypeBool | TypeBottom | TypeTop | TypeUnit -> false
  | TypeList t | TypeRef t -> _check_type t
  | TypeFun (args, ret) ->
      let types = ret :: args in
      List.exists types ~f:_check_type
  | TypeSum (a, b) -> _check_types [ a; b ]
  | TypeTuple ts -> _check_types ts
  | TypeRecord fields ->
      let types = List.map fields ~f:(fun (ARecordFieldType (_, t)) -> t) in
      _check_types types
  | _ -> Utils.not_implemented ()

and _check_types = List.exists ~f:_check_type

let check_has_typevars (t : typeT) : bool = _check_type t
