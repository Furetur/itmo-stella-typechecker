open Base
open Stella_parser.Parsetree

let get_extentions (AProgram (_, ext, _)) =
  ext
  |> List.map ~f:(fun (AnExtension e) -> e)
  |> List.concat
  |> List.map ~f:(fun (ExtensionName e) -> e)

let is_extention_enabled extentions extention =
  List.mem extentions extention ~equal:String.( = )
