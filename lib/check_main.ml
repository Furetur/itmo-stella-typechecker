open Base
open Stella_parser.Parsetree
open Utils

let main_function_name = "main"

let is_main_function = function
  | DeclFun (_, StellaIdent name, _, _, _, _, _) ->
      String.equal name main_function_name
  | _ -> false

let check_main (AProgram (_, _, decls)) =
  let has_main_fn = any (List.map decls ~f:is_main_function) in
  if has_main_fn then Ok () else Error Errors.Error_missing_main
