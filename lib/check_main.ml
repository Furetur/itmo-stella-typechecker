open Base
open Stella_parser.Parsetree
open Errors
open Passes.Result_pass_syntax

let main_function_name = "main"

let as_main_function = function
  | DeclFun (_, StellaIdent name, params, ret_t, _, _, _)
    when String.equal name main_function_name ->
      Some (params, ret_t)
  | _ -> None

let find_last_main_function decls =
  List.map decls ~f:as_main_function |> List.filter_opt |> List.last

let check_main (AProgram (_, _, decls)) : unit pass_result =
  match find_last_main_function decls with
  | None -> fail (whole_file_error Errors.Error_missing_main)
  | Some (params, _) ->
      if List.length params <> 1 then
        fail (whole_file_error Error_incorrect_arity_of_main)
      else return ()
