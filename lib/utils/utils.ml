open Base
open Stella_parser.Parsetree

let not_implemented () = failwith "ERROR_NOT_IMPLEMENTED"

let expr_not_implemented expr =
  let open Stella_parser.Show_tree in
  Printf.failwithf
    "ERROR_NOT_IMPLEMENTED: This type of expressions is not implemented: %s"
    (show (showExpr expr))
    ()

let type_of_returnType = function
  | NoReturnType -> TypeUnit
  | SomeReturnType t -> t

let format_inline_code =
  let is_not_newline char = Char.( <> ) char '\n' && Char.( <> ) char '\t' in
  String.filter ~f:is_not_newline

let pp_type type' = format_inline_code (Stella_parser.pretty_print_type type')

let collect_exception_type (AProgram (_, _, decls)) : typeT option =
  let extract_exn_type = function DeclExceptionType t -> Some t | _ -> None in
  decls |> List.filter_map ~f:extract_exn_type |> List.last
