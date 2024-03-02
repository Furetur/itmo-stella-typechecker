open Base
open Utils
open Stella_parser

let get_input_file_path () =
  if Array.length Stdlib.Sys.argv <= 1 then fatal "Argument FILE required"
  else
    let arg = Stdlib.Sys.argv.(1) in
    match Fpath.of_string arg with
    | Error (`Msg msg) -> fatalf "Invalid path: %s" msg
    | Ok path -> path

let parse_file path =
  let path = Fpath.to_string path in
  let parsing_result =
    try Stdio.In_channel.with_file path ~f:parse
    with Sys_error msg -> fatal msg
  in
  match parsing_result with
  | Error err ->
      print_parse_error err;
      Stdlib.exit 1
  | Ok tree -> tree

let typecheck_program prog =
  match Stella_typechecker.check prog with
  | Ok () -> ()
  | Error err ->
      let msg = Stella_typechecker.Errors.show err in
      fatal msg

let main () =
  let inpath = get_input_file_path () in
  let tree = parse_file inpath in
  typecheck_program tree
;;

main ()
