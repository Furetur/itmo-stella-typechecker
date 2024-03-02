open Base
open Lexing
open Stella_parser

let print_result t =
  "[Abstract syntax]\n\n" ^ show_program t ^ "\n\n" ^ "[Linearized tree]\n\n"
  ^ pretty_print_program t ^ "\n"
  |> Stdio.print_string

let print_parse_error { start_pos; end_pos } =
  Stdlib.Printf.printf "Parse error at %d.%d-%d.%d\n" start_pos.pos_lnum
    (start_pos.pos_cnum - start_pos.pos_bol + 1)
    end_pos.pos_lnum
    (end_pos.pos_cnum - end_pos.pos_bol + 1)

let fatal msg =
  Stdio.print_string msg;
  Stdlib.exit 1

let fatalf format args =
  let msg = Printf.sprintf format args in
  fatal msg

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

let main () =
  let inpath = get_input_file_path () in
  let tree = parse_file inpath in
  print_result tree
;;

main ()
