open Base
open Stdio
open Stella_parser

let fatal msg =
  print_endline msg;
  Stdlib.exit 1

let fatalf format args =
  let msg = Printf.sprintf format args in
  fatal msg

let print_parse_error { start_pos; end_pos } =
  Stdlib.Printf.printf "Parse error at %d.%d-%d.%d\n" start_pos.pos_lnum
    (start_pos.pos_cnum - start_pos.pos_bol + 1)
    end_pos.pos_lnum
    (end_pos.pos_cnum - end_pos.pos_bol + 1)
