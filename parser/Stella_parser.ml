open Base
module Parsetree = AbsSyntax
module Pretty_print_tree = PrintSyntax
module Show_tree = ShowSyntax

type parsing_error = { start_pos : Lexing.position; end_pos : Lexing.position }

let parse (chan : In_channel.t) : (Parsetree.program, parsing_error) Result.t =
  let lexbuf = Lexing.from_channel chan in
  try
    let prog = ParSyntax.pProgram LexSyntax.token lexbuf in
    Ok prog
  with ParSyntax.Error ->
    let start_pos = Lexing.lexeme_start_p lexbuf
    and end_pos = Lexing.lexeme_end_p lexbuf in
    Error { start_pos; end_pos }

let pretty_print_program (tree : Parsetree.program) : string =
  Pretty_print_tree.printTree Pretty_print_tree.prtProgram tree

let pretty_print_type (type' : Parsetree.typeT) : string =
  Pretty_print_tree.printTree Pretty_print_tree.prtTypeT type'

let show_program (tree : Parsetree.program) : string =
  Show_tree.show (Show_tree.showProgram tree)
