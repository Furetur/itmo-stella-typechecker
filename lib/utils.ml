open Base

let any bools = List.fold bools ~init:false ~f:( || )
