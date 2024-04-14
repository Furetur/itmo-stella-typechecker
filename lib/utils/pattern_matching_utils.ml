open Base
open Stella_parser.Parsetree

let does_match_have_not_implemented_patterns patterns =
  let is_not_implemented = function
    | PatternInl _ | PatternInr _ | PatternVar _ -> false
    | _ -> true
  in
  List.exists patterns ~f:is_not_implemented

let is_sum_match_exhaustive patterns =
  let has_inl =
    patterns |> List.exists ~f:(function PatternInl _ -> true | _ -> false)
  in
  let has_inr =
    patterns |> List.exists ~f:(function PatternInr _ -> true | _ -> false)
  in
  let has_var =
    patterns |> List.exists ~f:(function PatternVar _ -> true | _ -> false)
  in
  has_var || (has_inl && has_inr)

let is_other_match_exhaustive patterns =
  List.exists patterns ~f:(function PatternVar _ -> true | _ -> false)
