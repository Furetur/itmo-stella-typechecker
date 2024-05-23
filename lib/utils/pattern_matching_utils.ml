open Base
open Stella_parser.Parsetree

let does_match_have_not_implemented_patterns patterns =
  let is_not_implemented = function
    | PatternInl _ | PatternInr _ | PatternVar _ -> false
    | _ -> true
  in
  List.exists patterns ~f:is_not_implemented

let check_has_inl patterns =
  patterns |> List.exists ~f:(function PatternInl _ -> true | _ -> false)

let check_has_inr patterns =
  patterns |> List.exists ~f:(function PatternInr _ -> true | _ -> false)

let check_is_sum_match pats = check_has_inl pats || check_has_inr pats

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
  let result = has_var || (has_inl && has_inr) in
  Logs.debug (fun m ->
      m
        "Pattern-matching exhaustiveness check:\n\
        \ has_var=%b\n\
        \ has_inl=%b\n\
        \ has_inr=%b\n\
        \ result=%b" has_var has_inl has_inr result);
  result

let is_other_match_exhaustive patterns =
  List.exists patterns ~f:(function PatternVar _ -> true | _ -> false)

let is_match_exhaustive patterns =
  if check_is_sum_match patterns then is_sum_match_exhaustive patterns
  else is_other_match_exhaustive patterns
