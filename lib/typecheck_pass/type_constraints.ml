open Base
open Stella_parser.Parsetree
open Has_type_vars
open Errors

type constraint' = {
  expected : typeT;
  actual : typeT;
  stacktrace : string list;
}

type constraints_system = {
  constraints : constraint' list;
  next_typevar_id : int;
}

let _add_constraint c stacktrace expected actual =
  let con = { expected; actual; stacktrace } in
  let c' = { c with constraints = con :: c.constraints } in
  c'

let expect_equal_types (c : constraints_system) (stacktrace : string list)
    (expected : typeT) (actual : typeT) =
  let have_type_vars =
    check_has_typevars expected || check_has_typevars actual
  in
  if have_type_vars then
    let c' = _add_constraint c stacktrace expected actual in
    Ok (c', expected)
  else if Stdlib.(expected = actual) then Ok (c, expected)
  else
    let error_kind =
      Error_unexpected_type_for_expression { expected; actual }
    in
    let error = { kind = error_kind; stacktrace } in
    Error error

