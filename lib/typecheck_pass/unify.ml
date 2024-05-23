open Base
open Errors
open Stella_parser.Parsetree
open Passes.Result_pass_syntax
open Has_type_vars
open Substitution
open Utils

type type_equation = {
  expected : typeT;
  actual : typeT;
  stacktrace : string list;
}

let show_equation { expected; actual; _ } =
  Printf.sprintf "%s = %s" (Utils.pp_type expected) (Utils.pp_type actual)

let log_equations eqs =
  let print_constraint index c =
    Logs.debug (fun m -> m "%d: %s" index (show_equation c))
  in
  List.iteri eqs ~f:print_constraint;
  Logs.debug (fun m -> m "\n");
  ()

let apply_substitution_to_equation sub { expected; actual; stacktrace } =
  {
    expected = apply_substitution sub expected;
    actual = apply_substitution sub actual;
    stacktrace;
  }

let apply_substitution_to_equations sub =
  List.map ~f:(apply_substitution_to_equation sub)

let rec unify (acc : substitution) (equations : type_equation list) :
    (substitution, error) Result.t =
  let n_equations = List.length equations in
  Logs.debug (fun m -> m "unify: %d equations left" n_equations);
  log_equations equations;
  Logs.debug (fun m -> m "substitution = \n%s" (show_substitution acc));
  match equations with
  | [] -> return empty_substitution
  | { expected; actual; stacktrace } :: equations ->
      if Utils.syntactically_equal expected actual then unify acc equations
      else unify_solve acc expected actual stacktrace equations

(** This function solves one equation and progresses further *)
and unify_solve acc expected actual stacktrace equations =
  let broad_error =
    {
      kind = Error_unexpected_type_for_expression { expected; actual };
      stacktrace;
    }
  in

  let apply_solution_for_1_var var var_value equations =
    (* We know that 'var = var_value'.
       We need to replace all occurences of var in the equations
       and proceed. *)
    let (StellaIdent name) = var in
    Logs.debug (fun m -> m "unify: Hooray! %s = %s" name (pp_type var_value));
    let sub = simple_substitution var var_value in
    let equations = apply_substitution_to_equations sub equations in
    let acc = sub @@@ acc in
    unify acc equations
  in

  let solve_immediate_equality var expr equations =
    let (StellaIdent varname) = var in
    Logs.debug (fun m ->
        m "This is an immediate equality equation: %s = %s" varname
          (pp_type expr));
    if not (has_typevar var expr) then
      apply_solution_for_1_var var expr equations
    else
      let () =
        Logs.debug (fun m -> m "unify: %s is an infinite type!" varname)
      in
      let err_kind =
        Error_occurs_check_infinite_type
          { type' = TypeVar var; type_value = expr }
      in
      let err = { kind = err_kind; stacktrace } in
      Error err
  in

  let make_pairwise_equations expected_els actual_els =
    match List.zip expected_els actual_els with
    | Unequal_lengths -> fail broad_error
    | Ok pairs ->
        let eqs =
          List.map pairs ~f:(fun (expected, actual) ->
              { expected; actual; stacktrace })
        in
        return eqs
  in

  let unify_functions args1 ret1 args2 ret2 equations =
    let* arg_equations = make_pairwise_equations args1 args2 in
    let ret_equation = { expected = ret1; actual = ret2; stacktrace } in
    let new_equations = ret_equation :: arg_equations in
    let equations = new_equations @ equations in
    unify acc equations
  in

  let unify_tuples els1 els2 equations =
    let* new_equations = make_pairwise_equations els1 els2 in
    let equations = new_equations @ equations in
    unify acc equations
  in

  let unify_sum l1 r1 l2 r2 equations =
    let* new_equations = make_pairwise_equations [ l1; r1 ] [ l2; r2 ] in
    let equations = new_equations @ equations in
    unify acc equations
  in

  let unify_list el1 el2 equations =
    let* new_equations = make_pairwise_equations [ el1 ] [ el2 ] in
    let equations = new_equations @ equations in
    unify acc equations
  in

  let unify_records _ _ _ = Utils.not_implemented () in

  match (expected, actual) with
  | TypeVar x, solution -> solve_immediate_equality x solution equations
  | solution, TypeVar x -> solve_immediate_equality x solution equations
  | TypeFun (args1, ret1), TypeFun (args2, ret2) ->
      unify_functions args1 ret1 args2 ret2 equations
  | TypeTuple els1, TypeTuple els2 -> unify_tuples els1 els2 equations
  | TypeRecord fields1, TypeRecord fields2 ->
      unify_records fields1 fields2 equations
  | TypeSum (l1, r1), TypeSum (l2, r2) -> unify_sum l1 r1 l2 r2 equations
  | TypeList el1, TypeList el2 -> unify_list el1 el2 equations
  | _ -> fail broad_error

let unify equations = unify empty_substitution equations
