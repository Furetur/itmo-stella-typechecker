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

  let error kind = fail { kind; stacktrace } in

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
      match equations with
      | [] ->
          Logs.debug (fun m ->
              m
                "unify: This is the last equation so we report the \
                 infinite-type error");
          let err_kind =
            Error_occurs_check_infinite_type
              { type' = TypeVar var; type_value = expr }
          in
          let err = { kind = err_kind; stacktrace } in
          Error err
      | _ ->
          Logs.debug (fun m ->
              m
                "unify: There are several equations left so we put this error \
                 off");
          let this_equation =
            { expected = TypeVar var; actual = expr; stacktrace }
          in
          let equations' = equations @ [ this_equation ] in
          unify acc equations'
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

  let replace_with_pairwise_equations expected_els actual_els equations =
    let* new_equations = make_pairwise_equations expected_els actual_els in
    let equations = new_equations @ equations in
    unify acc equations
  in

  let unify_functions args1 ret1 args2 ret2 equations =
    let* arg_equations = make_pairwise_equations args1 args2 in
    let ret_equation = { expected = ret1; actual = ret2; stacktrace } in
    let new_equations = ret_equation :: arg_equations in
    let equations = new_equations @ equations in
    unify acc equations
  in

  let unify_tuples els1 els2 equations =
    replace_with_pairwise_equations els1 els2 equations
  in

  let unify_sum l1 r1 l2 r2 equations =
    replace_with_pairwise_equations [ l1; r1 ] [ l2; r2 ] equations
  in

  let unify_list el1 el2 equations =
    replace_with_pairwise_equations [ el1 ] [ el2 ] equations
  in

  let check_record_missing_keys expected_record_t expected_keys actual_keys =
    let missing_keys = Set.diff expected_keys actual_keys in
    match Set.choose missing_keys with
    | Some key ->
        error
          (Error_missing_record_fields
             { record_type = expected_record_t; field_name = StellaIdent key })
    | None -> ok
  in

  let check_record_unknown_keys expected_record_t expected_keys actual_keys =
    let unknown_keys = Set.diff actual_keys expected_keys in
    match Set.choose unknown_keys with
    | Some key ->
        error
          (Error_unexpected_record_fields
             { record_type = expected_record_t; field_name = StellaIdent key })
    | None -> ok
  in

  let unify_records expected_fields actual_fields equations =
    let expected_record_t = TypeRecord expected_fields in
    let expected_map = Type_map.of_record_fields expected_fields in
    let actual_map = Type_map.of_record_fields actual_fields in
    let expected_keys = Type_map.keys_set expected_map in
    let actual_keys = Type_map.keys_set actual_map in
    let* _ =
      check_record_missing_keys expected_record_t expected_keys actual_keys
    in
    let* _ =
      check_record_unknown_keys expected_record_t expected_keys actual_keys
    in
    let map =
      Map.merge expected_map actual_map ~f:(fun ~key:_ -> function
        | `Both (l, r) -> Some (l, r) | _ -> failwith "Key sets are equal")
    in
    let expected_field_types, actual_field_types =
      map |> Map.data |> List.unzip
    in
    replace_with_pairwise_equations expected_field_types actual_field_types
      equations
  in

  let unify_refs el1 el2 equations =
    replace_with_pairwise_equations [ el1 ] [ el2 ] equations
  in

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
  | TypeRef el1, TypeRef el2 -> unify_refs el1 el2 equations
  | _ -> fail broad_error

let unify equations = unify empty_substitution equations
