open Base
open Stella_parser.Parsetree
open Stella_parser.Pretty_print_tree
open Utils
open Pattern_matching_utils
open Errors
open Type_models

(* --- Pass Setup --- *)

type state = {
  ambiguous_as_bottom : bool;
  type_model : (module Type_model);
  is_subtyping_enabled : bool;
  stacktrace : string list;
  typemap : Type_map.t;
  exception_type : typeT option;
}

module ThisPass = Passes.SingleError (struct
  type pass_state = state
  type pass_error = error
end)

open ThisPass

(* --- Errors --- *)

let in_error_context context f =
  let* old_state = get in
  set { old_state with stacktrace = context :: old_state.stacktrace }
  *>
  let* result = f in
  set old_state *> return result

let in_expr_error_context context expected_type f =
  let context = format_inline_code context in
  let type' = pp_expected_type expected_type in
  let context = Printf.sprintf "%s <--- Expected: %s" context type' in
  in_error_context context f

let error kind =
  let* { stacktrace; _ } = get in
  fail { kind; stacktrace }

let zip_or_error error_kind list1 list2 =
  match List.zip list1 list2 with
  | Unequal_lengths -> error error_kind
  | Ok zipped -> return zipped

(* --- State --- *)

let get_var_type ident =
  let* s = get in
  match Type_map.get_type s.typemap ident with
  | Some t -> return t
  | None -> error (Error_undefined_variable ident)

let set_typemap new_typemap =
  let* s = get in
  set { s with typemap = new_typemap }

let with_updated_typemap pass update_typemap =
  let* s = get in
  let new_typemap = update_typemap s.typemap in
  set_typemap new_typemap
  *> let* result = pass in
     set_typemap s.typemap *> return result

let with_new_binding ident type' pass =
  with_updated_typemap pass @@ fun typemap ->
  Type_map.set_type typemap ident type'

let with_new_bindings ident_type_pairs pass =
  with_updated_typemap pass @@ fun typemap ->
  Type_map.set_types typemap ident_type_pairs

let with_param_bindings param_decls pass =
  with_updated_typemap pass @@ fun typemap ->
  Type_map.add_params typemap param_decls

let is_subtype ~subtype ~supertype =
  let* { type_model; _ } = get in
  let module M = (val type_model) in
  return (M.is_subtype ~s:subtype ~t:supertype)

let expect_equal_type expected_type actual_type =
  match expected_type with
  | None -> return actual_type
  | Some expected_type ->
      let* { is_subtyping_enabled; _ } = get in
      let* are_types_compatible =
        is_subtype ~subtype:actual_type ~supertype:expected_type
      in
      if are_types_compatible then return actual_type
      else
        error
          (if is_subtyping_enabled then
             Error_unexpected_subtype
               { expected = expected_type; actual = actual_type }
           else
             Error_unexpected_type_for_expression
               { expected = expected_type; actual = actual_type })

let disambiguate_with_bottom otherwise_error =
  let* { ambiguous_as_bottom; _ } = get in
  if ambiguous_as_bottom then return TypeBottom else error otherwise_error

(* ----- Pass ----- *)

(* --- Expressions --- *)

let rec check_application expected_type callee args =
  Logs.debug (fun m ->
      m "check_application: expected=%s" (pp_expected_type expected_type));
  let check_arg_types expected_arg_types args =
    let* type_arg_pairs =
      zip_or_error Error_incorrect_number_of_arguments expected_arg_types args
    in
    check_exprs type_arg_pairs
  in
  Logs.debug (fun m -> m "check_application: Entering callee");
  let* callee_type = check_expr None callee in
  Logs.debug (fun m ->
      m "check_application: callee_type=%s" (pp_type callee_type));
  match callee_type with
  | TypeFun (arg_types, ret_type) ->
      expect_equal_type expected_type ret_type
      *> check_arg_types arg_types args
      *> return ret_type
  | _ ->
      Logs.debug (fun m -> m "check_application: Error_not_a_function");
      error Error_not_a_function

and check_abstraction expected_type param_decls body_expr =
  Logs.debug (fun m ->
      m "check_abstraction: enter, expected_type=%s"
        (pp_expected_type expected_type));
  let expect_equal_param_types (expected, actual) =
    let* is_ok = is_subtype ~subtype:expected ~supertype:actual in
    if is_ok then return ()
    else error (Error_unexpected_type_for_parameter { expected; actual })
  in
  let check_params expected_param_types =
    let actual_param_types =
      List.map param_decls ~f:(fun (AParamDecl (_, t)) -> t)
    in
    let expected_n_params = List.length expected_param_types in
    let* type_pairs =
      zip_or_error
        (Error_unexpected_number_of_parameters_in_lambda
           { expected = expected_n_params })
        expected_param_types actual_param_types
    in
    many_unit type_pairs ~f:expect_equal_param_types
  in
  match expected_type with
  | Some (TypeFun (expected_param_types, expected_ret_t) as result_t) ->
      check_params expected_param_types
      *> check_abstraction_body (Some expected_ret_t) param_decls body_expr
      *> return result_t
  | Some TypeTop ->
      check_unknown_abstraction param_decls body_expr *> return TypeTop
  | Some t -> error (Error_unexpected_lambda { expected = t })
  | None -> check_unknown_abstraction param_decls body_expr

and check_unknown_abstraction param_decls body_expr =
  let actual_param_types =
    List.map param_decls ~f:(fun (AParamDecl (_, t)) -> t)
  in
  let* ret_t = check_abstraction_body None param_decls body_expr in
  return (TypeFun (actual_param_types, ret_t))

and check_abstraction_body expected_type param_decls body_expr =
  with_param_bindings param_decls (check_expr expected_type body_expr)

and check_if expected_type cond then_expr else_expr =
  Logs.debug (fun m -> m "check_if: Enter");
  check_expr (Some TypeBool) cond
  *>
  match expected_type with
  | Some expected_type ->
      Logs.debug (fun m ->
          m "check_if: known expected_type=%s" (pp_type expected_type));
      check_expr (Some expected_type) then_expr
      *> check_expr (Some expected_type) else_expr
      *> return expected_type
  | None ->
      Logs.debug (fun m ->
          m "check_if: expected type unknown, checking 'then' branch");
      let* then_type = check_expr None then_expr in
      Logs.debug (fun m ->
          m "check_if: unknown expected type, but then_type=%s"
            (pp_type then_type));
      check_expr (Some then_type) else_expr

and check_each_let_binding = function
  | APatternBinding (PatternVar ident, expr) ->
      let* expr_type = check_expr None expr in
      return (ident, expr_type)
  | _ -> not_implemented ()

and check_let expected_type pattern_bindings body_expr =
  let* ident_type_pairs = pattern_bindings |> many ~f:check_each_let_binding in
  with_new_bindings ident_type_pairs (check_expr expected_type body_expr)

(* - Tuple - *)

and check_dot_tuple expected_type expr index =
  let* expr_type = check_expr None expr in
  match expr_type with
  | TypeTuple component_types -> (
      match List.nth component_types (index - 1) with
      | None ->
          error
            (Error_tuple_index_out_of_bounds
               { max_index = List.length component_types; actual_index = index })
      | Some component_type -> expect_equal_type expected_type component_type)
  | _ -> error (Error_not_a_tuple expr_type)

and check_tuple expected_type (exprs : expr list) =
  match expected_type with
  | Some (TypeTuple component_types) ->
      let wrong_length_error =
        Error_unexpected_tuple_length { expected = List.length component_types }
      in
      let* type_expr_pairs =
        zip_or_error wrong_length_error component_types exprs
      in
      check_exprs type_expr_pairs *> return (TypeTuple component_types)
  | Some TypeTop -> many exprs ~f:(check_expr None) *> return TypeTop
  | Some expected_type -> error (Error_unexpected_tuple { expected_type })
  | None ->
      let* actual_component_types = many exprs ~f:(check_expr None) in
      return (TypeTuple actual_component_types)

(* - Record - *)

and check_dot_record expected_type record_expr field_ident =
  let* record_type = check_expr None record_expr in
  match record_type with
  | TypeRecord field_types -> (
      let fields_typemap = Type_map.of_record_fields field_types in
      match Type_map.get_type fields_typemap field_ident with
      | None ->
          error
            (Error_unexpected_field_access
               { typeT = record_type; field_name = field_ident })
      | Some result_type -> expect_equal_type expected_type result_type)
  | _ -> error (Error_not_a_record record_type)

and check_record expected_type bindings =
  match expected_type with
  | Some (TypeRecord field_types) ->
      check_known_type_record field_types bindings
  | Some TypeTop -> check_unknown_record bindings *> return TypeTop
  | Some t -> error (Error_unexpected_record t)
  | None -> check_unknown_record bindings

and diagnose_strict_record_type_mismatch record_field_types record_bindings =
  let record_type = TypeRecord record_field_types in
  let check_each_binding remaining_fields_typemap (ABinding (field_ident, expr))
      =
    match Type_map.get_type remaining_fields_typemap field_ident with
    | None ->
        error
          (Error_unexpected_record_fields
             { record_type; field_name = field_ident })
    | Some field_t ->
        check_expr (Some field_t) expr
        *>
        let new_typemap =
          Type_map.remove remaining_fields_typemap field_ident
        in
        return new_typemap
  in
  let field_typemap = Type_map.of_record_fields record_field_types in
  let* remaining_required_fields =
    fold record_bindings ~init:field_typemap ~f:check_each_binding
  in
  match Map.to_alist remaining_required_fields with
  | [] -> return record_type
  | (field_name, _) :: _ ->
      error
        (Error_missing_record_fields
           { record_type; field_name = StellaIdent field_name })

and check_known_type_record record_field_types record_bindings =
  let expected_record_type = TypeRecord record_field_types in
  let* actual_record_type = check_unknown_record record_bindings in
  let* is_ok =
    is_subtype ~subtype:actual_record_type ~supertype:expected_record_type
  in
  if is_ok then return actual_record_type
  else (
    Logs.debug (fun m ->
        m
          "record types do not match expected=%s actual=%s, entering \
           diagnostics"
          (pp_type expected_record_type)
          (pp_type actual_record_type));
    diagnose_strict_record_type_mismatch record_field_types record_bindings)

and check_unknown_record record_bindings =
  let check_each_binding (ABinding (ident, expr)) =
    let* expr_t = check_expr None expr in
    return (ARecordFieldType (ident, expr_t))
  in
  let* record_field_types = many record_bindings ~f:check_each_binding in
  return (TypeRecord record_field_types)

(* - List - *)

and check_list expected_type elements =
  let check_each_element expected_element_type el_expr =
    check_expr expected_element_type el_expr *> return ()
  in
  match expected_type with
  | Some (TypeList element_type as list_type) ->
      many_unit elements ~f:(check_each_element (Some element_type))
      *> return list_type
  | Some TypeTop -> check_unknown_list elements *> return TypeTop
  | Some t -> error (Error_unexpected_list { expected = t })
  | None -> check_unknown_list elements

and check_unknown_list elements =
  match elements with
  | [] ->
      let* el_t = disambiguate_with_bottom Error_ambiguous_list in
      return (TypeList el_t)
  | el :: elements ->
      let* first_el_type = check_expr None el in
      check_list (Some (TypeList first_el_type)) elements

and check_unknown_cons_list head tail =
  let* head_t = check_expr None head in
  let list_t = TypeList head_t in
  check_expr (Some list_t) tail

and check_cons_list expected_type head tail =
  match expected_type with
  | Some (TypeList element_type as list_type) ->
      check_expr (Some element_type) head *> check_expr (Some list_type) tail
  | Some TypeTop -> check_unknown_cons_list head tail *> return TypeTop
  | Some t -> error (Error_unexpected_list { expected = t })
  | None -> check_unknown_cons_list head tail

and check_head expected_type expr =
  match expected_type with
  | Some t ->
      let list_t = TypeList t in
      check_expr (Some list_t) expr *> return t
  | None -> (
      let* expr_t = check_expr None expr in
      match expr_t with
      | TypeList el_t -> return el_t
      | t -> error (Error_not_a_list t))

and check_tail expected_type expr =
  let* expr_t = check_expr None expr in
  match expr_t with
  | TypeList _ as list_t -> expect_equal_type expected_type list_t
  | t -> error (Error_not_a_list t)

and check_is_empty expected_type expr =
  expect_equal_type expected_type TypeBool
  *>
  let* expr_t = check_expr None expr in
  match expr_t with
  | TypeList _ -> return TypeBool
  | TypeBottom -> return TypeBottom
  | _ -> error (Error_not_a_list expr_t)

(* - Sum types - *)

and check_unknown_injection make_sum_type operator =
  let* operator_t = check_expr None operator in
  let* other_t = disambiguate_with_bottom Error_ambiguous_sum_type in
  return (make_sum_type operator_t other_t)

and check_injection expected_type make_sum_type select_operator_t operator =
  match expected_type with
  | Some (TypeSum (l_t, r_t) as sum_t) ->
      let op_t = select_operator_t l_t r_t in
      check_expr (Some op_t) operator *> return sum_t
  | None -> check_unknown_injection make_sum_type operator
  | Some TypeTop ->
      check_unknown_injection make_sum_type operator *> return TypeTop
  | Some t -> error (Error_unexpected_injection { expected = t })

and check_inl expected_type operator =
  let select_left_type l_t _ = l_t in
  let make_sum_type op_t other_t = TypeSum (op_t, other_t) in
  check_injection expected_type make_sum_type select_left_type operator

and check_inr expected_type operator =
  let select_right_type _ r_t = r_t in
  let make_sum_type op_t other_t = TypeSum (other_t, op_t) in
  check_injection expected_type make_sum_type select_right_type operator

(* - Pattern Matching - *)

and check_match_not_implemented_patterns patterns =
  if does_match_have_not_implemented_patterns patterns then not_implemented ()
  else return ()

and check_match_exhaustiveness scrutinee_t cases =
  if List.length cases <> 0 then
    let pats = List.map cases ~f:(fun (AMatchCase (pat, _)) -> pat) in
    let is_exhaustive =
      match scrutinee_t with
      | TypeSum _ -> is_sum_match_exhaustive pats
      | TypeVariant t -> is_variant_match_exhaustive t pats
      | _ -> is_other_match_exhaustive pats
    in
    if not is_exhaustive then error Error_nonexhaustive_match_patterns
    else return ()
  else error Error_illegal_empty_matching

and check_match_branch expected_type varname var_t expr =
  with_new_binding varname var_t (check_expr expected_type expr)

and check_match_variant_branch expected_type field_name pattern_data field_types
    expr =
  let variant_t = Variant_type.make field_types in
  match Variant_type.lookup_field variant_t field_name with
  | None -> error Error_unexpected_pattern_for_type
  | Some typing -> (
      match (typing, pattern_data) with
      | NoTyping, NoPatternData -> check_expr expected_type expr
      | SomeTyping var_t, SomePatternData (PatternVar varname) ->
          check_match_branch expected_type varname var_t expr
      | SomeTyping _, _ -> not_implemented ()
      | _ -> error Error_unexpected_pattern_for_type)

and check_match_case expected_type scrutinee_t case =
  match case with
  | AMatchCase (PatternInl (PatternVar varname), expr) -> (
      match scrutinee_t with
      | TypeSum (var_t, _) ->
          check_match_branch expected_type varname var_t expr
      | _ -> error Error_unexpected_pattern_for_type)
  | AMatchCase (PatternInr (PatternVar varname), expr) -> (
      match scrutinee_t with
      | TypeSum (_, var_t) ->
          check_match_branch expected_type varname var_t expr
      | _ -> error Error_unexpected_pattern_for_type)
  | AMatchCase (PatternVar varname, expr) ->
      check_match_branch expected_type varname scrutinee_t expr
  | AMatchCase (PatternVariant (field_name, pattern_data), expr) -> (
      match scrutinee_t with
      | TypeVariant field_types ->
          check_match_variant_branch expected_type field_name pattern_data
            field_types expr
      | _ -> error Error_unexpected_pattern_for_type)
  | _ -> not_implemented ()

and check_match expected_type scrutinee cases =
  match cases with
  | [] -> error Error_illegal_empty_matching
  | first_case :: rest_cases ->
      let* scrutinee_t = check_expr None scrutinee in
      let* case_t = check_match_case expected_type scrutinee_t first_case in
      let expected_type' =
        match expected_type with None -> case_t | Some t -> t
      in
      many rest_cases ~f:(check_match_case (Some expected_type') scrutinee_t)
      *> check_match_exhaustiveness scrutinee_t cases
      *> return case_t

and check_not_implemented_patterns cases =
  let check_each_case = function
    | AMatchCase (PatternInl _, _) | AMatchCase (PatternInr _, _) -> return ()
    | _ -> not_implemented ()
  in
  many_unit cases ~f:check_each_case

(* - Recursion - *)

and check_fix expected_type expr =
  let* expr_t = check_expr None expr in
  match expr_t with
  | TypeFun (param_ts, ret_t) ->
      if List.length param_ts <> 1 then
        error Error_incorrect_number_of_arguments
      else
        let needed_fun_t = TypeFun ([ ret_t ], ret_t) in
        (* Fake-check the expression once again to require that it has the correct type *)
        expect_equal_type (Some needed_fun_t) expr_t
        *>
        let result_type = ret_t in
        expect_equal_type expected_type result_type *> return result_type
  | _ -> error Error_not_a_function

and check_nat_rec expected_type n z s =
  check_param (Some TypeNat) n
  *> let* t = check_param expected_type z in
     let expected_s_t = TypeFun ([ TypeNat ], TypeFun ([ t ], t)) in
     check_expr (Some expected_s_t) s *> return t

(* - Errors - *)

and check_panic expected_type =
  match expected_type with
  | None -> disambiguate_with_bottom Error_ambiguous_panic_type
  | Some t -> return t

and check_throw expected_type expr =
  let* { exception_type; _ } = get in
  match exception_type with
  | None -> error Error_exception_type_not_declared
  | Some exn_t -> (
      match expected_type with
      | None -> disambiguate_with_bottom Error_ambiguous_throw_type
      | Some result_t -> check_expr (Some exn_t) expr *> return result_t)

and check_try_with expected_type try_expr with_expr =
  let* expr_t = check_expr expected_type try_expr in
  check_expr (Some expr_t) with_expr

and check_try_catch expected_type try_expr pat catch_expr =
  let* { exception_type; _ } = get in
  match exception_type with
  | None -> error Error_exception_type_not_declared
  | Some exn_t ->
      let* expr_t = check_expr expected_type try_expr in
      let case = AMatchCase (pat, catch_expr) in
      check_match_case (Some expr_t) exn_t case

(* - References - *)

and check_ref expected_type expr =
  let* expr_t = check_expr None expr in
  match expected_type with
  | None -> return (TypeRef expr_t)
  | Some (TypeRef arg_t) ->
      expect_equal_type (Some arg_t) expr_t *> return (TypeRef expr_t)
  | Some TypeTop -> return TypeTop
  | Some t -> error (Error_unexpected_reference t)

and check_deref expected_type expr =
  let* expr_t = check_expr None expr in
  match expr_t with
  | TypeRef t -> expect_equal_type expected_type t
  | t -> error (Error_not_a_reference t)

and check_const_memory expected_type =
  match expected_type with
  | Some (TypeRef t) -> return (TypeRef t)
  | Some TypeTop -> return TypeTop
  | Some t -> error (Error_unexpected_memory_address t)
  | None ->
      let* el_t = disambiguate_with_bottom Error_ambiguous_reference_type in
      return (TypeRef el_t)

and check_assign expected_type l r =
  expect_equal_type expected_type TypeUnit
  *>
  let* l_t = check_expr None l in
  (match l_t with
  | TypeRef l_t -> check_expr (Some l_t) r
  | t -> error (Error_not_a_reference t))
  *> return TypeUnit

(* - Basic operators - *)

and check_simple_unary_op expected_type ~op_t ~return_t operand_expr =
  expect_equal_type expected_type return_t
  *> check_expr (Some op_t) operand_expr
  *> return return_t

and check_nat_unary_op expected_type operand_expr return_t =
  check_simple_unary_op expected_type ~op_t:TypeNat ~return_t operand_expr

and check_simple_binary_op expected_type ~left_t ~right_t ~return_t left_expr
    right_expr =
  expect_equal_type expected_type return_t
  *> check_expr (Some left_t) left_expr
  *> check_expr (Some right_t) right_expr
  *> return return_t

and check_nat_arithmetic_op expected_type left right =
  check_simple_binary_op expected_type ~left_t:TypeNat ~right_t:TypeNat
    ~return_t:TypeNat left right

and check_nat_comparison_op expected_type left right =
  check_simple_binary_op expected_type ~left_t:TypeNat ~right_t:TypeNat
    ~return_t:TypeBool left right

and check_logic_op expected_type left right =
  check_simple_binary_op expected_type ~left_t:TypeBool ~right_t:TypeBool
    ~return_t:TypeBool left right

and check_equality expected_type left right =
  expect_equal_type expected_type TypeBool
  *> let* left_t = check_expr None left in
     check_expr (Some left_t) right *> return TypeBool

(* - Sequencing - *)

and check_sequence expected_type expr1 expr2 =
  check_expr (Some TypeUnit) expr1 *> check_expr expected_type expr2

(* Variant *)

and check_variant expected_type ident data =
  match expected_type with
  | None -> error Error_ambiguous_variant
  | Some (TypeVariant variant_field_types as expected_type) -> (
      let variant_type = Variant_type.make variant_field_types in
      match Variant_type.lookup_field variant_type ident with
      | None ->
          let err = Error_unexpected_variant_label (expected_type, ident) in
          error err
      | Some expected_typing ->
          check_expr_data expected_typing data *> return expected_type)
  | Some _ -> error Error_unexpected_variant

and check_expr_data expected_typing expr_data =
  match (expected_typing, expr_data) with
  | SomeTyping expected_t, SomeExprData e ->
      let* t = check_expr (Some expected_t) e in
      return (SomeTyping t)
  | NoTyping, NoExprData -> return NoTyping
  | _ -> error Error_unknown

(* - Main visitor - *)

and check_param expected_type expr =
  let* expr_t = check_expr None expr in
  match expected_type with
  | None -> return expr_t
  | Some expected_type ->
      let* are_comp = is_subtype ~supertype:expected_type ~subtype:expr_t in
      if are_comp then return expr_t
      else
        error
          (Error_unexpected_type_for_parameter
             { expected = expected_type; actual = expr_t })

and check_expr expected_type expr =
  in_expr_error_context (printTree prtExpr expr) expected_type
  @@
  match expr with
  | Sequence (expr1, expr2) -> check_sequence expected_type expr1 expr2
  | Application (callee, args) -> check_application expected_type callee args
  | Abstraction (params, body) -> check_abstraction expected_type params body
  | ConstUnit -> expect_equal_type expected_type TypeUnit
  | If (cond, then_expr, else_expr) ->
      check_if expected_type cond then_expr else_expr
  | TypeAsc (expr, typeT) ->
      expect_equal_type expected_type typeT *> check_expr (Some typeT) expr
  | Equal (l, r) | NotEqual (l, r) -> check_equality expected_type l r
  (* Bools *)
  | ConstFalse | ConstTrue -> expect_equal_type expected_type TypeBool
  | LogicAnd (l, r) | LogicOr (l, r) -> check_logic_op expected_type l r
  | LogicNot e ->
      check_simple_unary_op expected_type ~op_t:TypeBool ~return_t:TypeBool e
  (* Nats *)
  | ConstInt _ -> expect_equal_type expected_type TypeNat
  | IsZero expr -> check_nat_unary_op expected_type expr TypeBool
  | Pred expr | Succ expr -> check_nat_unary_op expected_type expr TypeNat
  | Multiply (l, r) | Divide (l, r) | Add (l, r) | Subtract (l, r) ->
      check_nat_arithmetic_op expected_type l r
  | LessThan (l, r)
  | LessThanOrEqual (l, r)
  | GreaterThan (l, r)
  | GreaterThanOrEqual (l, r) ->
      check_nat_comparison_op expected_type l r
  (* Bindings *)
  | Let (pattern_bindings, body_expr) ->
      check_let expected_type pattern_bindings body_expr
  | Var ident ->
      let* type' = get_var_type ident in
      expect_equal_type expected_type type'
  (* Tuples *)
  | DotTuple (expr, index) -> check_dot_tuple expected_type expr index
  | Tuple exprs -> check_tuple expected_type exprs
  (* Records *)
  | DotRecord (record_expr, field_ident) ->
      check_dot_record expected_type record_expr field_ident
  | Record bindings -> check_record expected_type bindings
  (* Lists *)
  | List elements -> check_list expected_type elements
  | ConsList (head, tail) -> check_cons_list expected_type head tail
  | Head expr -> check_head expected_type expr
  | Tail expr -> check_tail expected_type expr
  | IsEmpty expr -> check_is_empty expected_type expr
  (* Sum Types *)
  | Inl expr -> check_inl expected_type expr
  | Inr expr -> check_inr expected_type expr
  (* Pattern matching *)
  | Match (scrutinee, cases) -> check_match expected_type scrutinee cases
  (* Recursion *)
  | Fix expr -> check_fix expected_type expr
  | NatRec (n, z, s) -> check_nat_rec expected_type n z s
  (* Errors *)
  | Panic -> check_panic expected_type
  | Throw e -> check_throw expected_type e
  | TryWith (try_expr, with_expr) ->
      check_try_with expected_type try_expr with_expr
  | TryCatch (try_expr, pat, catch_expr) ->
      check_try_catch expected_type try_expr pat catch_expr
  (* References *)
  | Ref expr -> check_ref expected_type expr
  | Deref expr -> check_deref expected_type expr
  | ConstMemory _ -> check_const_memory expected_type
  | Assign (l, r) -> check_assign expected_type l r
  (* Variants *)
  | Variant (ident, data) -> check_variant expected_type ident data
  | _ -> expr_not_implemented expr

and check_exprs (type_expr_pairs : (typeT * expr) list) : unit t =
  let check_expr (expected_type, expr) =
    check_expr (Some expected_type) expr *> return ()
  in
  many_unit type_expr_pairs ~f:check_expr

(* --- Declarations --- *)

and check_fun_decl params ret_type body_decls return_expr : unit t =
  let in_return_error_context f =
    let expr_str = printTree prtExpr return_expr in
    let context = Printf.sprintf "return %s" expr_str in
    in_error_context context f
  in
  match body_decls with
  | _ :: _ -> not_implemented ()
  | [] ->
      in_return_error_context
      @@
      let ret_type = Some (type_of_returnType ret_type) in
      with_param_bindings params
      @@ (check_expr ret_type return_expr *> return ())

and check_decl decl : unit t =
  in_error_context (printTree prtDecl decl)
  @@
  match decl with
  | DeclFun (_, _, params, ret_type, _, body_decls, return_expr) ->
      check_fun_decl params ret_type body_decls return_expr
  | DeclExceptionType _ -> return ()
  | _ -> not_implemented ()

(* ----- Runner ------ *)

let extract_fun_type params return_type =
  let param_types = List.map params ~f:(fun (AParamDecl (_, t)) -> t) in
  let return_type =
    match return_type with SomeReturnType t -> t | NoReturnType -> TypeUnit
  in
  TypeFun (param_types, return_type)

let make_globals_type_map (AProgram (_, _, decls)) =
  let pairs =
    List.filter_map decls ~f:(function
      | DeclFun (_, ident, params, ret_typ, _, _, _) ->
          Some (ident, extract_fun_type params ret_typ)
      | _ -> None)
  in
  Type_map.of_globals pairs

let check_program (AProgram (_, _, decls) as prog) : unit pass_result =
  let global_type_map = make_globals_type_map prog in
  let exception_type = collect_exception_type prog in
  let exts = Extentions.get_extentions prog in
  let type_model = choose_type_model exts in
  let init =
    {
      stacktrace = [];
      typemap = global_type_map;
      exception_type;
      type_model;
      ambiguous_as_bottom = Extentions.is_bottom_disambiguation_enabled exts;
      is_subtyping_enabled = Extentions.is_structural_subtyping_enabled exts;
    }
  in
  let pass = many_unit decls ~f:check_decl in
  run_pass ~init pass
