open Base
open Stella_parser.Parsetree
open Stella_parser.Pretty_print_tree
open Utils
open Pattern_matching_utils
open Errors
open Typecheck_pass
open ThisPass

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
  let n_args = List.length args in
  let* arg_types, ret_type =
    expect_a_function callee_type n_args Error_not_a_function
      Error_incorrect_number_of_arguments
  in
  expect_equal_type expected_type ret_type
  *> check_arg_types arg_types args
  *> return ret_type

and check_abstraction expected_type param_decls body_expr =
  Logs.debug (fun m ->
      m "check_abstraction: enter, expected_type=%s"
        (pp_expected_type expected_type));
  let* actual_t = check_unknown_abstraction param_decls body_expr in
  expect_equal_type expected_type actual_t

and check_unknown_abstraction param_decls body_expr =
  let actual_param_types =
    List.map param_decls ~f:(fun (AParamDecl (_, t)) -> t)
  in
  let* ret_t = check_abstraction_body None param_decls body_expr in
  return (TypeFun (actual_param_types, ret_t))

and check_abstraction_body expected_type param_decls body_expr =
  with_param_bindings param_decls (fun _ -> check_expr expected_type body_expr)

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
  with_new_bindings ident_type_pairs (fun _ ->
      check_expr expected_type body_expr)

(* - Tuple - *)

and check_dot_tuple expected_type expr index =
  if index <> 1 && index <> 2 then
    Printf.failwithf "Tuples of length %d not supported" index ()
  else
    let* expr_type = check_expr None expr in
    let* element_type = expect_a_tuple_with_element expr_type index in
    expect_equal_type expected_type element_type

and check_tuple expected_type (exprs : expr list) =
  let length = List.length exprs in
  if length <> 2 then
    Printf.failwithf "Tuples of length %d not supported" length ()
  else
    match expected_type with
    | Some expected_type ->
        let* component_types =
          expect_a_tuple_of_length expected_type length (fun _ ->
              Error_unexpected_tuple { expected_type })
        in
        let type_expr_pairs = List.zip_exn component_types exprs in
        check_exprs type_expr_pairs *> return (TypeTuple component_types)
    | None ->
        let* actual_component_types = many exprs ~f:(check_expr None) in
        return (TypeTuple actual_component_types)

(* - Record - *)

and check_dot_record expected_type record_expr field_ident =
  let* record_type = check_expr None record_expr in
  let* field_type = expect_a_record_with_field record_type field_ident in
  expect_equal_type expected_type field_type

and check_record expected_type bindings =
  let* actual_field_types = check_unknown_record bindings in
  let record_t = TypeRecord actual_field_types in
  let* _ = expect_equal_type expected_type record_t in
  return record_t

and check_unknown_record record_bindings =
  let check_each_binding (ABinding (ident, expr)) =
    let* expr_t = check_expr None expr in
    return (ARecordFieldType (ident, expr_t))
  in
  let* record_field_types = many record_bindings ~f:check_each_binding in
  return record_field_types

(* - List - *)

and check_list expected_type elements =
  let check_each_element expected_element_type el_expr =
    check_expr expected_element_type el_expr *> return ()
  in
  let* el_t = new_typevar in
  many_unit elements ~f:(check_each_element (Some el_t))
  *>
  let actual_list_t = TypeList el_t in
  expect_equal_type expected_type actual_list_t

and check_unknown_list elements =
  match elements with
  | [] ->
      let* typevar = new_typevar in
      return @@ TypeList typevar
  | el :: elements ->
      let* first_el_type = check_expr None el in
      check_list (Some (TypeList first_el_type)) elements

and check_cons_list expected_type head tail =
  let* el_t = check_expr None head in
  let list_t = TypeList el_t in
  expect_equal_type expected_type list_t *> check_expr (Some list_t) tail

and check_head expected_type expr =
  match expected_type with
  | Some t ->
      let list_t = TypeList t in
      check_expr (Some list_t) expr *> return t
  | None ->
      let* expr_t = check_expr None expr in
      let* el_t = expect_a_list expr_t (Error_not_a_list expr_t) in
      return el_t

and check_tail expected_type expr =
  let* expr_t = check_expr None expr in
  let* element_type = expect_a_list expr_t (Error_not_a_list expr_t) in
  let list_t = TypeList element_type in
  expect_equal_type expected_type list_t

and check_is_empty expected_type expr =
  expect_equal_type expected_type TypeBool
  *>
  let* expr_t = check_expr None expr in
  let* _ = expect_a_list expr_t (Error_not_a_list expr_t) in
  return TypeBool

(* - Sum types - *)

and check_injection expected_type create_sum_t operator =
  let* typevar = new_typevar in
  let* operator_t = check_expr None operator in
  let actual_sum_t = create_sum_t operator_t typevar in
  expect_equal_type expected_type actual_sum_t

and check_inl expected_type operator =
  let make_sum_of_inl l_t r_t = TypeSum (l_t, r_t) in
  check_injection expected_type make_sum_of_inl operator

and check_inr expected_type operator =
  let make_sum_of_inr r_t l_t = TypeSum (l_t, r_t) in
  check_injection expected_type make_sum_of_inr operator

(* - Pattern Matching - *)

and check_match_not_implemented_patterns patterns =
  if does_match_have_not_implemented_patterns patterns then not_implemented ()
  else return ()

and check_match_exhaustiveness cases =
  if List.length cases <> 0 then
    let pats = List.map cases ~f:(fun (AMatchCase (pat, _)) -> pat) in
    let is_exhaustive = is_match_exhaustive pats in
    if not is_exhaustive then error Error_nonexhaustive_match_patterns
    else return ()
  else error Error_illegal_empty_matching

and check_match_branch expected_type varname var_t expr =
  with_new_binding varname var_t (fun _ -> check_expr expected_type expr)

and check_match_case expected_type scrutinee_t case =
  match case with
  | AMatchCase (PatternInl (PatternVar varname), expr) ->
      let* var_t, _ =
        expect_a_sum scrutinee_t Error_unexpected_pattern_for_type
      in
      check_match_branch expected_type varname var_t expr
  | AMatchCase (PatternInr (PatternVar varname), expr) ->
      let* _, var_t =
        expect_a_sum scrutinee_t Error_unexpected_pattern_for_type
      in
      check_match_branch expected_type varname var_t expr
  | AMatchCase (PatternVar varname, expr) ->
      check_match_branch expected_type varname scrutinee_t expr
  | _ -> not_implemented ()

and check_match expected_type scrutinee cases =
  match cases with
  | [] -> error Error_illegal_empty_matching
  | first_case :: rest_cases ->
      let* scrutinee_t = check_expr None scrutinee in
      let* case_t = check_match_case expected_type scrutinee_t first_case in
      many rest_cases ~f:(check_match_case (Some case_t) scrutinee_t)
      *> check_match_exhaustiveness cases
      *> return case_t

and check_not_implemented_patterns cases =
  let check_each_case = function
    | AMatchCase (PatternInl _, _) | AMatchCase (PatternInr _, _) -> return ()
    | _ -> not_implemented ()
  in
  many_unit cases ~f:check_each_case

(* - Recursion - *)

and check_fix expected_type expr =
  let* result_t = new_typevar in
  expect_equal_type expected_type result_t
  *>
  let expected_func_t = TypeFun ([ result_t ], result_t) in
  check_expr (Some expected_func_t) expr

and check_nat_rec expected_type n z s =
  check_expr (Some TypeNat) n
  *> let* t = check_expr expected_type z in
     let expected_s_t = TypeFun ([ TypeNat ], TypeFun ([ t ], t)) in
     check_expr (Some expected_s_t) s *> return t

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

(* - References - *)

and check_ref expected_type expr =
  let* expr_t = check_expr None expr in
  let ref_t = TypeRef expr_t in
  expect_equal_type expected_type ref_t

and check_deref expected_type expr =
  let* arg_t = new_typevar in
  let ref_t = TypeRef arg_t in
  let* _ = expect_equal_type expected_type arg_t in
  let* _ = check_expr (Some ref_t) expr in
  return arg_t

and check_const_memory expected_type =
  let* arg_t = new_typevar in
  let ref_t = TypeRef arg_t in
  let* _ = expect_equal_type expected_type ref_t in
  return ref_t

and check_assign expected_type l r =
  let* _ = expect_equal_type expected_type TypeUnit in
  let* r_t = check_expr None r in
  let ref_t = TypeRef r_t in
  let* _ = check_expr (Some ref_t) l in
  return TypeUnit

(* - Exceptions - *)

and check_panic expected_type =
  let* t = new_typevar in
  let* _ = expect_equal_type expected_type t in
  return t

and check_throw expected_type expr =
  let* { exception_type; _ } = get in
  match exception_type with
  | None -> error Error_exception_type_not_declared
  | Some exception_type ->
      let* _ = check_expr (Some exception_type) expr in
      let* bottom_t = new_typevar in
      let* _ = expect_equal_type expected_type bottom_t in
      return bottom_t

and check_try_with expected_type try_expr with_expr =
  let* t = check_expr expected_type try_expr in
  let* _ = check_expr expected_type with_expr in
  return t

and check_try_catch expected_type try_expr pat catch_expr =
  let* { exception_type; _ } = get in
  match exception_type with
  | None -> error Error_exception_type_not_declared
  | Some exception_type ->
      let* result_t = check_expr expected_type try_expr in
      let case = AMatchCase (pat, catch_expr) in
      let* _ = check_match_case expected_type exception_type case in
      return result_t

(* - Main visitor - *)

and check_expr expected_type expr =
  in_expr_error_context (printTree prtExpr expr) expected_type @@ fun _ ->
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
  (* References *)
  | Ref expr -> check_ref expected_type expr
  | Deref expr -> check_deref expected_type expr
  | ConstMemory _ -> check_const_memory expected_type
  | Assign (l, r) -> check_assign expected_type l r
  (* Exceptions *)
  | Panic -> check_panic expected_type
  | Throw expr -> check_throw expected_type expr
  | TryWith (try_expr, catch_expr) ->
      check_try_with expected_type try_expr catch_expr
  | TryCatch (try_expr, pat, catch_expr) ->
      check_try_catch expected_type try_expr pat catch_expr
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
      in_return_error_context @@ fun _ ->
      let ret_type = Some (type_of_returnType ret_type) in
      with_param_bindings params @@ fun _ ->
      check_expr ret_type return_expr *> return ()

and check_decl decl : unit t =
  in_error_context (printTree prtDecl decl) @@ fun _ ->
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

let check_prog (AProgram (_, _, decls)) =
  many_unit decls ~f:check_decl *> solve_constraints

let check_program prog : unit pass_result =
  let global_type_map = make_globals_type_map prog in
  let exception_type = collect_exception_type prog in
  let init =
    {
      stacktrace = [];
      typemap = global_type_map;
      next_typevar_id = 0;
      constraints = [];
      exception_type;
    }
  in
  run_pass ~init (check_prog prog)
