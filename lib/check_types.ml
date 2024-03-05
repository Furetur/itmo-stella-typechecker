open Base
open Stella_parser.Parsetree
open Stella_parser.Pretty_print_tree
open Utils
open Errors

let ( $ ) f x = f x

(* --- Pass Setup --- *)

type state = { stacktrace : string list }

module ThisPass = Passes.SingleError (struct
  type pass_state = state
  type pass_error = error
end)

open ThisPass

(* --- Errors --- *)

let in_error_context context f =
  let* old_state = get in
  set { stacktrace = context :: old_state.stacktrace }
  *>
  let* result = f in
  set old_state *> return result

let error kind =
  let* { stacktrace; _ } = get in
  fail { kind; stacktrace }

let zip_or_error error_kind list1 list2 =
  match List.zip list1 list2 with
  | Unequal_lengths -> error error_kind
  | Ok zipped -> return zipped

(* --- State --- *)

let get_var_type typemap ident =
  match Type_map.get_type typemap ident with
  | Some t -> return t
  | None -> error (Error_undefined_variable ident)

let expect_equal_type expected_type actual_type =
  match expected_type with
  | None -> return actual_type
  | Some expected_type ->
      if Stdlib.( == ) expected_type actual_type then return actual_type
      else
        error
          (Error_unexpected_type_for_expression
             { expected = expected_type; actual = actual_type })

(* ----- Pass ----- *)

(* --- Expressions --- *)

let rec check_application typemap expected_type callee args =
  let check_arg_types expected_arg_types args =
    let* type_arg_pairs =
      zip_or_error Error_incorrect_number_of_arguments expected_arg_types args
    in
    check_exprs typemap type_arg_pairs
  in
  let* callee_type = check_expr typemap None callee in
  match callee_type with
  | TypeFun (arg_types, ret_type) ->
      expect_equal_type expected_type ret_type
      *> check_arg_types arg_types args
      *> return ret_type
  | _ -> error Error_not_a_function

and check_if typemap expected_type cond then_expr else_expr =
  check_expr typemap (Some TypeBool) cond
  *>
  let* then_type = check_expr typemap None then_expr in
  check_expr typemap (Some then_type) else_expr
  *> expect_equal_type expected_type then_type

and check_let typemap expected_type pattern_bindings body_expr =
  let check_each_binding_and_add_to_typemap typemap
      (APatternBinding (pattern, expr)) =
    match pattern with
    | PatternVar ident ->
        let* expr_type = check_expr typemap None expr in
        return (Type_map.set_type typemap ident expr_type)
    | _ -> not_implemented ()
  in
  let* new_typemap =
    fold pattern_bindings ~init:typemap ~f:check_each_binding_and_add_to_typemap
  in
  check_expr new_typemap expected_type body_expr

(* - Tuple - *)

and check_dot_tuple typemap expected_type expr index =
  let* expr_type = check_expr typemap None expr in
  match expr_type with
  | TypeTuple component_types -> (
      match List.nth component_types (index - 1) with
      | None ->
          error
            (Error_tuple_index_out_of_bounds
               { max_index = List.length component_types; actual_index = index })
      | Some component_type -> expect_equal_type expected_type component_type)
  | _ -> error (Error_not_a_tuple expr_type)

and check_tuple typemap expected_type (exprs : expr list) =
  match expected_type with
  | Some (TypeTuple component_types) ->
      let wrong_length_error =
        Error_unexpected_tuple_length { expected = List.length component_types }
      in
      let* type_expr_pairs =
        zip_or_error wrong_length_error component_types exprs
      in
      check_exprs typemap type_expr_pairs *> return (TypeTuple component_types)
  | Some expected_type -> error (Error_unexpected_tuple { expected_type })
  | None ->
      let* actual_component_types = many exprs ~f:(check_expr typemap None) in
      return (TypeTuple actual_component_types)

(* - Record - *)

and check_dot_record typemap expected_type record_expr field_ident =
  let* record_type = check_expr typemap None record_expr in
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

and check_record typemap expected_type bindings =
  match expected_type with
  | Some (TypeRecord field_types) ->
      check_known_type_record typemap field_types bindings
  | Some t -> error (Error_not_a_record t)
  | None -> check_unknown_record typemap bindings

and check_known_type_record typemap record_field_types record_bindings =
  let record_type = TypeRecord record_field_types in
  let check_each_binding remaining_fields_typemap (ABinding (field_ident, expr))
      =
    match Type_map.get_type remaining_fields_typemap field_ident with
    | None ->
        error
          (Error_unexpected_record_fields
             { record_type; field_name = field_ident })
    | Some field_t ->
        check_expr typemap (Some field_t) expr
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

and check_unknown_record typemap record_bindings =
  let check_each_binding (ABinding (ident, expr)) =
    let* expr_t = check_expr typemap None expr in
    return (ARecordFieldType (ident, expr_t))
  in
  let* record_field_types = many record_bindings ~f:check_each_binding in
  return (TypeRecord record_field_types)

(* - List - *)

and check_list typemap expected_type elements =
  let check_each_element expected_element_type el_expr =
    check_expr typemap expected_element_type el_expr *> return ()
  in
  match expected_type with
  | Some (TypeList element_type as list_type) ->
      many_unit elements ~f:(check_each_element (Some element_type))
      *> return list_type
  | Some t -> error (Error_unexpected_list { expected = t })
  | None -> check_unknown_list typemap elements

and check_unknown_list typemap elements =
  match elements with
  | [] -> error Error_ambiguous_list
  | el :: elements ->
      let* first_el_type = check_expr typemap None el in
      check_list typemap (Some first_el_type) elements

and check_cons_list typemap expected_type head tail =
  match expected_type with
  | Some (TypeList element_type as list_type) ->
      check_expr typemap (Some element_type) head
      *> check_expr typemap (Some list_type) tail
  | Some t -> error (Error_unexpected_list { expected = t })
  | None ->
      let* head_t = check_expr typemap None head in
      let list_t = TypeList head_t in
      check_expr typemap (Some list_t) tail

and check_head typemap expected_type expr =
  match expected_type with
  | Some t ->
      let list_t = TypeList t in
      check_expr typemap (Some list_t) expr *> return t
  | None -> (
      let* expr_t = check_expr typemap None expr in
      match expr_t with
      | TypeList el_t -> return el_t
      | t -> error (Error_not_a_list t))

and check_tail typemap expected_type expr =
  check_expr typemap expected_type expr

and check_is_empty typemap expected_type expr =
  match expected_type with
  | None | Some TypeBool -> (
      let* expr_t = check_expr typemap None expr in
      match expr_t with
      | TypeList _ -> return TypeBool
      | _ -> error (Error_not_a_list expr_t))
  | Some t ->
      error
        (Error_unexpected_type_for_expression
           { expected = t; actual = TypeBool })

(* Sum types *)

and check_injection typemap expected_type operator_type operator =
  match expected_type with
  | Some (TypeSum (l_t, r_t) as sum_t) ->
      let op_t = operator_type l_t r_t in
      check_expr typemap (Some op_t) operator *> return sum_t
  | None -> error Error_ambiguous_sum_type
  | Some t -> error (Error_unexpected_injection { expected = t })

and check_inl typemap expected_type operator =
  let select_left_type l_t _ = l_t in
  check_injection typemap expected_type select_left_type operator

and check_inr typemap expected_type operator =
  let select_right_type _ r_t = r_t in
  check_injection typemap expected_type select_right_type operator

(* - Basic operators - *)

and check_simple_unary_op typemap expected_type ~op_t ~return_t operand_expr =
  expect_equal_type expected_type return_t
  *> check_expr typemap (Some op_t) operand_expr
  *> return return_t

and check_nat_unary_op typemap expected_type operand_expr return_t =
  check_simple_unary_op typemap expected_type ~op_t:TypeNat ~return_t
    operand_expr

and check_simple_binary_op typemap expected_type ~left_t ~right_t ~return_t
    left_expr right_expr =
  expect_equal_type expected_type return_t
  *> check_expr typemap (Some left_t) left_expr
  *> check_expr typemap (Some right_t) right_expr
  *> return return_t

and check_nat_arithmetic_op typemap expected_type left right =
  check_simple_binary_op typemap expected_type ~left_t:TypeNat ~right_t:TypeNat
    ~return_t:TypeNat left right

and check_nat_comparison_op typemap expected_type left right =
  check_simple_binary_op typemap expected_type ~left_t:TypeNat ~right_t:TypeNat
    ~return_t:TypeBool left right

and check_logic_op typemap expected_type left right =
  check_simple_binary_op typemap expected_type ~left_t:TypeBool
    ~right_t:TypeBool ~return_t:TypeBool left right

(* - Main visitor - *)

and check_expr typemap expected_type expr =
  in_error_context (printTree prtExpr expr)
  $
  match expr with
  | Application (callee, args) ->
      check_application typemap expected_type callee args
  | ConstUnit -> expect_equal_type expected_type TypeUnit
  | If (cond, then_expr, else_expr) ->
      check_if typemap expected_type cond then_expr else_expr
  (* Bools *)
  | ConstFalse | ConstTrue -> expect_equal_type expected_type TypeBool
  | LogicAnd (l, r) | LogicOr (l, r) -> check_logic_op typemap expected_type l r
  | LogicNot e ->
      check_simple_unary_op typemap expected_type ~op_t:TypeBool
        ~return_t:TypeBool e
  (* Nats *)
  | ConstInt _ -> expect_equal_type expected_type TypeNat
  | IsZero expr -> check_nat_unary_op typemap expected_type expr TypeBool
  | Pred expr | Succ expr ->
      check_nat_unary_op typemap expected_type expr TypeNat
  | Multiply (l, r) | Divide (l, r) | Add (l, r) | Subtract (l, r) ->
      check_nat_arithmetic_op typemap expected_type l r
  | LessThan (l, r)
  | LessThanOrEqual (l, r)
  | GreaterThan (l, r)
  | GreaterThanOrEqual (l, r) ->
      check_nat_comparison_op typemap expected_type l r
  (* Bindings *)
  | Let (pattern_bindings, body_expr) ->
      check_let typemap expected_type pattern_bindings body_expr
  | Var ident ->
      let* type' = get_var_type typemap ident in
      expect_equal_type expected_type type'
  (* Tuples *)
  | DotTuple (expr, index) -> check_dot_tuple typemap expected_type expr index
  | Tuple exprs -> check_tuple typemap expected_type exprs
  (* Records *)
  | DotRecord (record_expr, field_ident) ->
      check_dot_record typemap expected_type record_expr field_ident
  | Record bindings -> check_record typemap expected_type bindings
  (* Lists *)
  | List elements -> check_list typemap expected_type elements
  | ConsList (head, tail) -> check_cons_list typemap expected_type head tail
  | Head expr -> check_head typemap expected_type expr
  | Tail expr -> check_tail typemap expected_type expr
  | IsEmpty expr -> check_is_empty typemap expected_type expr
  (* Sum Types *)
  | Inl expr -> check_inl typemap expected_type expr
  | Inr expr -> check_inr typemap expected_type expr
  | _ -> expr_not_implemented expr

and check_exprs typemap (type_expr_pairs : (typeT * expr) list) : unit t =
  let check_expr (expected_type, expr) =
    check_expr typemap (Some expected_type) expr *> return ()
  in
  many_unit type_expr_pairs ~f:check_expr

(* --- Declarations --- *)

and check_fun_decl typemap params ret_type body_decls return_expr : unit t =
  let in_return_error_context f =
    let expr_str = printTree prtExpr return_expr in
    let context = Printf.sprintf "return %s" expr_str in
    in_error_context context f
  in
  let typemap' = Type_map.add_params typemap params in
  match body_decls with
  | _ :: _ -> not_implemented ()
  | [] ->
      in_return_error_context
      $
      let ret_type = Some (type_of_returnType ret_type) in
      check_expr typemap' ret_type return_expr *> return ()

and check_decl typemap decl : unit t =
  in_error_context (printTree prtDecl decl)
  $
  match decl with
  | DeclFun (_, _, params, ret_type, _, body_decls, return_expr) ->
      check_fun_decl typemap params ret_type body_decls return_expr
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
  let init = { stacktrace = [] } in
  let pass = many_unit decls ~f:(check_decl global_type_map) in
  run_pass ~init pass
