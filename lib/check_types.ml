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
  let check_each_arg (expected_arg_typ, arg) =
    check_expr typemap (Some expected_arg_typ) arg *> return ()
  in
  let check_arg_types expected_arg_types args =
    match List.zip expected_arg_types args with
    | Unequal_lengths -> error Error_incorrect_number_of_arguments
    | Ok type_arg_pairs -> many_unit type_arg_pairs ~f:check_each_arg
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

and check_expr typemap expected_type expr =
  in_error_context (printTree prtExpr expr)
  $
  match expr with
  | Application (callee, args) ->
      check_application typemap expected_type callee args
  | ConstFalse | ConstTrue -> expect_equal_type expected_type TypeBool
  | ConstUnit -> expect_equal_type expected_type TypeUnit
  | ConstInt _ -> expect_equal_type expected_type TypeNat
  | Pred expr | Succ expr ->
      expect_equal_type expected_type TypeNat
      *> check_expr typemap (Some TypeNat) expr
  | If (cond, then_expr, else_expr) ->
      check_if typemap expected_type cond then_expr else_expr
  | _ -> not_implemented ()

(* --- Declarations --- *)

and check_fun_decl typemap params ret_type body_decls return_expr : unit t =
  let typemap' = Type_map.add_params typemap params in
  match body_decls with
  | _ :: _ -> not_implemented ()
  | [] ->
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

let check_program (AProgram (_, _, decls) as prog): unit pass_result =
  let global_type_map = make_globals_type_map prog in
  let init = { stacktrace = [] } in
  let pass = many_unit decls ~f:(check_decl global_type_map) in
  run_pass ~init pass
