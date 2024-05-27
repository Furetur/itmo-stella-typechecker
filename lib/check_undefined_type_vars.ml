open Base
open Stella_parser.Parsetree
open Passes.Result_pass_syntax
open Universal_types
open Errors

let rec check_types typeenv types = many_unit types ~f:(check_type typeenv)

and check_type_var typeenv tv =
  match lookup_typevar typeenv tv with
  | Some _ -> return ()
  | None -> fail (Error_undefined_type_variable tv)

and check_type typeenv = function
  | TypeTuple ts -> check_types typeenv ts
  | TypeSum (a, b) -> check_types typeenv [ a; b ]
  | TypeList t | TypeRef t -> check_type typeenv t
  | TypeFun (args, ret) -> check_types typeenv args *> check_type typeenv ret
  | TypeForAll (tvs, t) ->
      let typeenv' = add_typevars typeenv tvs in
      check_type typeenv' t
  | TypeVar x -> check_type_var typeenv x
  | TypeRecord fields ->
      let types = List.map fields ~f:(fun (ARecordFieldType (_, t)) -> t) in
      check_types typeenv types
  | _ -> return ()

let rec check_exprs (typeenv : typeenv) (exprs : expr list) =
  many_unit exprs ~f:(check_expr typeenv)

and check_params typeenv params =
  let param_types = List.map params ~f:(fun (AParamDecl (_, t)) -> t) in
  check_types typeenv param_types

and check_expr (typeenv : typeenv) = function
  | Application (callee, args) -> check_exprs typeenv (callee :: args)
  | Abstraction (params, body) ->
      check_params typeenv params *> check_expr typeenv body
  | If (cond, then_expr, else_expr) ->
      check_exprs typeenv [ cond; then_expr; else_expr ]
  | Sequence (l, r)
  | Equal (l, r)
  | NotEqual (l, r)
  | Multiply (l, r)
  | Divide (l, r)
  | Add (l, r)
  | Subtract (l, r)
  | LessThan (l, r)
  | LessThanOrEqual (l, r)
  | GreaterThan (l, r)
  | GreaterThanOrEqual (l, r)
  | LogicAnd (l, r)
  | LogicOr (l, r)
  | Assign (l, r)
  | ConsList (l, r) ->
      check_exprs typeenv [ l; r ]
  | LogicNot expr
  | IsZero expr
  | Pred expr
  | Succ expr
  | Let (_, expr)
  | DotTuple (expr, _)
  | Inl expr
  | Inr expr
  | Fix expr
  | Ref expr
  | Head expr
  | Tail expr
  | IsEmpty expr
  | Deref expr
  | DotRecord (expr, _) ->
      check_expr typeenv expr
  | ConstInt _ | Var _ | ConstMemory _ | ConstUnit | ConstFalse | ConstTrue ->
      return ()
  | List exprs | Tuple exprs -> check_exprs typeenv exprs
  | Record bindings ->
      let exprs = List.map bindings ~f:(fun (ABinding (_, e)) -> e) in
      check_exprs typeenv exprs
  | Match (scrutinee, cases) ->
      let case_exprs = List.map cases ~f:(fun (AMatchCase (_, e)) -> e) in
      check_expr typeenv scrutinee *> check_exprs typeenv case_exprs
  | NatRec (n, z, s) -> check_exprs typeenv [ n; z; s ]
  | TypeApplication (expr, types) ->
      check_types typeenv types *> check_expr typeenv expr
  | TypeAbstraction (tvs, expr) ->
      let typeenv' = add_typevars typeenv tvs in
      check_expr typeenv' expr
  | _ -> return ()

let rec check_fun_decl typeenv params ret decls expr =
  let ret = Utils.type_of_returnType ret in
  check_params typeenv params
  *> many decls ~f:(check_decl typeenv)
  *> check_type typeenv ret *> check_expr typeenv expr

and check_decl typeenv = function
  | DeclFun (_, _, params, ret, _, decls, expr) ->
      check_fun_decl typeenv params ret decls expr
  | DeclFunGeneric (_, _, tvs, params, ret, _, decls, expr) ->
      let typeenv' = add_typevars typeenv tvs in
      check_fun_decl typeenv' params ret decls expr
  | _ -> return ()

and check_prog (AProgram (_, _, decls)) =
  let typeenv = [] in
  many_unit decls ~f:(check_decl typeenv)

let check_program prog =
  match check_prog prog with
  | Error kind -> Error { stacktrace = []; kind }
  | Ok x -> Ok x
