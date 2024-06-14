open Base
open Stella_parser.Parsetree

type next_typevar_id = int

module ThisPass = Passes.SingleError (struct
  type pass_state = next_typevar_id
  type pass_error = unit
end)

open ThisPass

(* ----- Helpers ----- *)

let typevar_prefix = "__auto"

let get_next_typevar =
  let* id = get in
  set (id + 1)
  *>
  let typevar = Utils.make_typevar typevar_prefix id in
  return typevar

(* ----- Pass ----- *)

let pass_type_var (StellaIdent name) =
  match name with "auto" -> get_next_typevar | _ -> Utils.not_implemented ()

let rec pass_types = many ~f:pass_type

and pass_type = function
  | TypeVar ident -> pass_type_var ident
  | TypeFun (args, ret) ->
      let* args = pass_types args in
      let* ret = pass_type ret in
      return @@ TypeFun (args, ret)
  | TypeSum (l, r) ->
      let* l = pass_type l in
      let* r = pass_type r in
      return @@ TypeSum (l, r)
  | TypeTuple els ->
      let* els = pass_types els in
      return @@ TypeTuple els
  | TypeList el ->
      let* el = pass_type el in
      return @@ TypeList el
  | TypeRecord fields -> pass_record fields
  | t -> return t

and pass_record fields =
  let pass_field (ARecordFieldType (ident, typ)) =
    let* typ = pass_type typ in
    return @@ ARecordFieldType (ident, typ)
  in
  let* fields = many fields ~f:pass_field in
  return @@ TypeRecord fields

let pass_param_decl (AParamDecl (ident, t)) =
  let* t = pass_type t in
  return @@ AParamDecl (ident, t)

let pass_param_decls = many ~f:pass_param_decl

let rec pass_exprs = many ~f:pass_expr

and pass_expr = function
  | Sequence (expr1, expr2) ->
      let* expr1 = pass_expr expr1 in
      let* expr2 = pass_expr expr2 in
      return @@ Sequence (expr1, expr2)
  | Application (callee, args) ->
      let* callee = pass_expr callee in
      let* args = pass_exprs args in
      return @@ Application (callee, args)
  | Abstraction (params, body) ->
      let* params = pass_param_decls params in
      let* body = pass_expr body in
      return @@ Abstraction (params, body)
  | ConstUnit -> return ConstUnit
  | If (cond, then_expr, else_expr) ->
      let* cond = pass_expr cond in
      let* then_expr = pass_expr then_expr in
      let* else_expr = pass_expr else_expr in
      return @@ If (cond, then_expr, else_expr)
  | TypeAsc (expr, typeT) ->
      let* e = pass_expr expr in
      let* t = pass_type typeT in
      return @@ TypeAsc (e, t)
  | Equal (l, r) -> pass_binary l r (fun l r -> Equal (l, r))
  | NotEqual (l, r) -> pass_binary l r (fun l r -> NotEqual (l, r))
  (* Bools *)
  | LogicAnd (l, r) -> pass_binary l r (fun l r -> LogicAnd (l, r))
  | LogicOr (l, r) -> pass_binary l r (fun l r -> LogicOr (l, r))
  | LogicNot expr -> pass_unary expr (fun e -> LogicNot e)
  (* Nats *)
  | IsZero expr -> pass_unary expr (fun e -> IsZero e)
  | Pred expr -> pass_unary expr (fun e -> Pred e)
  | Succ expr -> pass_unary expr (fun e -> Succ e)
  | Multiply (l, r) -> pass_binary l r (fun l r -> Multiply (l, r))
  | Divide (l, r) -> pass_binary l r (fun l r -> Divide (l, r))
  | Add (l, r) -> pass_binary l r (fun l r -> Add (l, r))
  | Subtract (l, r) -> pass_binary l r (fun l r -> Subtract (l, r))
  | LessThan (l, r) -> pass_binary l r (fun l r -> LessThan (l, r))
  | LessThanOrEqual (l, r) ->
      pass_binary l r (fun l r -> LessThanOrEqual (l, r))
  | GreaterThan (l, r) -> pass_binary l r (fun l r -> GreaterThan (l, r))
  | GreaterThanOrEqual (l, r) ->
      pass_binary l r (fun l r -> GreaterThanOrEqual (l, r))
  (* Bindings *)
  | Let (pattern_bindings, body_expr) ->
      let* e = pass_expr body_expr in
      return @@ Let (pattern_bindings, e)
  (* Tuples *)
  | DotTuple (expr, index) -> pass_unary expr (fun e -> DotTuple (e, index))
  | Tuple exprs ->
      let* exprs = pass_exprs exprs in
      return @@ Tuple exprs
  (* Records *)
  | DotRecord (expr, field_ident) ->
      pass_unary expr (fun e -> DotRecord (e, field_ident))
  | Record bindings ->
      let* bindings = many bindings ~f:pass_binding in
      return @@ Record bindings
  (* Lists *)
  | List elements ->
      let* elements = pass_exprs elements in
      return @@ List elements
  | ConsList (head, tail) -> pass_binary head tail (fun h t -> ConsList (h, t))
  | Head expr -> pass_unary expr (fun e -> Head e)
  | Tail expr -> pass_unary expr (fun e -> Tail e)
  | IsEmpty expr -> pass_unary expr (fun e -> IsEmpty e)
  (* Sum Types *)
  | Inl expr -> pass_unary expr (fun e -> Inl e)
  | Inr expr -> pass_unary expr (fun e -> Inr e)
  (* Pattern matching *)
  | Match (scrutinee, cases) ->
      let* scrutinee = pass_expr scrutinee in
      let* cases = many cases ~f:pass_match_case in
      return @@ Match (scrutinee, cases)
  (* Recursion *)
  | Fix expr -> pass_unary expr (fun e -> Fix e)
  | NatRec (n, z, s) ->
      let* n = pass_expr n in
      let* z = pass_expr z in
      let* s = pass_expr s in
      return @@ NatRec (n, z, s)
  | e -> return e

and pass_unary e constructor =
  let* e = pass_expr e in
  return @@ constructor e

and pass_binary l r constructor =
  let* l = pass_expr l in
  let* r = pass_expr r in
  return @@ constructor l r

and pass_match_case (AMatchCase (pat, expr)) =
  let* expr = pass_expr expr in
  return @@ AMatchCase (pat, expr)

and pass_binding (ABinding (ident, expr)) =
  let* e = pass_expr expr in
  return @@ ABinding (ident, e)

let rec pass_decls = many ~f:pass_decl

and pass_decl = function
  | DeclFun (annots, name, params, ret, throw, decls, body) ->
      let* params = pass_param_decls params in
      let* ret =
        match ret with
        | NoReturnType -> return NoReturnType
        | SomeReturnType t ->
            let* t = pass_type t in
            return @@ SomeReturnType t
      in
      let* decls = pass_decls decls in
      let* body = pass_expr body in
      return @@ DeclFun (annots, name, params, ret, throw, decls, body)
  | DeclFunGeneric _ as t ->
      (* When universal types are enabled, auto is not supported *)
      return t
  | DeclExceptionType t ->
      let* t = pass_type t in
      return (DeclExceptionType t)
  | _ -> Utils.not_implemented ()

let pass_program (AProgram (l, exts, decls)) =
  let* decls = pass_decls decls in
  return @@ AProgram (l, exts, decls)

let replace_auto prog =
  let pass = pass_program prog in
  let result = run_pass pass ~init:0 in
  match result with Ok prog -> prog | _ -> Utils.internal_error ()
