open Base
open Stella_parser.Parsetree

type typeenv = stellaIdent list

let lookup_typevar typeenv name =
  List.find_mapi typeenv ~f:(fun i name' ->
      if Stdlib.(name = name') then Some i else None)

let add_typevars typeenv typevars = List.rev typevars @ typeenv
let tupled f (a, b) = f a b

(* ----- Compatibility ----- *)

let rec are_types_compatible lhs rhs env =
  let l_env, r_env = env in
  match (lhs, rhs) with
  | TypeTuple ts1, TypeTuple ts2 -> are_all_types_compatible ts1 ts2 env
  | TypeSum (a1, b1), TypeSum (a2, b2) ->
      are_all_types_compatible [ a1; b1 ] [ a2; b2 ] env
  | TypeList el1, TypeList el2 -> are_types_compatible el1 el2 env
  | TypeBool, TypeBool | TypeNat, TypeNat | TypeUnit, TypeUnit -> true
  | TypeFun (args1, ret1), TypeFun (args2, ret2) ->
      are_all_types_compatible (args1 @ [ ret1 ]) (args2 @ [ ret2 ]) env
  | TypeRecord fields1, TypeRecord fields2 ->
      let map1 = Type_map.of_record_fields fields1 in
      let map2 = Type_map.of_record_fields fields2 in
      let eq x y = are_types_compatible x y env in
      Map.equal eq map1 map2
  | TypeForAll (tv1, t1), TypeForAll (tv2, t2) ->
      let l_env' = add_typevars l_env tv1 in
      let r_env' = add_typevars r_env tv2 in
      let env' = (l_env', r_env') in
      are_types_compatible t1 t2 env'
  | TypeVar x, TypeVar y -> (
      let x_id = lookup_typevar l_env x in
      let y_id = lookup_typevar r_env y in
      match (x_id, y_id) with
      | Some x_id, Some y_id -> x_id = y_id
      | None, None -> Stdlib.(x = y)
      | _ -> false)
  | _ -> false

and are_all_types_compatible ts1 ts2 env =
  match List.zip ts1 ts2 with
  | Ok ts -> List.for_all ts ~f:(fun (a, b) -> are_types_compatible a b env)
  | Unequal_lengths -> false

let are_types_compatible lhs rhs = are_types_compatible lhs rhs ([], [])

(* ----- Substitution ----- *)

(** Replaces type variable 'tv' in 't' with 'target'  *)
let type_substitute_1 t tv target =
  let rec subst env t =
    match t with
    | TypeTuple ts ->
        let ts' = subst_all env ts in
        TypeTuple ts'
    | TypeSum (a, b) ->
        let a' = subst env a in
        let b' = subst env b in
        TypeSum (a', b')
    | TypeList el -> TypeList (subst env el)
    | TypeFun (args, ret) ->
        let args' = subst_all env args in
        let ret' = subst env ret in
        TypeFun (args', ret')
    | TypeForAll (tvs, t) ->
        let env' = add_typevars env tvs in
        let t' = subst env' t in
        TypeForAll (tvs, t')
    | TypeVar x -> (
        match lookup_typevar env x with
        | Some _ -> TypeVar x
        | None -> if Stdlib.(tv = x) then target else TypeVar x)
    | x -> x
  and subst_all env ts = List.map ts ~f:(subst env) in
  subst [] t

let type_substitute t tv_target_pairs =
  List.fold tv_target_pairs ~init:t ~f:(fun t (tv, target) ->
      type_substitute_1 t tv target)
