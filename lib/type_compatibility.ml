open Base
open Stella_parser.Parsetree

type type_compatibility_checker = target:typeT -> source:typeT -> bool

let rec subtype_fun checker s_params s_ret t_params t_ret =
  match s_params, t_params with
  | [], [] -> checker s_ret t_ret
  | [s_param], [t_param] -> checker t_param s_param
  | _ -> false

and subtype_check atom_checker s t =
  match (s, t) with
  | TypeBottom, _ -> true
  | _, TypeTop -> true
  | TypeFun (s_params, s_ret), TypeFun (t_params, t_ret) ->
      subtype_fun atom_checker s_params s_ret t_params t_ret
  | TypeList s, TypeList t -> subtype_check atom_checker s t
  | TypeRef s, TypeRef t -> (subtype_check atom_checker s t) && (subtype_check atom_checker t s)
  | TypeSum (l1, r1), TypeSum(l2, r2) ->
    (subtype_check atom_checker l1 l2) && (subtype_check atom_checker r1 r2)
  | l, r -> atom_checker l r



module Simple = struct
  let check s t = Stdlib.( = ) s t
end

module WithTopBottom = struct
  let check ~target ~source =
    match target with
    | TypeTop -> true
    | _ -> (
        match source with TypeBottom -> true | _ -> Stdlib.( = ) target source)
end
