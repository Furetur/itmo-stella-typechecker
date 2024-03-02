open Base

module type PassConfig = sig
  type pass_state
  type pass_error
end

module SingleError (Cfg : PassConfig) = struct
  type 'a t = Cfg.pass_state -> (Cfg.pass_state * 'a, Cfg.pass_error) Result.t

  (* ----- Base ----- *)

  (* - Constructors - *)

  let return (x : 'r) : 'r t = fun s -> Ok (s, x)
  let fail (err : Cfg.pass_error) : _ t = fun _ -> Error err

  (* - State -  *)

  let get : Cfg.pass_state t = fun s -> Ok (s, s)
  let set (s : Cfg.pass_state) : unit t = fun _ -> Ok (s, ())

  (* - Combinators - *)

  let ( let* ) (t : 'a t) (f : 'a -> 'b t) : 'b t =
   fun s ->
    let pass_result = t s in
    match pass_result with Error err -> Error err | Ok (s, a) -> f a s

  (* ----- Helpers ----- *)

  let ( *> ) x y =
    let* _ = x in
    y

  let ( <* ) x y =
    let* x = x in
    let* _ = y in
    return x

  let many (xs : 'a list) ~(f : 'a -> 'b t) : 'b list t =
    let rec aux xs acc =
      match xs with
      | [] -> return acc
      | a :: xs ->
          let* b = f a in
          aux xs (acc @ [ b ])
    in
    aux xs []
end
