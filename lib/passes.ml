open Base

module Result_pass_syntax = struct
  let return = Result.return
  let fail = Result.fail
  let ( let* ) = Result.( >>= )

  let ( *> ) a b =
    let* _ = a in
    b

  let ok = return ()

  let rec all_unit (list : 'a list) ~(f : 'a -> (unit, 'b) Result.t) :
      (unit, 'b) Result.t =
    match list with
    | [] -> ok
    | h :: t ->
        let* _ = f h in
        all_unit t ~f
end

module type PassConfig = sig
  type pass_state
  type pass_error
end

module SingleError (Cfg : PassConfig) = struct
  type 'a t = Cfg.pass_state -> (Cfg.pass_state * 'a, Cfg.pass_error) Result.t

  (* ----- Base ----- *)

  let run_pass ~(init : Cfg.pass_state) (pass : 'r t) :
      ('r, Cfg.pass_error) Result.t =
    let pass_result = pass init in
    Result.map pass_result ~f:(fun (_, r) -> r)

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

  let catch_error (f : unit -> 'a t) ~(handle : Cfg.pass_error -> 'a t) : 'a t =
   fun state ->
    let result = f () state in
    match result with
    | Ok x -> Ok x
    | Error err ->
        let t = handle err in
        (* TODO: this is old state *)
        t state

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

  let many_unit (xs : 'a list) ~(f : 'a -> unit t) : unit t =
    many xs ~f *> return ()

  let fold (xs : 'a list) ~(init : 'acc) ~(f : 'acc -> 'a -> 'acc t) : 'acc t =
    List.fold xs ~init:(return init) ~f:(fun acc_t x ->
        let* acc = acc_t in
        f acc x)
end
