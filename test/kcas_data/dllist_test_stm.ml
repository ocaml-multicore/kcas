open QCheck
open STM
open Kcas_data

module Spec = struct
  type cmd = Add_l of int | Take_opt_l | Add_r of int | Take_opt_r

  let show_cmd = function
    | Add_l x -> "Add_l " ^ string_of_int x
    | Take_opt_l -> "Take_opt_l"
    | Add_r x -> "Add_r " ^ string_of_int x
    | Take_opt_r -> "Take_opt_r"

  module State = struct
    type t = int list * int list

    let push_l x (l, r) = (x :: l, r)
    let push_r x (l, r) = (l, x :: r)

    let drop_l (l, r) =
      match l with
      | _ :: l -> (l, r)
      | [] -> begin
          match List.rev r with [] -> ([], []) | _ :: l -> (l, [])
        end

    let drop_r (l, r) =
      match r with
      | _ :: r -> (l, r)
      | [] -> begin
          match List.rev l with [] -> ([], []) | _ :: r -> ([], r)
        end

    let peek_opt_l (l, r) =
      match l with
      | x :: _ -> Some x
      | [] -> begin match List.rev r with x :: _ -> Some x | [] -> None end

    let peek_opt_r (l, r) =
      match r with
      | x :: _ -> Some x
      | [] -> begin match List.rev l with x :: _ -> Some x | [] -> None end
  end

  type state = State.t
  type sut = int Dllist.t

  let arb_cmd _s =
    [
      Gen.int |> Gen.map (fun x -> Add_l x);
      Gen.return Take_opt_l;
      Gen.int |> Gen.map (fun x -> Add_r x);
      Gen.return Take_opt_r;
    ]
    |> Gen.oneof |> make ~print:show_cmd

  let init_state = ([], [])
  let init_sut () = Dllist.create ()
  let cleanup _ = ()

  let next_state c s =
    match c with
    | Add_l x -> State.push_l x s
    | Take_opt_l -> State.drop_l s
    | Add_r x -> State.push_r x s
    | Take_opt_r -> State.drop_r s

  let precond _ _ = true

  let run c d =
    match c with
    | Add_l x -> Res (unit, Dllist.add_l x d |> ignore)
    | Take_opt_l -> Res (option int, Dllist.take_opt_l d)
    | Add_r x -> Res (unit, Dllist.add_r x d |> ignore)
    | Take_opt_r -> Res (option int, Dllist.take_opt_r d)

  let postcond c (s : state) res =
    match (c, res) with
    | Add_l _x, Res ((Unit, _), ()) -> true
    | Take_opt_l, Res ((Option Int, _), res) -> res = State.peek_opt_l s
    | Add_r _x, Res ((Unit, _), ()) -> true
    | Take_opt_r, Res ((Option Int, _), res) -> res = State.peek_opt_r s
    | _, _ -> false
end

let () = Stm_run.run ~name:"Dllist" (module Spec) |> exit
