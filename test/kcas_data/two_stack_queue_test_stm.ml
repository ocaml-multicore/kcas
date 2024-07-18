open QCheck
open STM
open Kcas_data

module Spec = struct
  type cmd = Push of int | Take_opt | Length

  let show_cmd = function
    | Push x -> "Push " ^ string_of_int x
    | Take_opt -> "Take_opt"
    | Length -> "Length"

  module State = struct
    type t = int list * int list

    let push x (h, t) = if h == [] then ([ x ], []) else (h, x :: t)
    let peek_opt (h, _) = match h with x :: _ -> Some x | [] -> None

    let drop ((h, t) as s) =
      match h with [] -> s | [ _ ] -> (List.rev t, []) | _ :: h -> (h, t)

    let length (h, t) = List.length h + List.length t
  end

  type state = State.t
  type sut = int Two_stack_queue.t

  let arb_cmd _s =
    [
      Gen.int |> Gen.map (fun x -> Push x);
      Gen.return Take_opt;
      Gen.return Length;
    ]
    |> Gen.oneof |> make ~print:show_cmd

  let init_state = ([], [])
  let init_sut () = Two_stack_queue.create ()
  let cleanup _ = ()

  let next_state c s =
    match c with
    | Push x -> State.push x s
    | Take_opt -> State.drop s
    | Length -> s

  let precond _ _ = true

  let run c d =
    match c with
    | Push x -> Res (unit, Two_stack_queue.push d x)
    | Take_opt -> Res (option int, Two_stack_queue.pop_opt d)
    | Length -> Res (int, Two_stack_queue.length d)

  let postcond c (s : state) res =
    match (c, res) with
    | Push _x, Res ((Unit, _), ()) -> true
    | Take_opt, Res ((Option Int, _), res) -> res = State.peek_opt s
    | Length, Res ((Int, _), res) -> res = State.length s
    | _, _ -> false
end

let () =
  Stm_run.run ~count:1000 ~verbose:true ~name:"Two_stack_queue" (module Spec)
  |> exit
