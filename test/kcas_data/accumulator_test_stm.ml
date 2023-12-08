open QCheck
open STM
open Kcas_data

module Spec = struct
  type cmd = Incr | Decr | Get | Set of int

  let show_cmd = function
    | Incr -> "Incr"
    | Decr -> "Decr"
    | Get -> "Get"
    | Set v -> "Set " ^ string_of_int v

  type state = int
  type sut = Accumulator.t

  let arb_cmd _s =
    [
      Gen.return Incr;
      Gen.return Decr;
      Gen.return Get;
      Gen.map (fun i -> Set i) Gen.nat;
    ]
    |> Gen.oneof |> make ~print:show_cmd

  let init_state = 0
  let init_sut () = Accumulator.make 0
  let cleanup _ = ()

  let next_state c s =
    match c with Incr -> s + 1 | Decr -> s - 1 | Get -> s | Set v -> v

  let precond _ _ = true

  let run c d =
    match c with
    | Incr -> Res (unit, Accumulator.incr d)
    | Decr -> Res (unit, Accumulator.decr d)
    | Get -> Res (int, Accumulator.get d)
    | Set v -> Res (unit, Accumulator.set d v)

  let postcond c (s : state) res =
    match (c, res) with
    | Incr, Res ((Unit, _), ()) -> true
    | Decr, Res ((Unit, _), ()) -> true
    | Set _, Res ((Unit, _), ()) -> true
    | Get, Res ((Int, _), res) -> res = s
    | _, _ -> false
end

let () =
  Stm_run.run ~count:1000 ~verbose:true ~name:"Accumulator" (module Spec)
  |> exit
