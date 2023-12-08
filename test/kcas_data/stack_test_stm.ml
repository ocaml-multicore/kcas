open Kcas_data

module Spec = struct
  type cmd = Push of int | Pop_opt | Top_opt | Length

  let show_cmd = function
    | Push x -> "Push " ^ string_of_int x
    | Pop_opt -> "Pop_opt"
    | Top_opt -> "Top_opt"
    | Length -> "Length"

  type state = int list
  type sut = int Stack.t

  let arb_cmd _s =
    QCheck.(
      [
        Gen.int |> Gen.map (fun x -> Push x);
        Gen.return Pop_opt;
        Gen.return Top_opt;
        Gen.return Length;
      ]
      |> Gen.oneof |> make ~print:show_cmd)

  let init_state = []
  let init_sut () = Stack.create ()
  let cleanup _ = ()

  let next_state c s =
    match c with
    | Push x -> x :: s
    | Pop_opt -> ( match s with [] -> [] | _ :: s -> s)
    | Top_opt -> s
    | Length -> s

  let precond _ _ = true

  let run c d =
    let open STM in
    match c with
    | Push x -> Res (unit, Stack.push x d)
    | Pop_opt -> Res (option int, Stack.pop_opt d)
    | Top_opt -> Res (option int, Stack.top_opt d)
    | Length -> Res (int, Stack.length d)

  let postcond c (s : state) res =
    let open STM in
    match (c, res) with
    | Push _x, Res ((Unit, _), ()) -> true
    | Pop_opt, Res ((Option Int, _), res) -> (
        res = match s with [] -> None | x :: _ -> Some x)
    | Top_opt, Res ((Option Int, _), res) -> (
        res = match s with [] -> None | x :: _ -> Some x)
    | Length, Res ((Int, _), res) -> res = List.length s
    | _, _ -> false
end

let () =
  Stm_run.run ~count:1000 ~verbose:true ~name:"Stack" (module Spec) |> exit
