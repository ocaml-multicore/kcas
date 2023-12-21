open QCheck
open STM
open Kcas_data

module Spec = struct
  type cmd = Add of int | Mem of int | Remove of int | Clear | Length

  let show_cmd = function
    | Add x -> "Add " ^ string_of_int x
    | Mem x -> "Mem " ^ string_of_int x
    | Remove x -> "Remove " ^ string_of_int x
    | Clear -> "Clear"
    | Length -> "Length"

  type state = int list
  type sut = (int, unit) Hashtbl.t

  let arb_cmd _s =
    [
      Gen.int_bound 10 |> Gen.map (fun x -> Add x);
      Gen.int_bound 10 |> Gen.map (fun x -> Mem x);
      Gen.int_bound 10 |> Gen.map (fun x -> Remove x);
      Gen.return Clear;
      Gen.return Length;
    ]
    |> Gen.oneof |> make ~print:show_cmd

  let init_state = []
  let init_sut () = Hashtbl.create ()
  let cleanup _ = ()

  let next_state c s =
    match c with
    | Add x -> x :: s
    | Mem _ -> s
    | Remove x ->
        let[@tail_mod_cons] rec drop_first = function
          | [] -> []
          | x' :: xs -> if x = x' then xs else x' :: drop_first xs
        in
        drop_first s
    | Clear -> []
    | Length -> s

  let precond _ _ = true

  let run c d =
    match c with
    | Add x -> Res (unit, Hashtbl.add d x ())
    | Mem x -> Res (bool, Hashtbl.mem d x)
    | Remove x -> Res (unit, Hashtbl.remove d x)
    | Clear -> Res (unit, Hashtbl.clear d)
    | Length -> Res (int, Hashtbl.length d)

  let postcond c (s : state) res =
    match (c, res) with
    | Add _x, Res ((Unit, _), ()) -> true
    | Mem x, Res ((Bool, _), res) -> res = List.exists (( = ) x) s
    | Remove _x, Res ((Unit, _), ()) -> true
    | Clear, Res ((Unit, _), ()) -> true
    | Length, Res ((Int, _), res) -> res = List.length s
    | _, _ -> false
end

let () =
  Stm_run.run ~count:1000 ~verbose:true ~name:"Hashtbl" (module Spec) |> exit
