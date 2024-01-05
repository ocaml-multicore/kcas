open Kcas
open Multicore_bench

type t = Op : string * int * 'a * ('a Loc.t -> unit) * ('a Loc.t -> unit) -> t

let run_one ~budgetf ?(n_iter = 250 * Util.iter_factor)
    (Op (name, extra, value, op1, op2)) =
  let n_iter = n_iter * extra in

  let loc = Loc.make value in

  let init _ = () in
  let work _ () =
    let rec loop i =
      if i > 0 then begin
        op1 loc;
        op2 loc;
        loop (i - 2)
      end
    in
    loop n_iter
  in

  Times.record ~budgetf ~n_domains:1 ~init ~work ()
  |> Times.to_thruput_metrics ~n:n_iter ~singular:"op" ~config:name

let run_suite ~budgetf =
  [
    (let get x = Loc.get x |> ignore in
     Op ("get", 5, 42, get, get));
    (let incr x = Loc.incr x in
     Op ("incr", 1, 0, incr, incr));
    (let push x = Loc.modify x (fun xs -> 101 :: xs)
     and pop x = Loc.modify x (function [] -> [] | _ :: xs -> xs) in
     Op ("push & pop", 1, [], push, pop));
    (let cas01 x = Loc.compare_and_set x 0 1 |> ignore
     and cas10 x = Loc.compare_and_set x 1 0 |> ignore in
     Op ("cas int", 2, 0, cas01, cas10));
    (let xchg1 x = Loc.exchange x 1 |> ignore
     and xchg0 x = Loc.exchange x 0 |> ignore in
     Op ("xchg int", 2, 0, xchg1, xchg0));
    (let swap x = Loc.modify x (fun (x, y) -> (y, x)) in
     Op ("swap", 1, (4, 2), swap, swap));
  ]
  |> List.concat_map @@ run_one ~budgetf
