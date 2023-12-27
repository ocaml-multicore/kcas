open Kcas
open Bench

type t = Op : string * 'a * ('a Loc.t -> unit) * ('a Loc.t -> unit) -> t

let run_one ?(factor = 1) ?(n_iter = 25 * factor * Util.iter_factor)
    (Op (name, value, op1, op2)) =
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

  let times = Times.record ~n_domains:1 ~init ~work () in

  List.concat
    [
      Stats.of_times times
      |> Stats.scale (1_000_000_000.0 /. Float.of_int n_iter)
      |> Stats.to_json
           ~name:(Printf.sprintf "time per op/%s" name)
           ~description:"Time to perform a single op" ~units:"ns";
      Times.invert times |> Stats.of_times
      |> Stats.scale (Float.of_int n_iter /. 1_000_000.0)
      |> Stats.to_json
           ~name:(Printf.sprintf "ops over time/%s" name)
           ~description:"Number of operations performed over time" ~units:"M/s";
    ]

let run_suite ~factor =
  [
    (let get x = Loc.get x |> ignore in
     Op ("get", 42, get, get));
    (let incr x = Loc.incr x in
     Op ("incr", 0, incr, incr));
    (let push x = Loc.modify x (fun xs -> 101 :: xs)
     and pop x = Loc.modify x (function [] -> [] | _ :: xs -> xs) in
     Op ("push & pop", [], push, pop));
    (let cas01 x = Loc.compare_and_set x 0 1 |> ignore
     and cas10 x = Loc.compare_and_set x 1 0 |> ignore in
     Op ("cas int", 0, cas01, cas10));
    (let xchg1 x = Loc.exchange x 1 |> ignore
     and xchg0 x = Loc.exchange x 0 |> ignore in
     Op ("xchg int", 0, xchg1, xchg0));
    (let swap x = Loc.modify x (fun (x, y) -> (y, x)) in
     Op ("swap", (4, 2), swap, swap));
  ]
  |> List.concat_map @@ run_one ~factor
