open Bench

module Ref = struct
  type 'a t = 'a ref

  let make = ref
  let get = ( ! )
  let[@poll error] [@inline never] incr x = x := !x + 1

  let[@poll error] [@inline never] compare_and_set x before after =
    !x == before
    && begin
         x := after;
         true
       end

  let[@poll error] [@inline never] exchange x after =
    let before = !x in
    x := after;
    before

  let rec modify ?(backoff = Backoff.default) x f =
    let before = get x in
    let after = f before in
    if not (compare_and_set x before after) then
      modify ~backoff:(Backoff.once backoff) x f
end

type t = Op : string * int * 'a * ('a Ref.t -> unit) * ('a Ref.t -> unit) -> t

let run_one ~budgetf ?(n_iter = 500 * Util.iter_factor)
    (Op (name, extra, value, op1, op2)) =
  let n_iter = n_iter * extra in

  let loc = Ref.make value in

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

  let times = Times.record ~n_domains:1 ~budgetf ~init ~work () in

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

let run_suite ~budgetf =
  [
    (let get x = Ref.get x |> ignore in
     Op ("get", 10, 42, get, get));
    (let incr x = Ref.incr x in
     Op ("incr", 1, 0, incr, incr));
    (let push x = Ref.modify x (fun xs -> 101 :: xs)
     and pop x = Ref.modify x (function [] -> [] | _ :: xs -> xs) in
     Op ("push & pop", 2, [], push, pop));
    (let cas01 x = Ref.compare_and_set x 0 1 |> ignore
     and cas10 x = Ref.compare_and_set x 1 0 |> ignore in
     Op ("cas int", 1, 0, cas01, cas10));
    (let xchg1 x = Ref.exchange x 1 |> ignore
     and xchg0 x = Ref.exchange x 0 |> ignore in
     Op ("xchg int", 1, 0, xchg1, xchg0));
    (let swap x = Ref.modify x (fun (x, y) -> (y, x)) in
     Op ("swap", 2, (4, 2), swap, swap));
  ]
  |> List.concat_map @@ run_one ~budgetf
