open Multicore_bench

module Ref = struct
  type 'a t = 'a ref

  let make = ref

  let[@inline] compare_and_set x before after =
    !x == before
    && begin
         x := after;
         true
       end

  let[@inline] exchange x after =
    let before = !x in
    x := after;
    before
end

type t = Op : string * 'a * ('a Ref.t -> unit) * ('a Ref.t -> unit) -> t

(** For some reason allocating the mutex inside [run_one] tends to cause
    performance hiccups, i.e. some operations appear to be 10x slower than
    others, which doesn't make sense.  So, we allocate the mutex here. *)
let mutex = Mutex.create ()

let run_one ~budgetf ?(n_iter = 250 * Util.iter_factor)
    (Op (name, value, op1, op2)) =
  let loc = Ref.make value in

  let init _ = () in
  let work _ () =
    let rec loop i =
      if i > 0 then begin
        Mutex.lock mutex;
        op1 loc;
        Mutex.unlock mutex;
        Mutex.lock mutex;
        op2 loc;
        Mutex.unlock mutex;
        loop (i - 2)
      end
    in
    loop n_iter
  in

  Times.record ~budgetf ~n_domains:1 ~init ~work ()
  |> Times.to_thruput_metrics ~n:n_iter ~singular:"op" ~config:name

let run_suite ~budgetf =
  [
    (let get x = !x |> ignore in
     Op ("get", 42, get, get));
    (let incr x = x := !x + 1 in
     Op ("incr", 0, incr, incr));
    (let push x = x := 101 :: !x
     and pop x = match !x with [] -> () | _ :: xs -> x := xs in
     Op ("push & pop", [], push, pop));
    (let cas01 x = Ref.compare_and_set x 0 1 |> ignore
     and cas10 x = Ref.compare_and_set x 1 0 |> ignore in
     Op ("cas int", 0, cas01, cas10));
    (let xchg1 x = Ref.exchange x 1 |> ignore
     and xchg0 x = Ref.exchange x 0 |> ignore in
     Op ("xchg int", 0, xchg1, xchg0));
    (let swap x =
       let l, r = !x in
       x := (r, l)
     in
     Op ("swap", (4, 2), swap, swap));
  ]
  |> List.concat_map @@ run_one ~budgetf
