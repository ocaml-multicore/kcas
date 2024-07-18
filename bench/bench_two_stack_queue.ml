open Multicore_bench
module Queue = Kcas_data.Two_stack_queue

let run_one_domain ~budgetf ?(n_msgs = 50 * Util.iter_factor) () =
  let t = Queue.create () in

  let op push = if push then Queue.push t 101 else Queue.pop_opt t |> ignore in

  let init _ =
    assert (Queue.pop_opt t == None);
    Util.generate_push_and_pop_sequence n_msgs
  in
  let work _ bits = Util.Bits.iter op bits in

  Times.record ~budgetf ~n_domains:1 ~init ~work ()
  |> Times.to_thruput_metrics ~n:n_msgs ~singular:"message" ~config:"one domain"

let run_one ~budgetf ?(n_adders = 2) ?(n_takers = 2)
    ?(n_msgs = 100 * Util.iter_factor) () =
  let n_domains = n_adders + n_takers in

  let t = Queue.create () in

  let n_msgs_to_take = Atomic.make n_msgs |> Multicore_magic.copy_as_padded in
  let n_msgs_to_add = Atomic.make n_msgs |> Multicore_magic.copy_as_padded in

  let init _ = () in
  let work i () =
    if i < n_adders then
      let rec work () =
        let n = Util.alloc n_msgs_to_add in
        if 0 < n then begin
          for i = 1 to n do
            Queue.push t i
          done;
          work ()
        end
      in
      work ()
    else
      let rec work () =
        let n = Util.alloc n_msgs_to_take in
        if n <> 0 then begin
          for _ = 1 to n do
            while Option.is_none (Queue.pop_opt t) do
              Domain.cpu_relax ()
            done
          done;
          work ()
        end
      in
      work ()
  in
  let after () =
    Atomic.set n_msgs_to_take n_msgs;
    Atomic.set n_msgs_to_add n_msgs
  in

  let config =
    let format role blocking n =
      Printf.sprintf "%d %s%s%s" n
        (if blocking then "" else "nb ")
        role
        (if n = 1 then "" else "s")
    in
    Printf.sprintf "%s, %s"
      (format "adder" false n_adders)
      (format "taker" false n_takers)
  in
  Times.record ~budgetf ~n_domains ~init ~work ~after ()
  |> Times.to_thruput_metrics ~n:n_msgs ~singular:"message" ~config

let run_suite ~budgetf =
  run_one_domain ~budgetf ()
  @ (Util.cross [ 1; 2 ] [ 1; 2 ]
    |> List.concat_map @@ fun (n_adders, n_takers) ->
       run_one ~budgetf ~n_adders ~n_takers ())
