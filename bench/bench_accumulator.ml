open Kcas_data
open Multicore_bench

let run_one ~budgetf ~n_domains ?(n_ops = 180 * Util.iter_factor) () =
  let n_ops = n_ops * n_domains in

  let t = Accumulator.make 0 in

  let n_ops_todo = Atomic.make n_ops |> Multicore_magic.copy_as_padded in

  let init _ = () in

  let work _ () =
    let rec work () =
      let n = Util.alloc n_ops_todo in
      if n <> 0 then
        let rec loop n =
          if 0 < n then begin
            Accumulator.incr t;
            Accumulator.decr t;
            loop (n - 2)
          end
          else work ()
        in
        loop n
    in
    work ()
  in

  let after () = Atomic.set n_ops_todo n_ops in

  let config =
    Printf.sprintf "%d worker%s, 0%% reads" n_domains
      (if n_domains = 1 then "" else "s")
  in

  Times.record ~budgetf ~n_domains ~init ~work ~after ()
  |> Times.to_thruput_metrics ~n:n_ops ~config ~singular:"operation"

let run_suite ~budgetf =
  [ 1; 2; 4 ]
  |> List.concat_map @@ fun n_domains -> run_one ~n_domains ~budgetf ()
