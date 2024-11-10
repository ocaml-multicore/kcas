open Kcas_data
open Multicore_bench

let run_one ~budgetf ~n_domains ?(n_ops = 180 * Util.iter_factor) () =
  let n_ops = n_ops * n_domains in

  let t = Accumulator.make 0 in

  let n_ops_todo = Countdown.create ~n_domains () in

  let init _ = Countdown.non_atomic_set n_ops_todo n_ops in
  let work domain_index () =
    let rec work () =
      let n = Countdown.alloc n_ops_todo ~domain_index ~batch:1000 in
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

  let config =
    Printf.sprintf "%d worker%s, 0%% reads" n_domains
      (if n_domains = 1 then "" else "s")
  in

  Times.record ~budgetf ~n_domains ~init ~work ()
  |> Times.to_thruput_metrics ~n:n_ops ~config ~singular:"operation"

let run_suite ~budgetf =
  [ 1; 2; 4; 8 ]
  |> List.concat_map @@ fun n_domains ->
     if Domain.recommended_domain_count () < n_domains then []
     else run_one ~n_domains ~budgetf ()
