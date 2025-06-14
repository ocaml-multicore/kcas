open Kcas_data
open Multicore_bench

module Int = struct
  include Int

  let hash = Fun.id
end

let run_one ~budgetf ~n_domains ?(n_ops = 40 * Util.iter_factor)
    ?(n_keys = 1000) ~percent_read () =
  let t = Hashtbl.create ~hashed_type:(module Int) () in

  let n_ops = (100 + percent_read) * n_ops / 100 in
  let n_ops = n_ops * n_domains in

  for i = 0 to n_keys - 1 do
    Hashtbl.replace t i i
  done;

  let n_ops_todo = Countdown.create ~n_domains () in

  let init _ =
    Countdown.non_atomic_set n_ops_todo n_ops;
    Random.State.make_self_init ()
  in
  let wrap _ _ action = Scheduler.run action in
  let work domain_index state =
    let rec work () =
      let n = Countdown.alloc n_ops_todo ~domain_index ~batch:1000 in
      if n <> 0 then
        let rec loop n =
          if 0 < n then
            let value = Random.State.bits state in
            let op = (value asr 20) mod 100 in
            let key = value mod n_keys in
            if op < percent_read then begin
              Hashtbl.find_opt t key |> ignore;
              loop (n - 1)
            end
            else begin
              Hashtbl.remove t key;
              Hashtbl.add t key value;
              loop (n - 2)
            end
          else work ()
        in
        loop n
    in
    work ()
  in

  let config =
    Printf.sprintf "%d worker%s, %d%% reads" n_domains
      (if n_domains = 1 then "" else "s")
      percent_read
  in

  Times.record ~budgetf ~n_domains ~init ~wrap ~work ()
  |> Times.to_thruput_metrics ~n:n_ops ~singular:"operation" ~config

let run_suite ~budgetf =
  Util.cross [ 90; 50; 10 ] [ 1; 2; 4; 8 ]
  |> List.concat_map @@ fun (percent_read, n_domains) ->
     if Domain.recommended_domain_count () < n_domains then []
     else run_one ~budgetf ~n_domains ~percent_read ()
