open Kcas_data
open Multicore_bench

let run_one ~budgetf ?(n_adders = 2) ?(blocking_add = false) ?(n_takers = 2)
    ?(blocking_take = false) ?(n_msgs = 2 * Util.iter_factor) () =
  let n_domains = n_adders + n_takers in

  let t = Mvar.create None in

  let n_msgs_to_take = Countdown.create ~n_domains:n_takers () in
  let n_msgs_to_add = Countdown.create ~n_domains:n_adders () in

  let init _ =
    Countdown.non_atomic_set n_msgs_to_take n_msgs;
    Countdown.non_atomic_set n_msgs_to_add n_msgs
  in
  let wrap _ _ action = Scheduler.run action in
  let work i () =
    if i < n_adders then
      let domain_index = i in
      if blocking_add then
        let rec work () =
          let n = Countdown.alloc n_msgs_to_add ~domain_index ~batch:1000 in
          if 0 < n then begin
            for i = 1 to n do
              Mvar.put t i
            done;
            work ()
          end
        in
        work ()
      else
        let rec work () =
          let n = Countdown.alloc n_msgs_to_add ~domain_index ~batch:1000 in
          if 0 < n then begin
            for i = 1 to n do
              while not (Mvar.try_put t i) do
                Backoff.once Backoff.default |> ignore
              done
            done;
            work ()
          end
        in
        work ()
    else
      let domain_index = i - n_adders in
      if blocking_take then
        let rec work () =
          let n = Countdown.alloc n_msgs_to_take ~domain_index ~batch:1000 in
          if n <> 0 then begin
            for _ = 1 to n do
              ignore (Mvar.take t)
            done;
            work ()
          end
        in
        work ()
      else
        let rec work () =
          let n = Countdown.alloc n_msgs_to_take ~domain_index ~batch:1000 in
          if n <> 0 then begin
            for _ = 1 to n do
              while Option.is_none (Mvar.take_opt t) do
                Backoff.once Backoff.default |> ignore
              done
            done;
            work ()
          end
        in
        work ()
  in

  let config =
    let format role blocking n =
      Printf.sprintf "%d %s%s%s" n
        (if blocking then "" else "nb ")
        role
        (if n = 1 then "" else "s")
    in
    Printf.sprintf "%s, %s"
      (format "adder" blocking_add n_adders)
      (format "taker" blocking_take n_takers)
  in

  Times.record ~budgetf ~n_domains ~init ~wrap ~work ()
  |> Times.to_thruput_metrics ~n:n_msgs ~singular:"message" ~config

let run_suite ~budgetf =
  Util.cross
    (Util.cross [ 1; 2 ] [ false; true ])
    (Util.cross [ 1; 2 ] [ false; true ])
  |> List.concat_map
     @@ fun ((n_adders, blocking_add), (n_takers, blocking_take)) ->
     if Domain.recommended_domain_count () < n_adders + n_takers then []
     else run_one ~budgetf ~n_adders ~blocking_add ~n_takers ~blocking_take ()
