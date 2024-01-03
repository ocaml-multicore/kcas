open Kcas_data
open Bench

let run_one ~budgetf ?(n_adders = 2) ?(blocking_add = false) ?(n_takers = 2)
    ?(blocking_take = false) ?(n_msgs = 2 * Util.iter_factor) () =
  let n_domains = n_adders + n_takers in

  let t = Mvar.create None in

  let n_msgs_to_take = Atomic.make n_msgs |> Multicore_magic.copy_as_padded in
  let n_msgs_to_add = Atomic.make n_msgs |> Multicore_magic.copy_as_padded in

  let init _ = () in
  let work i () =
    if i < n_adders then
      if blocking_add then
        let rec work () =
          let n = Util.alloc n_msgs_to_add in
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
          let n = Util.alloc n_msgs_to_add in
          if 0 < n then begin
            for i = 1 to n do
              while not (Mvar.try_put t i) do
                Domain.cpu_relax ()
              done
            done;
            work ()
          end
        in
        work ()
    else if blocking_take then
      let rec work () =
        let n = Util.alloc n_msgs_to_take in
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
        let n = Util.alloc n_msgs_to_take in
        if n <> 0 then begin
          for _ = 1 to n do
            while Option.is_none (Mvar.take_opt t) do
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

  let times = Times.record ~n_domains ~budgetf ~init ~work ~after () in

  let name metric =
    let format role blocking n =
      Printf.sprintf "%d %s%s%s" n
        (if blocking then "" else "nb ")
        role
        (if n = 1 then "" else "s")
    in
    Printf.sprintf "%s/%s, %s" metric
      (format "adder" blocking_add n_adders)
      (format "taker" blocking_take n_takers)
  in

  List.concat
    [
      Stats.of_times times
      |> Stats.scale (1_000_000_000.0 /. Float.of_int n_msgs)
      |> Stats.to_json ~name:(name "time per message")
           ~description:
             "Time to transmit one message from one domain to another"
           ~units:"ns";
      Times.invert times |> Stats.of_times
      |> Stats.scale (Float.of_int (n_msgs * n_domains) /. 1_000_000.0)
      |> Stats.to_json
           ~name:(name "messages over time")
           ~description:
             "Number of messages transmitted over time using all domains"
           ~units:"M/s";
    ]

let run_suite ~budgetf =
  Util.cross
    (Util.cross [ 1; 2 ] [ false; true ])
    (Util.cross [ 1; 2 ] [ false; true ])
  |> List.concat_map
     @@ fun ((n_adders, blocking_add), (n_takers, blocking_take)) ->
     run_one ~budgetf ~n_adders ~blocking_add ~n_takers ~blocking_take ()
