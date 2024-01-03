open Kcas_data
open Bench

let run_single ~budgetf ?(n_msgs = 15 * Util.iter_factor) () =
  let t = Dllist.create () in

  let init _ = () in
  let work _ () =
    for i = 1 to n_msgs do
      Dllist.add_l i t |> ignore;
      Dllist.take_r t |> ignore
    done
  in

  let times = Times.record ~n_domains:1 ~budgetf ~init ~work () in

  let name metric = Printf.sprintf "%s/single-domain" metric in

  List.concat
    [
      Stats.of_times times
      |> Stats.scale (1_000_000_000.0 /. Float.of_int n_msgs)
      |> Stats.to_json ~name:(name "time per message")
           ~description:
             "Time to transmit one message from one domain to another"
           ~units:"ns";
      Times.invert times |> Stats.of_times
      |> Stats.scale (Float.of_int n_msgs /. 1_000_000.0)
      |> Stats.to_json
           ~name:(name "messages over time")
           ~description:
             "Number of messages transmitted over time using all domains"
           ~units:"M/s";
    ]

let run_one ~budgetf ?(n_adders = 2) ?(n_takers = 2) ?(factor = 1)
    ?(n_msgs = 20 * factor * Util.iter_factor) () =
  let n_domains = n_adders + n_takers in

  let t = Dllist.create () in

  let n_msgs_to_take = Atomic.make n_msgs |> Multicore_magic.copy_as_padded in
  let n_msgs_to_add = Atomic.make n_msgs |> Multicore_magic.copy_as_padded in

  let init _ = () in
  let work i () =
    if i < n_adders then
      let rec work () =
        let n = Util.alloc n_msgs_to_add in
        if 0 < n then begin
          for i = 1 to n do
            Dllist.add_r i t |> ignore
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
            while Option.is_none (Dllist.take_opt_l t) do
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
      (format "adder" false n_adders)
      (format "taker" false n_takers)
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
  run_single ~budgetf ()
  @ (Util.cross [ 1; 2 ] [ 1; 2 ]
    |> List.concat_map @@ fun (n_adders, n_takers) ->
       run_one ~budgetf ~n_adders ~n_takers ())
