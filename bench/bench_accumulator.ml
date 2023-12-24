open Kcas_data
open Bench

let run_one ~n_domains ?(factor = 1) ?(n_ops = 60 * factor * Util.iter_factor)
    () =
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

  let times = Times.record ~n_domains ~init ~work ~after () in

  let name metric =
    Printf.sprintf "%s/%d worker%s, 0%% reads" metric n_domains
      (if n_domains = 1 then "" else "s")
  in

  List.concat
    [
      Stats.of_times times
      |> Stats.scale (1_000_000_000.0 /. Float.of_int n_ops)
      |> Stats.to_json
           ~name:(name "time per operation")
           ~description:"Average time to increment accumulator" ~units:"ns";
      Times.invert times |> Stats.of_times
      |> Stats.scale (Float.of_int (n_ops * n_domains) /. 1_000_000.0)
      |> Stats.to_json
           ~name:(name "operations over time")
           ~description:
             "Number of operations performed over time using all domains"
           ~units:"M/s";
    ]

let run_suite ~factor =
  [ 1; 2; 4 ]
  |> List.concat_map @@ fun n_domains -> run_one ~n_domains ~factor ()
