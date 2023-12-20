open Kcas
open Bench

let run_one ?(factor = 1) ?(n_iter = 25 * factor * Util.iter_factor) () =
  let loc = Loc.make 0 in

  let init _ = () in
  let work _ () =
    let rec loop i =
      if i > 0 then begin
        Loc.incr loc;
        loop (i - 1)
      end
    in
    loop n_iter
  in

  let times = Times.record ~n_domains:1 ~init ~work () in

  List.concat
    [
      Stats.of_times times
      |> Stats.scale (1_000_000_000.0 /. Float.of_int n_iter)
      |> Stats.to_json ~name:"time per op/incr"
           ~description:"Time to perform a single op" ~units:"ns";
      Times.invert times |> Stats.of_times
      |> Stats.scale (Float.of_int n_iter /. 1_000_000.0)
      |> Stats.to_json ~name:"ops over time/incr"
           ~description:"Number of operations performed over time" ~units:"M/s";
    ]

let run_suite ~factor = run_one ~factor ()
