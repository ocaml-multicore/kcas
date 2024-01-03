open Kcas
open Bench

let run_one ~budgetf ?(n_locs = 2)
    ?(n_iter = 15 * (9 - n_locs) * Util.iter_factor) () =
  let locs = Loc.make_array n_locs 0 in
  let rec loop ~xt i =
    Xt.incr ~xt (Array.unsafe_get locs i);
    let i = i - 1 in
    if 0 <= i then loop ~xt i
  in
  let tx ~xt =
    let i = n_locs - 1 in
    if 0 <= i then loop ~xt i
  in

  let init _ = () in
  let work _ () =
    let rec loop i =
      if i > 0 then begin
        Xt.commit { tx };
        loop (i - 1)
      end
    in
    loop n_iter
  in

  let times = Times.record ~n_domains:1 ~budgetf ~init ~work () in

  let name metric = Printf.sprintf "%s/%d loc tx" metric n_locs in

  List.concat
    [
      Stats.of_times times
      |> Stats.scale (1_000_000_000.0 /. Float.of_int n_iter)
      |> Stats.to_json
           ~name:(name "time per transaction")
           ~description:"Time to perform a single transaction" ~units:"ns";
      Times.invert times |> Stats.of_times
      |> Stats.scale (Float.of_int n_iter /. 1_000_000.0)
      |> Stats.to_json
           ~name:(name "transactions over time")
           ~description:"Number of transactions performed over time"
           ~units:"M/s";
    ]

let run_suite ~budgetf =
  [ 0; 1; 2; 4; 8 ]
  |> List.concat_map @@ fun n_locs -> run_one ~budgetf ~n_locs ()
