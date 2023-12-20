open Kcas
open Bench

let run_one ?(n_locs = 2) ?(factor = 1)
    ?(n_iter = 10 * factor * Util.iter_factor) () =
  let locs = Loc.make_array n_locs 0 in

  let rec make_incr cass i n =
    if i < n then
      let loc = Array.unsafe_get locs i in
      let x = Loc.fenceless_get loc in
      let cas = Op.make_cas loc x (x + 1) in
      make_incr (cas :: cass) (i + 1) n
    else cass
  in

  let rec incr () =
    let cass = make_incr [] 0 n_locs in
    if not (Op.atomically cass) then incr ()
  in

  let init _ = () in
  let work _ () =
    let rec loop i =
      if i > 0 then begin
        incr ();
        loop (i - 1)
      end
    in
    loop n_iter
  in

  let times = Times.record ~n_domains:1 ~init ~work () in

  let name metric = Printf.sprintf "%s/%d loc op" metric n_locs in

  List.concat
    [
      Stats.of_times times
      |> Stats.scale (1_000_000_000.0 /. Float.of_int n_iter)
      |> Stats.to_json ~name:(name "time per op")
           ~description:"Time to perform a single op" ~units:"ns";
      Times.invert times |> Stats.of_times
      |> Stats.scale (Float.of_int n_iter /. 1_000_000.0)
      |> Stats.to_json ~name:(name "ops over time")
           ~description:"Number of operations performed over time" ~units:"M/s";
    ]

let run_suite ~factor =
  [ 1; 2; 4; 8 ] |> List.concat_map @@ fun n_locs -> run_one ~n_locs ~factor ()
