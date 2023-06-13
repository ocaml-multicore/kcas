open Kcas
open Bench

let run_one ?(n_locs = 2) ?(factor = 1)
    ?(n_iter = 10 * factor * Util.iter_factor) () =
  let locs = Loc.make_array n_locs 0 in

  let to_1 =
    locs |> Array.map (fun loc -> Op.make_cas loc 0 1) |> Array.to_list
  in
  let to_0 =
    locs |> Array.map (fun loc -> Op.make_cas loc 1 0) |> Array.to_list
  in

  let init _ = () in
  let work _ () =
    let rec loop i =
      if i > 0 then begin
        Op.atomically to_1 |> ignore;
        Op.atomically to_0 |> ignore;
        loop (i - 2)
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
