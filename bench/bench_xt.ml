open Kcas
open Multicore_bench

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

  let config = Printf.sprintf "%d loc tx" n_locs in

  Times.record ~budgetf ~n_domains:1 ~init ~work ()
  |> Times.to_thruput_metrics ~n:n_iter ~singular:"transaction" ~config

let run_suite ~budgetf =
  [ 0; 1; 2; 4; 8 ]
  |> List.concat_map @@ fun n_locs -> run_one ~budgetf ~n_locs ()
