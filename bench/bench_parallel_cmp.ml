open Kcas
open Multicore_bench

let run_one ~budgetf ~n_domains ?(n_ops = 50 * Util.iter_factor) () =
  let n_ops = n_ops * n_domains in

  let a = Loc.make ~padded:true 10 in
  let b = Loc.make ~padded:true 52 in
  let xs = Loc.make_array ~padded:true n_domains 0 in

  let n_ops_todo = Countdown.create ~n_domains () in

  let init i =
    Countdown.non_atomic_set n_ops_todo n_ops;
    Array.unsafe_get xs i
  in
  let work domain_index x =
    let tx1 ~xt =
      let a = Xt.get ~xt a in
      let b = Xt.get ~xt b in
      Xt.set ~xt x (b - a)
    and tx2 ~xt =
      let a = Xt.get ~xt a in
      let b = Xt.get ~xt b in
      Xt.set ~xt x (a + b)
    in
    let rec work () =
      let n = Countdown.alloc n_ops_todo ~domain_index ~batch:1000 in
      if n <> 0 then begin
        for _ = 1 to n asr 1 do
          Xt.commit { tx = tx1 };
          Xt.commit { tx = tx2 }
        done;
        work ()
      end
    in
    work ()
  in

  let config =
    Printf.sprintf "%d worker%s" n_domains (if n_domains = 1 then "" else "s")
  in

  Times.record ~budgetf ~n_domains ~init ~work ()
  |> Times.to_thruput_metrics ~n:n_ops ~singular:"transaction" ~config

let run_suite ~budgetf =
  [ 1; 2; 4 ]
  |> List.concat_map @@ fun n_domains ->
     if Domain.recommended_domain_count () < n_domains then []
     else run_one ~budgetf ~n_domains ()
