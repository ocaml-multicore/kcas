open Kcas
open Bench

let run_one ~budgetf ~n_domains ?(n_ops = 50 * Util.iter_factor) () =
  let n_ops = n_ops * n_domains in

  let a = Loc.make ~padded:true 10 in
  let b = Loc.make ~padded:true 52 in
  let xs = Loc.make_array ~padded:true n_domains 0 in

  let n_ops_todo = Atomic.make n_ops |> Multicore_magic.copy_as_padded in

  let init i = Array.unsafe_get xs i in

  let work _ x =
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
      let n = Util.alloc n_ops_todo in
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

  let after () = Atomic.set n_ops_todo n_ops in

  let times = Times.record ~n_domains ~budgetf ~init ~work ~after () in

  let name metric =
    Printf.sprintf "%s/%d worker%s" metric n_domains
      (if n_domains = 1 then "" else "s")
  in

  List.concat
    [
      Stats.of_times times
      |> Stats.scale (1_000_000_000.0 /. Float.of_int n_ops)
      |> Stats.to_json
           ~name:(name "time per transaction")
           ~description:"Time to perform a single transaction" ~units:"ns";
      Times.invert times |> Stats.of_times
      |> Stats.scale (Float.of_int (n_ops * n_domains) /. 1_000_000.0)
      |> Stats.to_json
           ~name:(name "transactions over time")
           ~description:
             "Number of transactions performed over time using 2 domains"
           ~units:"M/s";
    ]

let run_suite ~budgetf =
  [ 1; 2; 4 ]
  |> List.concat_map @@ fun n_domains -> run_one ~budgetf ~n_domains ()
