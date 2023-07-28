open Kcas
open Bench

let run_one ?(factor = 1) ?(n_iter = 10 * factor * Util.iter_factor) () =
  let a = Loc.make ~padded:true 10 and b = Loc.make ~padded:true 52 in

  let init _ = Loc.make ~padded:true 0 in
  let work i x =
    if i land 1 = 0 then begin
      let tx1 ~xt =
        let a = Xt.get ~xt a and b = Xt.get ~xt b in
        Xt.set ~xt x (b - a)
      and tx2 ~xt =
        let a = Xt.get ~xt a and b = Xt.get ~xt b in
        Xt.set ~xt x (a + b)
      in
      let tx1 = { Xt.tx = tx1 } and tx2 = { Xt.tx = tx2 } in
      for _ = 1 to n_iter asr 1 do
        Xt.commit tx1;
        Xt.commit tx2
      done
    end
    else begin
      let tx1 ~xt =
        let a = Xt.get ~xt a and b = Xt.get ~xt b in
        Xt.set ~xt x (b - a)
      and tx2 ~xt =
        let a = Xt.get ~xt a and b = Xt.get ~xt b in
        Xt.set ~xt x (a + b)
      in
      let tx1 = { Xt.tx = tx1 } and tx2 = { Xt.tx = tx2 } in
      for _ = 1 to n_iter asr 1 do
        Xt.commit tx1;
        Xt.commit tx2
      done
    end
  in

  let times = Times.record ~n_domains:2 ~init ~work () in

  List.concat
    [
      Stats.of_times times
      |> Stats.scale (1_000_000_000.0 /. Float.of_int n_iter)
      |> Stats.to_json ~name:"time per transaction"
           ~description:"Time to perform a single transaction" ~units:"ns";
      Times.invert times |> Stats.of_times
      |> Stats.scale (Float.of_int (n_iter * 2) /. 1_000_000.0)
      |> Stats.to_json ~name:"transactions over time"
           ~description:
             "Number of transactions performed over time using 2 domains"
           ~units:"M/s";
    ]

let run_suite ~factor = run_one ~factor ()
