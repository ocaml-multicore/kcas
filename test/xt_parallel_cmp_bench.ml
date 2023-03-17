module Loc = Kcas.Loc
module Xt = Kcas.Xt

let parallel_cmp_benchmark n_iter =
  let barrier = Barrier.make 2 in

  let a = Loc.make 10 and b = Loc.make 52 in

  let thread1 () =
    let x = Loc.make 0 in
    let tx1 ~xt =
      let a = Xt.get ~xt a and b = Xt.get ~xt b in
      Xt.set ~xt x (b - a)
    and tx2 ~xt =
      let a = Xt.get ~xt a and b = Xt.get ~xt b in
      Xt.set ~xt x (a + b)
    in
    let tx1 = { Xt.tx = tx1 } and tx2 = { Xt.tx = tx2 } in
    Barrier.await barrier;
    let start = Unix.gettimeofday () in
    for _ = 1 to n_iter do
      Xt.commit tx1;
      Xt.commit tx2
    done;
    Unix.gettimeofday () -. start
  and thread2 () =
    let y = Loc.make 0 in
    let tx1 ~xt =
      let a = Xt.get ~xt a and b = Xt.get ~xt b in
      Xt.set ~xt y (b - a)
    and tx2 ~xt =
      let a = Xt.get ~xt a and b = Xt.get ~xt b in
      Xt.set ~xt y (a + b)
    in
    let tx1 = { Xt.tx = tx1 } and tx2 = { Xt.tx = tx2 } in
    Barrier.await barrier;
    let start = Unix.gettimeofday () in
    for _ = 1 to n_iter do
      Xt.commit tx1;
      Xt.commit tx2
    done;
    Unix.gettimeofday () -. start
  in

  let total =
    [ thread1; thread2 ] |> List.map Domain.spawn |> List.map Domain.join
    |> List.fold_left ( +. ) 0.0
  in

  Printf.printf "%f ns/tx\n"
    (1_000_000_000.0 *. total /. Float.of_int (4 * n_iter))

let () =
  let n_iter = try int_of_string Sys.argv.(1) with _ -> 1_000_000 in
  parallel_cmp_benchmark n_iter
