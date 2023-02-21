module Loc = Kcas.Loc
module Tx = Kcas.Tx

let parallel_cmp_benchmark n_iter =
  let barrier = Barrier.make 2 in

  let a = Loc.make 10 and b = Loc.make 52 in

  let thread1 () =
    let x = Loc.make 0 in
    let tx1 =
      Tx.(
        let* a = get a and* b = get b in
        set x (b - a))
    and tx2 =
      Tx.(
        let* a = get a and* b = get b in
        set x (a + b))
    in
    Barrier.await barrier;
    let start = Unix.gettimeofday () in
    for _ = 1 to n_iter do
      Tx.commit tx1;
      Tx.commit tx2
    done;
    Unix.gettimeofday () -. start
  and thread2 () =
    let y = Loc.make 0 in
    let tx1 =
      Tx.(
        let* a = get a and* b = get b in
        set y (b - a))
    and tx2 =
      Tx.(
        let* a = get a and* b = get b in
        set y (a + b))
    in
    Barrier.await barrier;
    let start = Unix.gettimeofday () in
    for _ = 1 to n_iter do
      Tx.commit tx1;
      Tx.commit tx2
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
