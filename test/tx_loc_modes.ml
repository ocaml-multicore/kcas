(* This example is a simple test of the lock-free mode of Kcas.One thread reads each location whereas the other thread increments the value at each location.
   Threads synched with Barrier*)

module Loc = Kcas.Loc
module Mode = Kcas.Mode
module Tx = Kcas.Tx

let lock = Barrier.make 2
let loop_count = try int_of_string Sys.argv.(1) with _ -> 1500

let mode =
  try if Sys.argv.(2) = "lock-free" then Some Mode.lock_free else None
  with _ -> None
(* ... *)

let a = Loc.make ?mode 10
let b = Loc.make ~mode:Mode.lock_free 10

let read loc loc2 =
  Tx.(let* t1 = Tx.get loc2 in

      let* _ = Tx.get loc in

      Tx.set loc2 (t1 + 1))

let incr loc v =
  Tx.(
    let* v' = Tx.get loc in
    Tx.set loc (v' + v))

let thread1 () =
  Barrier.await lock;
  for _ = 1 to loop_count do
    let alp = incr a 1 in

    Tx.commit alp
  done

let thread2 () =
  Barrier.await lock;
  for _ = 1 to loop_count do
    let beta = read a b in

    Tx.commit beta
  done

let _ = [ thread2; thread1 ] |> List.map Domain.spawn |> List.iter Domain.join
