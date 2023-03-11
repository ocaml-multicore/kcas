(* This example is a simple test of the lock-free mode of Kcas.One thread reads each location whereas the other thread increments the value at each location.
   Using sleep*)

module Loc = Kcas.Loc
module Mode = Kcas.Mode
module Tx = Kcas.Tx

let loop_count = try int_of_string Sys.argv.(1) with _ -> 150

let mode =
  try if Sys.argv.(2) = "lock-free" then Some Mode.lock_free else None
  with _ -> None
(* ... *)

let a = Loc.make ?mode 4
let read loc = Tx.get loc

let incr loc v =
  Tx.(
    let* v' = Tx.get loc in
    Tx.set loc (v' + v))

let sleep_time = 0.01

let thread1 () =
  for _ = 1 to loop_count do
    let alp = incr a 1 in

    Unix.sleepf sleep_time;
    Tx.commit alp
  done

let thread2 () =
  for _ = 1 to loop_count do
    let beta = read a in

    Unix.sleepf sleep_time;
    Printf.printf "%d" @@ Tx.commit beta
  done

let _ = [ thread1; thread2 ] |> List.map Domain.spawn |> List.iter Domain.join
