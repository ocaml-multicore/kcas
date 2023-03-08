(* Location is in obstruction_free mode
   Each Thread increments the value at a and reads a in a loop*)

module Loc = Kcas.Loc
module Mode = Kcas.Mode
module Tx = Kcas.Tx

let a = Loc.make ~mode:Mode.obstruction_free 4
let read_loc loc = Tx.(return @@ Loc.get loc)
let incr_loc loc v = Tx.(return @@ Loc.fetch_and_add loc v)
let loop_count = 750
let sleep_time = 0.01

let thread1 () =
  for _ = 1 to loop_count do
    let alp = incr_loc a 1 in

    let _ = Printf.printf "%d 1 i\n" @@ Tx.commit alp in

    Unix.sleepf sleep_time
  done

let thread2 () =
  for _ = 1 to loop_count do
    let beta = read_loc a in

    let () = Printf.printf "%d 2 r\n" @@ Tx.commit beta in

    Unix.sleepf sleep_time
  done

let _ = [ thread1; thread2 ] |> List.map Domain.spawn |> List.iter Domain.join
