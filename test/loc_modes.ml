module Loc = Kcas.Loc
module Mode = Kcas.Mode


let a = Loc.make ~mode:Mode.obstruction_free 1 
let b = Loc.make ~mode:Mode.lock_free 2

let _ = Printf.printf "Testing loc_mos.ml\n"
let _ = Printf.printf "a = %d\n" (Loc.get a)
let _ = Printf.printf "b = %d\n" (Loc.get b)

let mode_printer l = 
  if l == Mode.lock_free then Printf.printf "lock_free\n"
  else Printf.printf "obstruction_free\n"


let _ = mode_printer @@ Loc.get_mode a  
let _ = mode_printer @@ Loc.get_mode b 


let _ = Loc.set a 3
let _ = Loc.set b 4

let _ = Printf.printf "a = %d\n" (Loc.get a)
let _ = Printf.printf "b = %d\n" (Loc.get b)
     