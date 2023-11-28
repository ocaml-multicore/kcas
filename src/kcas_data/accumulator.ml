open Kcas

type t = { mutable cache : int Loc.t array; truth : int Loc.t array Loc.t }

let make n =
  let cs = Loc.make_array ~padded:true ~mode:`Lock_free 1 0 in
  Loc.set (Array.unsafe_get cs 0) n;
  let truth = Loc.make ~padded:true cs in
  Multicore_magic.copy_as_padded { cache = cs; truth }

let[@inline never] rec get_self a i cs n =
  let add_cs = Loc.make_array ~padded:true ~mode:`Lock_free (n + 1) 0 in
  let new_cs =
    (* The length of [new_cs] will be a power of two minus 1, which means the
       whole heap block will have a power of two number of words, which may help
       to keep it cache line aligned. *)
    Array.init ((n * 2) + 1) @@ fun i ->
    if i <= n then Array.unsafe_get add_cs i else Array.unsafe_get cs (i - n - 1)
  in
  if Loc.compare_and_set a.truth cs new_cs then a.cache <- new_cs;
  let cs = a.cache in
  let n = Array.length cs in
  if i < n then Array.unsafe_get cs i else get_self a i cs n

let[@inline] get_self a =
  let i = Multicore_magic.instantaneous_domain_index () in
  let cs = a.cache in
  let n = Array.length cs in
  if i < n then Array.unsafe_get cs i else get_self a i cs n

module Xt = struct
  let add ~xt a n = if n <> 0 then Xt.fetch_and_add ~xt (get_self a) n |> ignore
  let incr ~xt a = Xt.incr ~xt (get_self a)
  let decr ~xt a = Xt.decr ~xt (get_self a)

  let rec get_rec ~xt cs s i =
    let s = s + Xt.get ~xt (Array.unsafe_get cs i) in
    if i = 0 then s else get_rec ~xt cs s (i - 1)

  let get ~xt a =
    let cs = Xt.get ~xt a.truth in
    let cs_old = a.cache in
    if cs != cs_old then a.cache <- cs;
    let i = Array.length cs - 1 in
    let s = Xt.get ~xt (Array.unsafe_get cs i) in
    if i = 0 then s else get_rec ~xt cs s (i - 1)

  let set ~xt a n =
    let delta = n - get ~xt a in
    if delta <> 0 then
      Xt.fetch_and_add ~xt (Array.unsafe_get a.cache 0) delta |> ignore
end

let add a n = if n <> 0 then Loc.fetch_and_add (get_self a) n |> ignore
let incr a = Loc.incr (get_self a)
let decr a = Loc.decr (get_self a)
let get a = Kcas.Xt.commit { tx = Xt.get a }
let set a n = Kcas.Xt.commit { tx = Xt.set a n }
