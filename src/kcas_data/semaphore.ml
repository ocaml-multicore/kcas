open Kcas

let non_neg_dec n = if 0 < n then n - 1 else n

type t = { count : int Loc.t; waiters : Waiters.t }

let make n =
  if n < 0 then invalid_arg "n < 0";
  let count = Loc.make n and waiters = Waiters.create () in
  { count; waiters }

let get_value sem = Loc.get sem.count

module Xt = struct
  let get_value ~xt sem = Xt.get ~xt sem.count

  let release ~xt sem =
    Waiters.Xt.signal ~xt sem.waiters ~on_none:(Xt.incr sem.count)

  let acquire ~xt sem = Retry.unless (0 < Xt.fetch_and_add ~xt sem.count (-1))
end

let release sem = Kcas.Xt.commit { tx = Xt.release sem }

let acquire sem =
  let tx ~xt =
    if 0 < Kcas.Xt.update ~xt sem.count non_neg_dec then None
    else Some (Waiters.Xt.enqueue ~xt sem.waiters)
  in
  Kcas.Xt.commit { tx }
  |> Option.iter (Waiters.await ~on_cancel:release ~the:sem)
