open Kcas

module Bounded = struct
  type 'a t = { capacity : int; queue : 'a Queue.t }

  let create capacity = { capacity; queue = Queue.create () }
  let capacity_of bounded = bounded.capacity
  let is_empty bounded = Queue.is_empty bounded.queue
  let length bounded = Queue.length bounded.queue

  let add bounded value =
    let tx ~xt =
      Retry.unless (Queue.Xt.length ~xt bounded.queue < bounded.capacity);
      Queue.Xt.add ~xt value bounded.queue
    in
    Xt.commit { tx }

  let take bounded = Queue.take_blocking bounded.queue
  let take_nonblocking bounded = Queue.take_opt bounded.queue
end

module Unbounded = struct
  type 'a t = 'a Queue.t

  let create () = Queue.create ()
  let capacity_of _ = Int.max_int
  let is_empty = Queue.is_empty
  let length = Queue.length
  let add stream value = Queue.add value stream
  let take = Queue.take_blocking
  let take_nonblocking = Queue.take_opt
end

type 'a t =
  | Ch of 'a Ch.t
  | Mvar of 'a Mvar.t
  | Bounded of 'a Bounded.t
  | Unbounded of 'a Unbounded.t

let create capacity =
  if capacity <= 1 then
    if capacity < 0 then invalid_arg "capacity < 0"
    else if capacity = 0 then Ch (Ch.create ())
    else Mvar (Mvar.create None)
  else if capacity = Int.max_int then Unbounded (Unbounded.create ())
  else Bounded (Bounded.create capacity)

let capacity_of = function
  | Ch _ -> 0
  | Mvar _ -> 1
  | Bounded bounded -> Bounded.capacity_of bounded
  | Unbounded unbounded -> Unbounded.capacity_of unbounded

let is_empty = function
  | Ch _ -> true
  | Mvar mvar -> Mvar.is_empty mvar
  | Bounded bounded -> Bounded.is_empty bounded
  | Unbounded unbounded -> Unbounded.is_empty unbounded

let length = function
  | Ch _ -> 0
  | Mvar mvar -> Bool.to_int (not (Mvar.is_empty mvar))
  | Bounded bounded -> Bounded.length bounded
  | Unbounded unbounded -> Unbounded.length unbounded

let add = function
  | Ch ch -> Ch.give ch
  | Mvar mvar -> Mvar.put mvar
  | Bounded bounded -> Bounded.add bounded
  | Unbounded unbounded -> Unbounded.add unbounded

let take = function
  | Ch ch -> Ch.take ch
  | Mvar mvar -> Mvar.take mvar
  | Bounded bounded -> Bounded.take bounded
  | Unbounded unbounded -> Unbounded.take unbounded

let take_nonblocking = function
  | Ch ch -> Ch.take_opt ch
  | Mvar mvar -> Mvar.take_opt mvar
  | Bounded bounded -> Bounded.take_nonblocking bounded
  | Unbounded unbounded -> Unbounded.take_nonblocking unbounded
