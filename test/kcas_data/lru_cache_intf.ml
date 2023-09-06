module type Ops = sig
  type ('k, 'v) t
  type ('x, 'fn) fn
  type ('x, 'fn) blocking_fn

  val capacity_of : ('x, ('k, 'v) t -> int) fn
  val set_capacity : ('x, ('k, 'v) t -> int -> unit) fn
  val get_opt : ('x, ('k, 'v) t -> 'k -> 'v option) fn
  val set_blocking : ('x, ('k, 'v) t -> 'k -> 'v -> unit) blocking_fn
  val remove : ('x, ('k, 'v) t -> 'k -> unit) fn
end
