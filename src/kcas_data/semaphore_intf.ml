module type Ops = sig
  type t
  type ('x, 'fn) fn

  val release : ('x, t -> unit) fn
  (** *)

  val acquire : ('x, t -> unit) fn
  (** *)

  val get_value : ('x, t -> int) fn
  (** *)
end
