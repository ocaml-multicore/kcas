module type Ops = sig
  type t
  type ('x, 'fn) fn

  val signal : ('x, t -> unit) fn
  (**  *)

  val broadcast : ('x, t -> unit) fn
  (**  *)
end
