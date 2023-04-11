open Kcas

type 'a t
(** Type of a synchronous channel. *)

val create : unit -> 'a t
(** [create ()] returns a new synchronous channel. *)

module Xt : sig
  val try_give : xt:'x Xt.t -> 'a t -> 'a -> bool
  (** *)

  val take_opt : xt:'x Xt.t -> 'a t -> 'a option
  (** *)
end

val give : 'a t -> 'a -> unit
(** *)

val take : 'a t -> 'a
(** *)

val take_opt : 'a t -> 'a option
(** *)
