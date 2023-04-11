open Kcas

(** Mutual exclusion. *)

(* TODO: Consider ordering of mutexes *)

(** {1 Common interface} *)

exception Poisoned of exn
(** *)

type t
(** *)

val create : unit -> t
(** *)

(** {1 Compositional interface} *)

module Xt : sig
  val lock : xt:'x Xt.t -> t -> unit
  (** *)

  val try_lock : xt:'x Xt.t -> t -> bool
  (** *)

  val unlock : xt:'x Xt.t -> t -> unit
  (** *)
end

(** {1 Non-compositional interface} *)

val lock : t -> unit
(** *)

val try_lock : t -> bool
(** *)

val unlock : t -> unit
(** *)

val use_rw : t -> (unit -> 'a) -> 'a
(** *)

val use_ro : t -> (unit -> 'a) -> 'a
(** *)
