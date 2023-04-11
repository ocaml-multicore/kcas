open Kcas

(** Condition variable. *)

(** {1 Common interface} *)

type t
(** *)

val create : unit -> t
(** *)

(** {1 Compositional interface} *)

module Xt :
  Condition_intf.Ops
    with type t := t
    with type ('x, 'fn) fn := xt:'x Xt.t -> 'fn

(** {1 Non-compositional interface} *)

include Condition_intf.Ops with type t := t with type ('x, 'fn) fn := 'fn

val await : t -> Mutex.t -> unit
(** *)

val await_no_mutex : t -> unit
(** *)

val broadcast : t -> unit
(** *)
