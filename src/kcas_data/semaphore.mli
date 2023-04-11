open Kcas

(** *)

(** {1 Common interface} *)

type t
(** *)

val make : int -> t
(** *)

(** {1 Compositional interface} *)

module Xt :
  Semaphore_intf.Ops
    with type t := t
    with type ('x, 'fn) fn := xt:'x Xt.t -> 'fn
(** Explicit transaction log passing on semaphores. *)

(** {1 Non-compositional interface} *)

include Semaphore_intf.Ops with type t := t with type ('x, 'fn) fn := 'fn
