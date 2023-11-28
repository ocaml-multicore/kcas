open Kcas

(** Scalable accumulator.

    A scalable accumulator can be used to scalably accumulate an integer value
    in parallel as long as the accumulated value is read infrequently. *)

(** {1 Common interface} *)

type t
(** The type of a scalable accumulator. *)

val make : int -> t
(** [make n] returns a new accumulator whose initial value is [n]. *)

(** {1 Compositional interface} *)

module Xt :
  Accumulator_intf.Ops
    with type t := t
    with type ('x, 'fn) fn := xt:'x Xt.t -> 'fn
(** Explicit transaction log passing on accumulators. *)

(** {1 Non-compositional interface} *)

include Accumulator_intf.Ops with type t := t with type ('x, 'fn) fn := 'fn
