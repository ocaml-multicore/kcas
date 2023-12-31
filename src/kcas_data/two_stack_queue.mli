type !'a t
(** *)

val create : unit -> 'a t
(** *)

val push : 'a t -> 'a -> unit
(** *)

exception Empty
(** *)

val pop : 'a t -> 'a
(** *)

val pop_opt : 'a t -> 'a option
(** *)

val length : 'a t -> int
(** *)
