(** *)

type !'a t
(** *)

val create : int -> 'a t
(** *)

val capacity_of : 'a t -> int
(** *)

val is_empty : 'a t -> bool
(** *)

val length : 'a t -> int
(** *)

val add : 'a t -> 'a -> unit
(** *)

val take : 'a t -> 'a
(** *)

val take_nonblocking : 'a t -> 'a option
(** *)
