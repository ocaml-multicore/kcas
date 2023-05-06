open Kcas

type 'a t

val create : unit -> 'a t
val is_empty : xt:'x Xt.t -> 'a t -> bool
val push_front : xt:'x Xt.t -> 'a t -> 'a -> unit
val push_back : xt:'x Xt.t -> 'a t -> 'a -> unit
val pop_front : xt:'x Xt.t -> 'a t -> 'a option
