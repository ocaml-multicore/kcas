type 'a t

val create : unit -> 'a t
val is_empty : xt:'x Kcas.Xt.t -> 'a t -> bool
val push_front : xt:'x Kcas.Xt.t -> 'a t -> 'a -> unit
val push_back : xt:'x Kcas.Xt.t -> 'a t -> 'a -> unit
val pop_front_opt : xt:'x Kcas.Xt.t -> 'a t -> 'a option
