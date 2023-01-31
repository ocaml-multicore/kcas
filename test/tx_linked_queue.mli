type 'a t

val create : unit -> 'a t
val is_empty : 'a t -> bool Kcas.Tx.t
val push_front : 'a t -> 'a -> unit Kcas.Tx.t
val push_back : 'a t -> 'a -> unit Kcas.Tx.t
val pop_front : 'a t -> 'a Kcas.Tx.t
