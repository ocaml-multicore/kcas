type 'a t

val create : unit -> 'a t
val is_empty : 'a t -> bool Kcas.Tx.t
val push : 'a t -> 'a -> unit Kcas.Tx.t
val pop : 'a t -> 'a Kcas.Tx.t
