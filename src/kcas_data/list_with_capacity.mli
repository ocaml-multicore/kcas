type !'a t

val empty_unlimited : 'a t
val make_empty : capacity:int -> 'a t
val make : capacity:int -> length:int -> list:'a list -> limit:int -> 'a t
val is_empty : 'a t -> bool
val length : 'a t -> int
val capacity : 'a t -> int
val limit : 'a t -> int
val list : 'a t -> 'a list
val cons_safe : 'a -> 'a t -> 'a t
val cons_or_retry : 'a -> 'a t -> 'a t
val move : 'a t -> 'a t
val move_last : 'a t -> 'a t
val clear : 'a t -> 'a t
val to_rev_elems : 'a t -> 'a Elems.t
val to_seq : 'a t -> 'a Seq.t
val rev_prepend_to_seq : 'a t -> 'a Seq.t -> 'a Seq.t
val of_seq_rev : ?capacity:int -> 'a Seq.t -> 'a t
val tl_safe : 'a t -> 'a t
val tl_or_retry : 'a t -> 'a t
val hd_opt : 'a t -> 'a option
val hd_or_retry : 'a t -> 'a
val hd_unsafe : 'a t -> 'a
