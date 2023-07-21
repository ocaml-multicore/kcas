val cpu_relax : unit -> unit
val recommended_domain_count : unit -> int

type 'a t

val spawn : (unit -> 'a) -> 'a t
val join : 'a t -> 'a
