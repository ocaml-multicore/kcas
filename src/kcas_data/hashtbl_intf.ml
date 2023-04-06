module type Ops = sig
  type ('k, 'v) t
  type 'a res
  type ('x, 'fn) fn

  val length : ('x, ('k, 'v) t -> int res) fn
  (** [length t] returns the number of {i bindings} in the hash table [t].

      ⚠️ The returned value may be greater than the number of {i distinct keys}
      in the hash table. *)

  val reset : ('x, ('k, 'v) t -> unit res) fn
  (** [reset t] remove all bindings from the hash table [t] and shrinks the
      capacity of the table back to the minimum. *)

  val clear : ('x, ('k, 'v) t -> unit res) fn
  (** [clear] is a synonym for {!reset}. *)

  val remove : ('x, ('k, 'v) t -> 'k -> unit res) fn
  (** [remove t k] removes the most recent existing binding of key [k], if any,
      from the hash table [t] thereby revealing the earlier binding of [k], if
      any. *)

  val add : ('x, ('k, 'v) t -> 'k -> 'v -> unit res) fn
  (** [add t k v] adds a binding of key [k] to value [v] to the hash table
      shadowing the previous binding of the key [k], if any.

      ⚠️ Consider using {!replace} instead of [add]. *)

  val replace : ('x, ('k, 'v) t -> 'k -> 'v -> unit res) fn
  (** [replace t k v] adds a binding of key [k] to value [v] or replaces the
      most recent existing binding of key [k] in the hash table [t]. *)

  val mem : ('x, ('k, 'v) t -> 'k -> bool res) fn
  (** [mem t k] is equivalent to [Option.is_some (find_opt t k)]. *)

  val find_opt : ('x, ('k, 'v) t -> 'k -> 'v option res) fn
  (** [find_opt t k] returns the current binding of key [k] in the hash table
      [t], or [None] if no such binding exists. *)

  val find_all : ('x, ('k, 'v) t -> 'k -> 'v list res) fn
  (** [find_all t k] returns a list of all the bindings of the key [k] in the
      hash table in reverse order of their introduction. *)
end