module type Ops = sig
  type 'a t
  type 'a res
  type ('x, 'fn) fn

  val is_empty : ('x, 'a t -> bool res) fn
  (** [is_empty s] determines whether the stack [s] is empty. *)

  val length : ('x, 'a t -> int res) fn
  (** [length s] returns the length of the stack [s]. *)

  val clear : ('x, 'a t -> unit res) fn
  (** [clear s] removes all elements from the stack [s]. *)

  val swap : ('x, 'a t -> 'a t -> unit res) fn
  (** [swap s1 s2] exchanges the contents of the stacks [s1] and [s2]. *)

  val to_seq : ('x, 'a t -> 'a Seq.t res) fn
  (** [to_seq s] returns a domain safe sequence for iterating through the
      elements of the stack top to bottom.

      The sequence is based on a constant time, [O(1)], snapshot of the stack
      and modifications of the stack have no effect on the sequence. *)

  val push : ('x, 'a -> 'a t -> unit res) fn
  (** [push x s] adds the element [x] to the top of the stack [s]. *)

  val pop_opt : ('x, 'a t -> 'a option res) fn
  (** [pop_opt s] removes and returns the topmost element of the stack [s], or
      [None] if the stack is empty. *)

  val top_opt : ('x, 'a t -> 'a option res) fn
  (** [top_opt s] returns the topmost element in stack [s], or [None] if the
      stack is empty. *)
end
