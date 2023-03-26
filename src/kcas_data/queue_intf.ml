module type Ops = sig
  type 'a t
  type 'a res
  type ('x, 'fn) fn

  val is_empty : ('x, 'a t -> bool res) fn
  (** [is_empty s] determines whether the queue [q] is empty. *)

  val length : ('x, 'a t -> int res) fn
  (** [length q] returns the length of the queue [q]. *)

  val clear : ('x, 'a t -> unit res) fn
  (** [clear q] removes all elements from the queue [q]. *)

  val swap : ('x, 'a t -> 'a t -> unit res) fn
  (** [swap q1 q2] exchanges the contents of the queues [q1] and [q2]. *)

  val to_seq : ('x, 'a t -> 'a Seq.t res) fn
  (** [to_seq s] returns a domain safe sequence for iterating through the
      elements of the queue front to back.

      The sequence is based on a constant time, [O(1)], snapshot of the queue
      and modifications of the queue have no effect on the sequence. *)

  val add : ('x, 'a -> 'a t -> unit res) fn
  (** [add q x] adds the element [x] at the end of the queue [q]. *)

  val push : ('x, 'a -> 'a t -> unit res) fn
  (** [push] is a synonym for {!add}. *)

  val peek_opt : ('x, 'a t -> 'a option res) fn
  (** [peek_opt q] returns the first element in queue [q], without removing it
      from the queue, or returns [None] if the queue is empty. *)

  val take_opt : ('x, 'a t -> 'a option res) fn
  (** [take_opt q] removes and returns the first element in queue [q], or
      returns [None] if the queue is empty. *)
end
