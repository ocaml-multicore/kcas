(*
########
Copyright (c) 2017, Nicolas ASSOUAD <nicolas.assouad@ens.fr>
########
*)

(** Type of shared memory reference *)
type 'a ref;;

(** Type of compare and swap value *)
type t;;

(** The type of CAS result. *)
type 'a cas_result =
  | Aborted
  | Failed
  | Success of 'a
;;

(** [ref x] returns a reference on a shared memory ceils containing the value [x] *)
val ref : 'a -> 'a ref;;

val equal : 'a ref -> 'b ref -> bool;;

val is_on_ref : t -> 'a ref -> bool;;

(** [mk_cas a o n] returns a new CAS value, which when performed, updates
    the reference [a] to [n] if the current content of [a] is [o] *)
val mk_cas : 'a ref -> 'a -> 'a -> t;;

(** [set r n] updates the reference [r] to value [n] directly. Not Safe to use with
    shared memory ! *)
val set : 'a ref -> 'a -> unit;;

(** [cas r e u] updates the reference [r] to value [u] if the current content
    of [r] is [e]. *)
val cas : 'a ref -> 'a -> 'a -> bool;;

(** [commit c] performs the CAS [c] and returns [true] if the CAS is successful. *)
val commit : t -> bool;;

(** [kCAS l] performs a lock-free multi-word CAS and returns [true] if the
    multi-word CAS is successful. *)
val kCAS : t list -> bool;;

(** [get a] reads the value contained in the memory ceil [a]. *)
val get : 'a ref -> 'a;;

(** [try_map r f] invokes [f c], where [c] is the result of [get r]. If the
    result of [f c] is [None], then [Aborted] is returned. If the result of [f c]
    is [Some v], then attempt to CAS update [r] from [c] to [v]. If the CAS
    succeeds, then [Success c] is returned. If the CAS fails, then [Failed] is
    returned. *)
val try_map : 'a ref -> ('a -> 'a option) -> 'a cas_result;;

(** Like {!try_map} but retries on CAS failure. Hence, [map r f] never returns
    [Failed]. *)
val map : 'a ref -> ('a -> 'a option) -> 'a cas_result;;

(** [incr r] atomically increments [r] *)
val incr : int ref -> unit;;

(** [decr r] atomically decrements [r] *)
val decr : int ref -> unit;;

(** {2 Backoff}
    Suspend domains with exponential backoff. *)
module type Backoff = sig
  (** The type of backoff value *)
  type t;;

  (** [create ~max:maxv ()] returns a backoff value, which when waited upon,
      suspends the calling domain for [x] milliseconds, where [x] is the
      current value of the backoff. The backoff value [x] is doubled after
      every wait upto a maximum of [maxv] milliseconds. The default maximum is
      32 milliseconds. The initial backoff is 1 millisecond. *)
  val create : ?max:int -> unit -> t;;
 
  (** [once b] suspends the current domain for [x] milliseconds, where [x] is
      the current value of the backoff. *)
  val once : t -> unit;;
  
  (** Resets the backoff clock to 1 millisecond. *)
  val reset : t -> unit;;
end

module Backoff : Backoff;;

(** {2 Single-word Compare-and-swap}

    References optimized for single-word (traditional) CAS operation. The
    internal representation of a single word CAS reference is more efficient
    than that of a multi-word CAS reference. *)
module type W1 = sig
  (** The type of shared memory reference. *)
  type 'a ref;;

  (** Create a new reference. *)
  val ref : 'a -> 'a ref;;

  (** Get the value of the reference. *)
  val get : 'a ref -> 'a;;

  val set : 'a ref -> 'a -> unit;;

  (** [cas r e u] updates the reference [r] to value [u] if the current content
      of [r] is [e]. *)
  val cas : 'a ref -> 'a -> 'a -> bool;;
 
  (** [try_map r f] invokes [f c], where [c] is the result of [get r]. If the
      result of [f c] is [None], then [Aborted] is returned. If the result of [f c]
      is [Some v], then attempt to CAS update [r] from [c] to [v]. If the CAS
      succeeds, then [Success c] is returned. If the CAS fails, then [Failed] is
      returned. *)
  val try_map : 'a ref -> ('a -> 'a option) -> 'a cas_result;;

  (** Like {!try_map} but retries on CAS failure. Hence, [map r f] never returns
      [Failed]. *)
  val map : 'a ref -> ('a -> 'a option) -> 'a cas_result;;

  (** [incr r] atomically increments [r] *)
  val incr : int ref -> unit;;

  val decr : int ref -> unit;;
end

module W1 : W1;;
