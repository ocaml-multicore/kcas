(*---------------------------------------------------------------------------
   Copyright (c) 2015 Théo Laurent <theo.laurent@ens.fr>
   Copyright (c) 2015 KC Sivaramakrishnan <sk826@cl.cam.ac.uk>
   All rights reserved.  Distributed under the ISC license, see terms at the
   end of the file.  %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Multi-word compare-and-swap library

    {e %%VERSION%% — {{:%%PKG_HOMEPAGE%% }homepage}} *)

(** {1 Kcas} *)

(*---------------------------------------------------------------------------
   Copyright (c) 2015 Théo Laurent <theo.laurent@ens.fr>
   Copyright (c) 2015 KC Sivaramakrishnan <sk826@cl.cam.ac.uk>

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)

(** {2 References} *)

type 'a ref
(** The type of shared memory reference. *)

val ref : 'a -> 'a ref
(** Create a new reference. *)

val get : 'a ref -> 'a
(** Get the value of the reference. *)

val get_id : 'a ref -> int
(** Get the unique identity of the reference. *)

(** {2 Compare-and-swap} *)

type t
(** The type of compare-and-swap value. *)

val mk_cas : 'a ref -> 'a -> 'a -> t
(** [mk_cas r e u] returns a new CAS value, which when performed, updates the
    reference [r] to [u] if the current content of [r] is [e]. *)

val cas : 'a ref -> 'a -> 'a -> bool
(** [cas r e u] updates the reference [r] to value [u] if the current content
    of [r] is [e]. *)

val is_on_ref : t -> 'a ref -> bool
(** [is_on_ref c r] returns [true] if [c] is a CAS on the reference [r]. *)

val commit : t -> bool
(** [commit c] performs the CAS [c] and returns [true] if the CAS is successful. *)

val kCAS : t list -> bool
(** [kCAS l] performs a lock-free multi-word CAS and returns [true] if the
    multi-word CAS is successful. *)

(** The type of CAS result. *)
type 'a cas_result = 
  | Aborted 
  | Failed 
  | Success of 'a

val try_map : 'a ref -> ('a -> 'a option) -> 'a cas_result
(** [try_map r f] invokes [f c], where [c] is the result of [get r]. If the
    result of [f c] is [None], then [Aborted] is returned. If the result of [f c]
    is [Some v], then attempt to CAS update [r] from [c] to [v]. If the CAS
    succeeds, then [Success c] is returned. If the CAS fails, then [Failed] is
    returned. *)

val map : 'a ref -> ('a -> 'a option) -> 'a cas_result
(** Like {!try_map} but retries on CAS failure. Hence, [map r f] never returns
    [Failed]. *)

val incr : int ref -> unit
(** [incr r] atomically increments [r] *)

val decr : int ref -> unit
(** [decr r] atomically decrements [r] *)

(** {2 Backoff} 
 
    Suspend domains with exponential backoff. *)

module type Backoff = sig
  type t
  (** The type of backoff value *)

  val create : ?max:int -> unit -> t
  (** [create ~max:maxv ()] returns a backoff value, which when waited upon,
      suspends the calling domain for [x] milliseconds, where [x] is the
      current value of the backoff. The backoff value [x] is doubled after
      every wait upto a maximum of [maxv] milliseconds. The default maximum is
      32 milliseconds. The initial backoff is 1 millisecond. *)

  val once : t -> unit
  (** [once b] suspends the current domain for [x] milliseconds, where [x] is
      the current value of the backoff. *)

  val reset : t -> unit
  (** Resets the backoff clock to 1 millisecond. *)
end

module Backoff : Backoff
