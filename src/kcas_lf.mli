(*
########
Copyright (c) 2017, Nicolas ASSOUAD <nicolas.assouad@ens.fr>
########
*)

type 'a state;;
(** Type of shared memory *)

type t;;
(** Type of compare and swap value *)

val mk_ref : 'a -> 'a state ref;;
(** [mk_ref x] returns a reference on a shared memory ceils containing the value [x] *)

val mk_cas : 'a state ref -> 'a -> 'a -> t
(** [mk_cas a o n] returns a new CAS value, which when performed, updates
    the reference [a] to [n] if the current content of [a] is [o] *)


val casn : t list -> bool;;
(** [casn cas_list] executes a the CAS contained in [cas_list] in a atomic way *)

val casn_read : 'a state ref -> 'a;;
(** [casn_read a] reads the value contained in the memory ceil [a]. *)
