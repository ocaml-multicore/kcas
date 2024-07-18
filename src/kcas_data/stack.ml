open Kcas

type 'a t = 'a List_with_capacity.t Loc.t

let create ?(capacity = Int.max_int) () =
  Loc.make ~padded:true (List_with_capacity.make_empty ~capacity)

let copy s = Loc.make ~padded:true (Loc.get s)

let of_seq ?(capacity = Int.max_int) xs =
  Loc.make ~padded:true (List_with_capacity.of_seq_rev ~capacity xs)

module Xt = struct
  let length ~xt s = List_with_capacity.length (Xt.get ~xt s)
  let is_empty ~xt s = List_with_capacity.is_empty (Xt.get ~xt s)
  let push ~xt x s = Xt.modify ~xt s (List_with_capacity.cons_or_retry x)

  let try_push ~xt x s =
    let lwc = Xt.update ~xt s (List_with_capacity.cons_safe x) in
    List_with_capacity.length lwc < List_with_capacity.capacity lwc

  let pop_opt ~xt s =
    Xt.update ~xt s List_with_capacity.tl_safe |> List_with_capacity.hd_opt

  let pop_all ~xt s =
    List_with_capacity.to_seq (Xt.update ~xt s List_with_capacity.clear)

  let pop_blocking ~xt s =
    Xt.update ~xt s List_with_capacity.tl_or_retry
    |> List_with_capacity.hd_unsafe

  let top_opt ~xt s = List_with_capacity.hd_opt (Xt.get ~xt s)
  let top_blocking ~xt s = List_with_capacity.hd_or_retry (Xt.get ~xt s)
  let clear ~xt s = Xt.modify ~xt s List_with_capacity.clear
  let swap ~xt s1 s2 = Xt.swap ~xt s1 s2
  let to_seq ~xt s = List_with_capacity.to_seq (Xt.get ~xt s)
end

let length s = List_with_capacity.length (Loc.get s)
let is_empty s = List_with_capacity.is_empty (Loc.get s)

let push x s =
  (* Fenceless is safe as we always update. *)
  Loc.modify s (List_with_capacity.cons_or_retry x)

let try_push x s =
  let lwc = Loc.update s (List_with_capacity.cons_safe x) in
  List_with_capacity.length lwc < List_with_capacity.capacity lwc

let pop_opt s =
  List_with_capacity.hd_opt (Loc.update s List_with_capacity.tl_safe)

let pop_all s =
  List_with_capacity.to_seq (Loc.update s List_with_capacity.clear)

let pop_blocking ?timeoutf s =
  (* Fenceless is safe as we always update. *)
  Loc.fenceless_update ?timeoutf s List_with_capacity.tl_or_retry
  |> List_with_capacity.hd_unsafe

let top_opt s = List_with_capacity.hd_opt (Loc.get s)

let top_blocking ?timeoutf s =
  Loc.get_as ?timeoutf List_with_capacity.hd_or_retry s

let clear s = Loc.modify s List_with_capacity.clear
let swap s1 s2 = Kcas.Xt.commit { tx = Kcas.Xt.swap s1 s2 }
let to_seq s = List_with_capacity.to_seq (Loc.get s)
let iter f s = List.iter f (List_with_capacity.list (Loc.get s))
let fold f a s = List.fold_left f a (List_with_capacity.list (Loc.get s))

exception Empty

let[@inline] of_option = function None -> raise Empty | Some value -> value
let top s = top_opt s |> of_option
let pop s = pop_opt s |> of_option
