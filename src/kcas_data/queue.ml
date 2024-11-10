open Kcas

type 'a t = {
  front : 'a Elems.t Loc.t;
  back : 'a List_with_capacity.t Loc.t;
  middle : 'a List_with_capacity.t Loc.t;
}

let alloc ~front ~back ~middle =
  (* We allocate locations in specific order to make most efficient use of the
     splay-tree based transaction log. *)
  let front = Loc.make ~padded:true front
  and back = Loc.make ~padded:true back
  and middle = Loc.make ~padded:true middle in
  Multicore_magic.copy_as_padded { back; middle; front }

let create ?(capacity = Int.max_int) () =
  if capacity < 0 then invalid_arg "Queue.create: capacity must be non-negative";
  let back = List_with_capacity.make_empty ~capacity in
  alloc ~front:Elems.empty ~back ~middle:List_with_capacity.empty_unlimited

let copy q =
  let tx ~xt = (Xt.get ~xt q.front, Xt.get ~xt q.back, Xt.get ~xt q.middle) in
  let front, back, middle = Xt.commit { tx } in
  alloc ~front ~back ~middle

module Xt = struct
  let is_empty ~xt t =
    (* We access locations in order of allocation to make most efficient use of
       the splay-tree based transaction log. *)
    Xt.get ~xt t.front == Elems.empty
    && List_with_capacity.is_empty (Xt.get ~xt t.back)
    && Xt.get ~xt t.middle == List_with_capacity.empty_unlimited

  let length ~xt q =
    Elems.length (Xt.get ~xt q.front)
    + List_with_capacity.length (Xt.get ~xt q.back)
    + List_with_capacity.length (Xt.get ~xt q.middle)

  let try_add ~xt x q =
    let lwc = Xt.update ~xt q.back (List_with_capacity.cons_safe x) in
    let capacity = List_with_capacity.capacity lwc in
    capacity = Int.max_int
    ||
    let back_length = List_with_capacity.length lwc in
    back_length < List_with_capacity.limit lwc
    ||
    let other_length =
      List_with_capacity.length (Xt.get ~xt q.middle)
      + Elems.length (Xt.get ~xt q.front)
    in
    let limit = capacity - other_length in
    back_length < limit
    &&
    (Xt.set ~xt q.back
       (List_with_capacity.make ~capacity ~length:(back_length + 1)
          ~list:(x :: List_with_capacity.list lwc)
          ~limit);
     true)

  let add ~xt x q = Retry.unless (try_add ~xt x q)
  let push = add

  (** Cooperative helper to move elems from back to middle. *)
  let back_to_middle ~back ~middle =
    let tx ~xt =
      let xs = Xt.update ~xt back List_with_capacity.move in
      if
        List_with_capacity.length xs = 0
        || Xt.exchange ~xt middle xs != List_with_capacity.empty_unlimited
      then raise_notrace Exit
    in
    try Xt.commit { tx } with Exit -> ()

  let take_opt_finish ~xt front lwc =
    let elems = List_with_capacity.to_rev_elems lwc in
    Xt.set ~xt front (Elems.tl_safe elems);
    Elems.hd_opt elems

  let take_opt ~xt t =
    let front = t.front in
    let elems = Xt.update ~xt front Elems.tl_safe in
    if elems != Elems.empty then Elems.hd_opt elems
    else
      let middle = t.middle and back = t.back in
      if not (Xt.is_in_log ~xt middle || Xt.is_in_log ~xt back) then
        back_to_middle ~back ~middle;
      let lwc = Xt.exchange ~xt middle List_with_capacity.empty_unlimited in
      if lwc != List_with_capacity.empty_unlimited then
        take_opt_finish ~xt front lwc
      else
        let lwc = Xt.update ~xt back List_with_capacity.move_last in
        if List_with_capacity.length lwc <> 0 then take_opt_finish ~xt front lwc
        else None

  let take_blocking ~xt q = Xt.to_blocking ~xt (take_opt q)

  let peek_opt_finish ~xt front lwc =
    let elems = List_with_capacity.to_rev_elems lwc in
    Xt.set ~xt front elems;
    Elems.hd_opt elems

  let peek_opt ~xt t =
    let front = t.front in
    let elems = Xt.get ~xt front in
    if elems != Elems.empty then Elems.hd_opt elems
    else
      let middle = t.middle and back = t.back in
      if not (Xt.is_in_log ~xt middle || Xt.is_in_log ~xt back) then
        back_to_middle ~back ~middle;
      let lwc = Xt.exchange ~xt middle List_with_capacity.empty_unlimited in
      if lwc != List_with_capacity.empty_unlimited then
        peek_opt_finish ~xt front lwc
      else
        let lwc = Xt.update ~xt back List_with_capacity.move_last in
        if List_with_capacity.length lwc <> 0 then peek_opt_finish ~xt front lwc
        else None

  let peek_blocking ~xt q = Xt.to_blocking ~xt (peek_opt q)

  let clear ~xt t =
    Xt.set ~xt t.front Elems.empty;
    Xt.modify ~xt t.back List_with_capacity.clear;
    Xt.set ~xt t.middle List_with_capacity.empty_unlimited

  let swap ~xt q1 q2 =
    let front = Xt.get ~xt q1.front
    and back = Xt.get ~xt q1.back
    and middle = Xt.get ~xt q1.middle in
    let front = Xt.exchange ~xt q2.front front
    and back = Xt.exchange ~xt q2.back back
    and middle = Xt.exchange ~xt q2.middle middle in
    Xt.set ~xt q1.front front;
    Xt.set ~xt q1.back back;
    Xt.set ~xt q1.middle middle

  let seq_of ~front ~middle ~back =
    (* Sequence construction is lazy, so this function is O(1). *)
    Seq.empty
    |> List_with_capacity.rev_prepend_to_seq back
    |> List_with_capacity.rev_prepend_to_seq middle
    |> Elems.prepend_to_seq front

  let to_seq ~xt t =
    let front = Xt.get ~xt t.front
    and back = Xt.get ~xt t.back
    and middle = Xt.get ~xt t.middle in
    seq_of ~front ~middle ~back

  let take_all ~xt t =
    let front = Xt.exchange ~xt t.front Elems.empty
    and back = Xt.update ~xt t.back List_with_capacity.clear
    and middle = Xt.exchange ~xt t.middle List_with_capacity.empty_unlimited in
    seq_of ~front ~middle ~back
end

let is_empty q = Kcas.Xt.commit { tx = Xt.is_empty q }
let length q = Kcas.Xt.commit { tx = Xt.length q }

let try_add x q =
  (* Fenceless is safe as we revert to a transaction in case we didn't update. *)
  let lwc = Loc.fenceless_update q.back (List_with_capacity.cons_safe x) in
  let capacity = List_with_capacity.capacity lwc in
  capacity = Int.max_int
  ||
  let back_length = List_with_capacity.length lwc in
  back_length < List_with_capacity.limit lwc
  || Kcas.Xt.commit { tx = Xt.try_add x q }

let add x q =
  (* Fenceless is safe as we revert to a transaction in case we didn't update. *)
  let lwc = Loc.fenceless_update q.back (List_with_capacity.cons_safe x) in
  if List_with_capacity.capacity lwc <> Int.max_int then
    if List_with_capacity.length lwc = List_with_capacity.limit lwc then
      Kcas.Xt.commit { tx = Xt.add x q }

let push = add

let take_opt q =
  (* Fenceless is safe as we revert to a transaction in case we didn't update. *)
  match Loc.fenceless_update q.front Elems.tl_safe |> Elems.hd_opt with
  | None -> Kcas.Xt.commit { tx = Xt.take_opt q }
  | some -> some

let take_blocking ?timeoutf q =
  (* Fenceless is safe as we revert to a transaction in case we didn't update. *)
  match Loc.fenceless_update q.front Elems.tl_safe |> Elems.hd_opt with
  | None -> Kcas.Xt.commit ?timeoutf { tx = Xt.take_blocking q }
  | Some elem -> elem

let take_all q = Kcas.Xt.commit { tx = Xt.take_all q }

let peek_opt q =
  match Loc.get q.front |> Elems.hd_opt with
  | None -> Kcas.Xt.commit { tx = Xt.peek_opt q }
  | some -> some

let peek_blocking ?timeoutf q =
  Kcas.Xt.commit ?timeoutf { tx = Xt.peek_blocking q }

let clear q = Kcas.Xt.commit { tx = Xt.clear q }
let swap q1 q2 = Kcas.Xt.commit { tx = Xt.swap q1 q2 }
let to_seq q = Kcas.Xt.commit { tx = Xt.to_seq q }
let iter f q = Seq.iter f @@ to_seq q
let fold f a q = Seq.fold_left f a @@ to_seq q

exception Empty

let[@inline] of_option = function None -> raise Empty | Some value -> value
let peek s = peek_opt s |> of_option
let top = peek
let take s = take_opt s |> of_option
