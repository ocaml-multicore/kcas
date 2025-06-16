open Kcas

module Elems = struct
  type 'a t = { value : 'a; tl : 'a t; length : int }

  let rec empty = { value = Obj.magic (); tl = empty; length = 0 }
  let[@inline] length t = t.length lxor (t.length asr (Sys.int_size - 1))

  let rec rev_append length t tl =
    if length = 0 then tl
    else rev_append (length - 1) t.tl { value = t.value; tl; length }

  let rec head i t = if i = -2 then t.value else head (i + 1) t.tl
  let[@inline] head t = if t.length < 0 then head t.length t else t.value

  let[@inline] tl t =
    if -2 <= t.length then t.tl
    else
      let length = lnot t.length - 1 in
      rev_append (length - 1) t.tl { value = t.value; tl = empty; length }

  let[@inline] peek t =
    if -2 <= t.length then t
    else
      let length = lnot t.length in
      rev_append (length - 1) t.tl { value = t.value; tl = empty; length }

  let rec prepend_to_seq t tl =
    (* TODO: handle reverse! *)
    if t == empty then tl
    else fun () -> Seq.Cons (t.value, prepend_to_seq t.tl tl)
end

module Back = struct
  type 'a t = { length : int; front : 'a; elems : 'a Elems.t }

  let empty = { length = -1; front = Obj.magic (); elems = Elems.empty }
  let[@inline] length t = lnot t.length

  let[@inline] snoc x t =
    let length = t.length in
    if length = -1 then { length = length - 1; front = x; elems = Elems.empty }
    else
      {
        length = length - 1;
        front = t.front;
        elems = { value = x; tl = t.elems; length };
      }

  let rev_prepend_to_seq t tl =
    let tl =
      if t.length >= -2 then Elems.prepend_to_seq t.elems tl
      else
        let t = ref (Either.Left t.elems) in
        fun () ->
          let t =
            match !t with
            | Left t' ->
                (* This is parallelism safe as the result is always equivalent. *)
                let t' = Elems.rev_append (lnot t'.length) t' Elems.empty in
                t := Right t';
                t'
            | Right t' -> t'
          in
          Elems.prepend_to_seq t tl ()
    in
    if t.length <= -2 then fun () -> Seq.Cons (t.front, tl) else tl
end

type 'a t = { front : 'a Elems.t Loc.t; back : 'a Back.t Loc.t }

let alloc ~front ~back =
  let front = Loc.make ~padded:true front in
  let back = Loc.make ~padded:true back in
  Multicore_magic.copy_as_padded { front; back }

let create () = alloc ~front:Elems.empty ~back:Back.empty

let copy t =
  let tx ~xt = (Xt.get ~xt t.front, Xt.get ~xt t.back) in
  let front, back = Xt.commit { tx } in
  alloc ~front ~back

module Xt = struct
  let is_empty ~xt t =
    Xt.get ~xt t.front == Elems.empty && Xt.get ~xt t.back == Back.empty

  let length ~xt t =
    Elems.length (Xt.get ~xt t.front) + Back.length (Xt.get ~xt t.back)

  let add ~xt x t = Xt.modify ~xt t.back @@ Back.snoc x
  let push = add

  let peek_opt ~xt t =
    let front = Xt.update ~xt t.front Elems.peek in
    if front.length = 0 then
      let back = Xt.get ~xt t.back in
      if back.length = -1 then None else Some back.front
    else Some (Elems.head front)

  let peek_blocking ~xt t =
    let front = Xt.update ~xt t.front Elems.peek in
    if front.length = 0 then
      let back = Xt.get ~xt t.back in
      if back.length = -1 then Retry.later () else back.front
    else Elems.head front

  let take_opt ~xt t =
    let front = Xt.update ~xt t.front Elems.tl in
    if front.length = 0 then
      let back = Xt.exchange ~xt t.back Back.empty in
      if back.length = -1 then None
      else begin
        if back.length <> -2 then Xt.set ~xt t.front back.elems;
        Some back.front
      end
    else Some (Elems.head front)

  let take_blocking ~xt t =
    let front = Xt.update ~xt t.front Elems.tl in
    if front.length = 0 then
      let back = Xt.exchange ~xt t.back Back.empty in
      if back.length = -1 then Retry.later ()
      else begin
        if back.length <> -2 then Xt.set ~xt t.front back.elems;
        back.front
      end
    else Elems.head front

  let clear ~xt t =
    Xt.set ~xt t.front Elems.empty;
    Xt.set ~xt t.back Back.empty

  let swap ~xt t1 t2 =
    let front = Xt.get ~xt t1.front and back = Xt.get ~xt t1.back in
    let front = Xt.exchange ~xt t2.front front
    and back = Xt.exchange ~xt t2.back back in
    Xt.set ~xt t1.front front;
    Xt.set ~xt t1.back back

  let seq_of ~front ~back =
    Seq.empty |> Back.rev_prepend_to_seq back |> Elems.prepend_to_seq front

  let to_seq ~xt t =
    let front = Xt.get ~xt t.front and back = Xt.get ~xt t.back in
    seq_of ~front ~back

  let take_all ~xt t =
    let front = Xt.exchange ~xt t.front Elems.empty
    and back = Xt.exchange ~xt t.back Back.empty in
    seq_of ~front ~back
end

let is_empty t = Kcas.Xt.commit { tx = Xt.is_empty t }
let length t = Kcas.Xt.commit { tx = Xt.length t }

let add x t =
  (* Fenceless is safe as we always update. *)
  Loc.fenceless_modify t.back @@ Back.snoc x

let push = add

let take_opt t =
  (* Fenceless is safe as we revert to a transaction in case we didn't update. *)
  let front = Loc.fenceless_update t.front Elems.tl in
  if front.length = 0 then Kcas.Xt.commit { tx = Xt.take_opt t }
  else Some (Elems.head front)

let take_blocking ?timeoutf t =
  (* Fenceless is safe as we revert to a transaction in case we didn't update. *)
  let front = Loc.fenceless_update t.front Elems.tl in
  if front.length = 0 then Kcas.Xt.commit ?timeoutf { tx = Xt.take_blocking t }
  else Elems.head front

let peek_opt t =
  (* Fenceless is safe as we revert to a transaction in case we didn't update. *)
  let front = Loc.fenceless_update t.front Elems.peek in
  if front.length = 0 then Kcas.Xt.commit { tx = Xt.peek_opt t }
  else Some (Elems.head front)

let peek_blocking ?timeoutf t =
  (* Fenceless is safe as we revert to a transaction in case we didn't update. *)
  let front = Loc.fenceless_update t.front Elems.peek in
  if front.length = 0 then Kcas.Xt.commit ?timeoutf { tx = Xt.peek_blocking t }
  else Elems.head front

let take_all t = Kcas.Xt.commit { tx = Xt.take_all t }
let clear t = Kcas.Xt.commit { tx = Xt.clear t }
let swap t1 t2 = Kcas.Xt.commit { tx = Xt.swap t1 t2 }
let to_seq t = Kcas.Xt.commit { tx = Xt.to_seq t }
let iter f t = Seq.iter f @@ to_seq t
let fold f a t = Seq.fold_left f a @@ to_seq t

exception Empty

let[@inline] of_option = function None -> raise Empty | Some value -> value
let peek t = peek_opt t |> of_option
let top = peek
let take t = take_opt t |> of_option
