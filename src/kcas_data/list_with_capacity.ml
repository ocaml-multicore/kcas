open Kcas

type 'a t = { capacity : int; length : int; list : 'a list; limit : int }

let empty_unlimited =
  { capacity = Int.max_int; length = 0; list = []; limit = Int.max_int }

let[@inline] make_empty ~capacity =
  if capacity = Int.max_int then empty_unlimited
  else { capacity; length = 0; list = []; limit = capacity }

let[@inline] make ~capacity ~length ~list ~limit =
  { capacity; length; list; limit }

let[@inline] to_rev_elems t = Elems.of_list_rev t.list
let[@inline] is_empty t = t.length = 0
let[@inline] length t = t.length
let[@inline] capacity t = t.capacity
let[@inline] limit t = t.limit
let[@inline] list t = t.list

let[@inline] tl_safe = function
  | { list = []; _ } as t -> t
  | { capacity; length; list = _ :: list; _ } as t ->
      let limit = if capacity = Int.max_int then capacity else t.limit in
      { capacity; length = length - 1; list; limit }

let[@inline] tl_or_retry = function
  | { list = []; _ } -> Retry.later ()
  | { capacity; length; list = _ :: list; _ } as t ->
      let limit = if capacity = Int.max_int then capacity else t.limit in
      { capacity; length = length - 1; list; limit }

let[@inline] hd_opt t = match t.list with [] -> None | x :: _ -> Some x

let[@inline] hd_or_retry t =
  match t.list with [] -> Retry.later () | x :: _ -> x

let[@inline] hd_unsafe t = List.hd t.list

let[@inline] cons_safe x ({ capacity; _ } as t) =
  if capacity = Int.max_int then
    let { length; list; _ } = t in
    { capacity; length = length + 1; list = x :: list; limit = capacity }
  else
    let { length; limit; _ } = t in
    if length < limit then
      let { list; _ } = t in
      { capacity; length = length + 1; list = x :: list; limit }
    else t

let[@inline] cons_or_retry x ({ capacity; _ } as t) =
  if capacity = Int.max_int then
    let { length; list; _ } = t in
    { capacity; length = length + 1; list = x :: list; limit = capacity }
  else
    let { length; limit; _ } = t in
    if length < limit then
      let { list; _ } = t in
      { capacity; length = length + 1; list = x :: list; limit }
    else Retry.later ()

let[@inline] move ({ capacity; _ } as t) =
  if capacity = Int.max_int then empty_unlimited
  else
    let { length; _ } = t in
    if length = 0 then t
    else
      let { limit; _ } = t in
      { capacity; length = 0; list = []; limit = limit - length }

let move_last ({ capacity; _ } as t) =
  if capacity = Int.max_int then empty_unlimited
  else
    let { length; _ } = t in
    let limit = capacity - length in
    if length = 0 && t.limit = limit then t
    else { capacity; length = 0; list = []; limit }

let[@inline] clear ({ capacity; _ } as t) =
  if capacity = Int.max_int then empty_unlimited
  else if t.length = 0 && t.limit = capacity then t
  else make_empty ~capacity

let rec prepend_to_seq xs tl =
  match xs with
  | [] -> tl
  | x :: xs -> fun () -> Seq.Cons (x, prepend_to_seq xs tl)

let to_seq { list; _ } = prepend_to_seq list Seq.empty

let rev_prepend_to_seq { length; list; _ } tl =
  if length <= 1 then prepend_to_seq list tl
  else
    let t = ref (`Original list) in
    fun () ->
      let t =
        match !t with
        | `Original t' ->
            (* This is domain safe as the result is always equivalent. *)
            let t' = List.rev t' in
            t := `Reversed t';
            t'
        | `Reversed t' -> t'
      in
      prepend_to_seq t tl ()

let of_list ?(capacity = Int.max_int) list =
  let length = List.length list in
  let limit = Int.min 0 (capacity - length) in
  { capacity; length; list; limit }

let of_seq_rev ?capacity xs =
  of_list ?capacity (Seq.fold_left (fun xs x -> x :: xs) [] xs)
