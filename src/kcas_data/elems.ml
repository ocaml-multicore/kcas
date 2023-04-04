type 'a t = { value : 'a; tl : 'a t; length : int }

let rec empty = { value = Obj.magic (); tl = empty; length = 0 }
let singleton value = { value; tl = empty; length = 1 } [@@inline]
let tl_safe { tl; _ } = tl [@@inline]
let length { length; _ } = length [@@inline]
let cons value tl = { value; tl; length = 1 + tl.length } [@@inline]
let hd_opt t = if t != empty then Some t.value else None [@@inline]
let rec fold f a t = if t == empty then a else fold f (f a t.value) t.tl
let iter f t = fold (fun () x -> f x) () t [@@inline]

let rec rev_append t tl =
  if t == empty then tl else rev_append t.tl @@ cons t.value tl

let rev t = if t.length <= 1 then t else rev_append t.tl (singleton t.value)

let rec prepend_to_seq t tl =
  if t == empty then tl else fun () -> Seq.Cons (t.value, prepend_to_seq t.tl tl)

let to_seq t = prepend_to_seq t Seq.empty
let of_seq_rev xs = Seq.fold_left (fun t x -> cons x t) empty xs

let rev_prepend_to_seq t tl =
  if t.length <= 1 then prepend_to_seq t tl
  else
    let t = ref (`Original t) in
    fun () ->
      let t =
        match !t with
        | `Original t' ->
            (* This is domain safe as the result is always equivalent. *)
            let t' = rev t' in
            t := `Reversed t';
            t'
        | `Reversed t' -> t'
      in
      prepend_to_seq t tl ()
