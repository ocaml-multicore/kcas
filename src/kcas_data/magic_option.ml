type 'a t = 'a

let none = ref ()
let none = Obj.magic none

external some : 'a -> 'a t = "%identity"

let is_none x = x == none [@@inline]
let is_some x = x != none [@@inline]
let get_or_retry x = if is_none x then Kcas.Retry.later () else x

external get_unsafe : 'a t -> 'a = "%identity"

let to_option x = if is_none x then None else Some x
