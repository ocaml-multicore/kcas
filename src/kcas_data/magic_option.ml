open Kcas

type 'a t = 'a

let none = ref ()
let none = Obj.magic none

external some : 'a -> 'a t = "%identity"

let[@inline] is_none x = x == none
let[@inline] is_some x = x != none
let[@inline] get_or_retry x = if is_none x then Retry.later () else x
let[@inline] put_or_retry v x = if is_none x then some v else Retry.later ()
let[@inline] take_or_retry x = if is_none x then Retry.later () else none

external get_unsafe : 'a t -> 'a = "%identity"

let[@inline] to_option x = if is_none x then None else Some x
let[@inline] of_option = function None -> none | Some x -> some x
