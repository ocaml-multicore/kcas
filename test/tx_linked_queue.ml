module Loc = Kcas.Loc
module Tx = Kcas.Tx

type 'a t = { front : 'a node Loc.t; back : 'a node Loc.t }
and 'a node = Nil | Node of 'a node Loc.t * 'a

let create () = { front = Loc.make Nil; back = Loc.make Nil }
let is_empty queue = queue.front |> Tx.get_as @@ ( == ) Nil

let push_front queue value =
  Tx.(
    delay @@ fun () ->
    let next = Loc.make Nil in
    let node = Node (next, value) in
    exchange queue.front node >>= function
    | Nil -> set queue.back node
    | succ -> set next succ)

let push_back queue value =
  Tx.(
    delay @@ fun () ->
    let node = Node (Loc.make Nil, value) in
    exchange queue.back node >>= function
    | Nil -> set queue.front node
    | Node (next, _) -> set next node)

let pop_front queue =
  Tx.(
    get queue.front >>= function
    | Nil -> forget
    | Node (next, value) -> (
        get next >>= function
        | Nil -> set queue.front Nil >> set queue.back Nil >>. value
        | node -> set queue.front node >>. value))
