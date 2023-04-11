open Kcas

type 'a t = { front : 'a node Loc.t; back : 'a node Loc.t }
and 'a node = Nil | Node of 'a node Loc.t * 'a

let create () = { front = Loc.make Nil; back = Loc.make Nil }
let is_empty ~xt queue = Xt.get ~xt queue.front == Nil

let push_front ~xt queue value =
  let next = Loc.make Nil in
  let node = Node (next, value) in
  match Xt.exchange ~xt queue.front node with
  | Nil -> Xt.set ~xt queue.back node
  | succ -> Xt.set ~xt next succ

let push_back ~xt queue value =
  let node = Node (Loc.make Nil, value) in
  match Xt.exchange ~xt queue.back node with
  | Nil -> Xt.set ~xt queue.front node
  | Node (next, _) -> Xt.set ~xt next node

let pop_front ~xt queue =
  match Xt.get ~xt queue.front with
  | Nil -> None
  | Node (next, value) -> (
      match Xt.get ~xt next with
      | Nil ->
          Xt.set ~xt queue.front Nil;
          Xt.set ~xt queue.back Nil;
          Some value
      | node ->
          Xt.set ~xt queue.front node;
          Some value)
