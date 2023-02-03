module Loc = Kcas.Loc
module Tx = Kcas.Tx

type 'a t = { front : 'a list Loc.t; back : 'a list Loc.t }

let create () = { front = Loc.make []; back = Loc.make [] }

let is_empty q =
  Tx.(
    get q.front >>= function
    | _ :: _ -> return false
    | [] -> q.back |> get_as @@ ( == ) [])

let push_front q x = Tx.modify q.front @@ List.cons x
let push_back q x = Tx.modify q.back @@ List.cons x
let tl_safe = function [] -> [] | _ :: xs -> xs

let pop_front q =
  Tx.(
    update q.front tl_safe >>= function
    | x :: _ -> return x
    | [] -> (
        exchange_as List.rev q.back [] >>= function
        | [] -> forget
        | x :: xs -> set q.front xs >>. x))
