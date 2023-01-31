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

let pop_front q =
  Tx.(
    get q.front >>= function
    | x :: xs -> set q.front xs >>. x
    | [] -> (
        q.back |> get_as List.rev >>= function
        | [] -> forget
        | x :: xs -> set q.back [] >> set q.front xs >>. x))
