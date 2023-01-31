module Loc = Kcas.Loc
module Tx = Kcas.Tx

type 'a t = 'a list Loc.t

let create () = Loc.make []
let is_empty s = s |> Tx.get_as @@ ( == ) []
let push s x = Tx.modify s @@ List.cons x

let pop s =
  Tx.update_as List.hd s @@ function [] -> raise Exit | _ :: xs -> xs
