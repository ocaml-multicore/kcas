open Kcas

type 'a t = 'a list Loc.t

let create () = Loc.make []
let is_empty ~xt s = Xt.get ~xt s == []
let push ~xt s x = Xt.modify ~xt s @@ List.cons x

let pop_opt ~xt s =
  match Xt.update ~xt s @@ function [] -> [] | _ :: xs -> xs with
  | x :: _ -> Some x
  | [] -> None
