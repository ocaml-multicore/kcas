module Loc = Kcas.Loc
module Xt = Kcas.Xt

type 'a t = {
  back : 'a list Loc.t;
  middle : 'a list Loc.t;
  front : 'a list Loc.t;
}

let create () =
  let back = Loc.make [] and middle = Loc.make [] and front = Loc.make [] in
  { back; middle; front }

let is_empty ~xt q =
  Xt.get ~xt q.back == []
  && Xt.get ~xt q.middle == []
  && Xt.get ~xt q.front == []

let push_front ~xt q x = Xt.modify ~xt q.front @@ List.cons x
let push_back ~xt q x = Xt.modify ~xt q.back @@ List.cons x

let back_to_middle q =
  let tx ~xt =
    match Xt.exchange ~xt q.back [] with
    | [] -> raise Not_found
    | xs -> if Xt.exchange ~xt q.middle xs != [] then raise Not_found
  in
  try Xt.commit { tx } with Not_found -> ()

let pop_front_opt ~xt q =
  match Xt.update ~xt q.front @@ function [] -> [] | _ :: xs -> xs with
  | x :: _ -> Some x
  | [] -> (
      back_to_middle q;
      match Xt.exchange ~xt q.middle [] |> List.rev with
      | x :: xs ->
          Xt.set ~xt q.front xs;
          Some x
      | [] -> (
          match Xt.exchange ~xt q.back [] |> List.rev with
          | x :: xs ->
              Xt.set ~xt q.front xs;
              Some x
          | [] -> None))
