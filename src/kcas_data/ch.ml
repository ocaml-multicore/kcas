open Kcas

(* TODO: The semantics are not quite right here *)

type 'a t = {
  givers : 'a option Loc.t Queue.t;
  takers : [ `Accepting | `Offer of 'a | `Finished ] Loc.t Queue.t;
}

let create () =
  let givers = Queue.create () and takers = Queue.create () in
  { givers; takers }

module Xt = struct
  let rec try_give ~xt ch value =
    match Queue.Xt.take_opt ~xt ch.takers with
    | None -> false
    | Some slot -> (
        match Xt.compare_and_swap ~xt slot `Accepting (`Offer value) with
        | `Accepting -> true
        | `Finished -> try_give ~xt ch value
        | `Offer _ -> Retry.later ())

  let rec take_opt ~xt ch =
    match Queue.Xt.take_opt ~xt ch.givers with
    | None -> None
    | Some slot -> (
        match Xt.exchange ~xt slot None with
        | None -> take_opt ~xt ch
        | Some _ as offer -> offer)
end

let give ch value =
  let tx ~xt =
    if Xt.try_give ~xt ch value then None
    else
      let offer = Some value in
      let slot = Loc.make offer in
      Queue.Xt.add ~xt slot ch.givers;
      Some (slot, offer)
  in
  match Kcas.Xt.commit { tx } with
  | None -> ()
  | Some (slot, offer) -> (
      try Loc.get_as (fun offer -> Retry.unless (offer == None)) slot
      with exn ->
        Loc.compare_and_set slot offer None |> ignore;
        raise exn)

let rec take ch =
  let tx ~xt =
    match Xt.take_opt ~xt ch with
    | None ->
        let slot = Loc.make `Accepting in
        Queue.Xt.add ~xt slot ch.takers;
        `Block slot
    | Some value -> `Offer value
  in
  match Kcas.Xt.commit { tx } with
  | `Offer value -> value
  | `Block slot -> (
      let tx ~xt =
        match Kcas.Xt.exchange ~xt slot `Finished with
        | `Offer value -> Some value
        | `Finished -> None
        | `Accepting -> Retry.later ()
      in
      match Kcas.Xt.commit { tx } with
      | None -> take ch
      | Some value -> value
      | exception exn ->
          Loc.compare_and_set slot `Accepting `Finished |> ignore;
          raise exn)

let take_opt ch = Kcas.Xt.commit { tx = Xt.take_opt ch }
