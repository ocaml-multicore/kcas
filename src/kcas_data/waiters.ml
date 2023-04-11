open Kcas

type t = [ `Awaiting | `Signaled ] Loc.t Queue.t

let create () = Queue.create ()

let await ~on_cancel ~the self =
  try Loc.get_as (fun s -> Retry.unless (s == `Signaled)) self
  with exn ->
    (* We have been canceled, so try to mark as signaled or release another.
       The node is left in the queue and repeated cancellations could cause a
       space leak.  Another alternative would be to use a doubly-linked list. *)
    if not (Loc.compare_and_set self `Awaiting `Signaled) then on_cancel the;
    raise exn

let rec signal waiters =
  Queue.take_opt waiters
  |> Option.iter @@ fun awaiter ->
     if not (Loc.compare_and_set awaiter `Awaiting `Signaled) then
       signal waiters

let broadcast waiters =
  Queue.take_all waiters
  |> Seq.iter @@ fun awaiter ->
     Loc.compare_and_set awaiter `Awaiting `Signaled |> ignore

let enqueue waiters =
  let self = Loc.make `Awaiting in
  Queue.add self waiters;
  self

module Xt = struct
  let enqueue ~xt waiters =
    let self = Loc.make `Awaiting in
    Queue.Xt.add ~xt self waiters;
    self

  let rec signal ~xt ~on_none waiters =
    match Queue.Xt.take_opt ~xt waiters with
    | None -> on_none ~xt
    | Some awaiter ->
        if Xt.get ~xt awaiter != `Signaled then
          Xt.post_commit ~xt @@ fun () -> Loc.set awaiter `Signaled
        else
          (* Apparently the awaiter was canceled, so signal another. *)
          signal ~xt ~on_none waiters

  let broadcast ~xt t =
    let awaiters = Queue.Xt.take_all ~xt t in
    Xt.post_commit ~xt @@ fun () ->
    awaiters
    |> Seq.iter @@ fun awaiter ->
       Loc.compare_and_set awaiter `Awaiting `Signaled |> ignore
end
