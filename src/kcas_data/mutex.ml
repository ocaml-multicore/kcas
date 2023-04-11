open Kcas

type t = {
  state : [ `Unlocked | `Locked | `Poisoned of exn ] Loc.t;
  waiters : Waiters.t;
}

exception Poisoned of exn

let poisoned exn = raise @@ Poisoned exn [@@inline never]

let create () =
  let state = Loc.make `Unlocked and waiters = Waiters.create () in
  { state; waiters }

let unlock mutex =
  let tx ~xt =
    match Xt.get ~xt mutex.state with
    | `Locked ->
        Waiters.Xt.signal ~xt mutex.waiters
          ~on_none:(Xt.set mutex.state `Unlocked);
        None
    | `Unlocked ->
        let exn = Sys_error "Mutex.unlock: already unlocked!" in
        Xt.set ~xt mutex.state @@ `Poisoned exn;
        Some exn
    | `Poisoned exn -> Some (Poisoned exn)
  in
  Xt.commit { tx } |> Option.iter raise

let lock mutex =
  let tx ~xt =
    match Xt.compare_and_swap ~xt mutex.state `Unlocked `Locked with
    | `Unlocked -> None
    | `Locked -> Some (Waiters.Xt.enqueue ~xt mutex.waiters)
    | `Poisoned exn -> poisoned exn
  in
  Xt.commit { tx } |> Option.iter (Waiters.await ~on_cancel:unlock ~the:mutex)

let try_lock mutex =
  match
    Loc.update mutex.state @@ function
    | `Unlocked -> `Locked
    | (`Locked | `Poisoned _) as other -> other
  with
  | `Unlocked -> true
  | `Locked -> false
  | `Poisoned exn -> poisoned exn

let poison mutex exn =
  match
    Loc.update mutex.state @@ function
    | `Locked | `Unlocked -> `Poisoned exn
    | `Poisoned _ as poisoned -> poisoned
  with
  | `Locked | `Unlocked -> Waiters.broadcast mutex.waiters
  | `Poisoned _ -> ()

let use_rw mutex thunk =
  lock mutex;
  match thunk () with
  | value ->
      unlock mutex;
      value
  | exception exn ->
      poison mutex exn;
      raise exn

let use_ro mutex thunk =
  lock mutex;
  Fun.protect ~finally:(fun () -> unlock mutex) thunk

module Xt = struct
  let lock ~xt mutex =
    match Xt.compare_and_swap ~xt mutex.state `Unlocked `Locked with
    | `Unlocked -> ()
    | `Locked -> Retry.later ()
    | `Poisoned exn -> poisoned exn

  let try_lock ~xt mutex =
    match Xt.compare_and_swap ~xt mutex.state `Unlocked `Locked with
    | `Unlocked -> true
    | `Locked -> false
    | `Poisoned exn -> poisoned exn

  let unlock ~xt mutex =
    match Xt.get ~xt mutex.state with
    | `Locked ->
        Waiters.Xt.signal ~xt mutex.waiters
          ~on_none:(Xt.set mutex.state `Unlocked)
    | `Unlocked ->
        let exn = Sys_error "Mutex.unlock: already unlocked!" in
        Xt.set ~xt mutex.state @@ `Poisoned exn;
        raise exn
    | `Poisoned exn -> poisoned exn
end
