type t = Waiters.t

let create = Waiters.create
let signal = Waiters.signal
let broadcast = Waiters.broadcast

let await cond mutex =
  let self = Waiters.enqueue cond in
  Mutex.unlock mutex;
  Fun.protect
    (fun () -> Waiters.await self ~on_cancel:signal ~the:cond)
    ~finally:(fun () ->
      (* TODO: This should be protected also from cancellation *)
      Mutex.lock mutex)

let await_no_mutex cond =
  Waiters.enqueue cond |> Waiters.await ~on_cancel:signal ~the:cond

module Xt = struct
  let signal = Waiters.Xt.signal ~on_none:(fun ~xt:_ -> ())
  let broadcast = Waiters.Xt.broadcast
end
