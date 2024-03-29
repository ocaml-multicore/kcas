open Kcas

type 'a t = 'a Magic_option.t Loc.t

let create x_opt = Loc.make ~padded:true (Magic_option.of_option x_opt)

module Xt = struct
  let is_empty ~xt mv = Magic_option.is_none (Xt.get ~xt mv)

  let try_put ~xt mv value =
    Magic_option.is_none
      (Xt.compare_and_swap ~xt mv Magic_option.none (Magic_option.some value))

  let put ~xt mv value = Xt.modify ~xt mv (Magic_option.put_or_retry value)

  let take_opt ~xt mv =
    Magic_option.to_option (Xt.exchange ~xt mv Magic_option.none)

  let take ~xt mv =
    Magic_option.get_unsafe (Xt.update ~xt mv Magic_option.take_or_retry)

  let peek ~xt mv = Magic_option.get_or_retry (Xt.get ~xt mv)
  let peek_opt ~xt mv = Magic_option.to_option (Xt.get ~xt mv)
end

let is_empty mv = Magic_option.is_none (Loc.get mv)

let put ?timeoutf mv value =
  (* Fenceless is safe as we always update. *)
  Loc.fenceless_modify ?timeoutf mv (Magic_option.put_or_retry value)

let try_put mv value =
  Loc.compare_and_set mv Magic_option.none (Magic_option.some value)

let take ?timeoutf mv =
  (* Fenceless is safe as we always update. *)
  Magic_option.get_unsafe
    (Loc.fenceless_update ?timeoutf mv Magic_option.take_or_retry)

let take_opt mv = Magic_option.to_option (Loc.exchange mv Magic_option.none)
let peek ?timeoutf mv = Loc.get_as ?timeoutf Magic_option.get_or_retry mv
let peek_opt mv = Magic_option.to_option (Loc.get mv)
