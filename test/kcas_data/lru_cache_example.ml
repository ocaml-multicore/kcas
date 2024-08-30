open Picos_std_structured
open Kcas

module Lru_cache = struct
  include Lru_cache

  module Xt = struct
    include Xt

    let get ~xt c key = Kcas.Xt.to_blocking ~xt (get_opt c key)

    let get_if ~xt c key predicate =
      let snap = Kcas.Xt.snapshot ~xt in
      let datum = get ~xt c key in
      if predicate datum then datum else Retry.later (Kcas.Xt.rollback ~xt snap)

    let try_set ~xt c key datum =
      match set_blocking ~xt c key datum with
      | () -> true
      | exception Retry.Later -> false
  end

  let get c k = Kcas.Xt.commit { tx = Xt.get c k }
  let get_if c k p = Kcas.Xt.commit { tx = Xt.get_if c k p }
  let try_set c k d = Kcas.Xt.commit { tx = Xt.try_set c k d }
end

let () =
  Scheduler.run ~n_domains:2 @@ fun () ->
  Flock.join_after @@ fun () ->
  let c = Lru_cache.create 10 in
  let answer =
    Flock.fork_as_promise @@ fun () ->
    let tx ~xt = Lru_cache.Xt.get ~xt c "a" + Lru_cache.Xt.get ~xt c "b" in
    Xt.commit { tx }
  in
  Lru_cache.set_blocking c "b" 30;
  Lru_cache.set_blocking c "a" 12;
  assert (Promise.await answer = 42);
  ()

let () =
  let c = Lru_cache.create 10 in
  assert (Lru_cache.try_set c "a" 1);
  Lru_cache.set_blocking c "c" 2;
  assert (Lru_cache.capacity_of c = 10);
  assert (Lru_cache.get_opt c "b" = None);
  assert (Lru_cache.get c "a" = 1);
  Lru_cache.set_capacity c 3;
  assert (Lru_cache.get c "c" = 2);
  Lru_cache.set_capacity c 1;
  assert (Lru_cache.capacity_of c = 1);
  assert (Lru_cache.get_opt c "a" = None);
  assert (Lru_cache.get_if c "c" (( <> ) 0) = 2);
  Lru_cache.remove c "c";
  assert (Lru_cache.get_opt c "c" = None);
  ()

let () = Printf.printf "LRU Cache OK!\n%!"
