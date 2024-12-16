open Kcas

(** This is a naïve transactional leftist heap implementation.

    ⚠️ This simply cannot scale, because essentially every delete min operation
    updates the root. This can still be a useful benchmark of the transaction
    mechanism, but please do not believe for a second that this is a scalable
    concurrent priority queue. *)
module Leftist_heap : sig
  type 'a t

  val create : lt:('a -> 'a -> bool) -> 'a t

  module Xt : sig
    val insert : xt:'x Xt.t -> 'a t -> 'a -> unit
    val delete_min_opt : xt:'x Xt.t -> 'a t -> 'a option
  end

  val insert : 'a t -> 'a -> unit
  val delete_min_opt : 'a t -> 'a option
end = struct
  type 'a t = { lt : 'a -> 'a -> bool; root : 'a link Loc.t }

  and ('a, _) tdt =
    | Null : ('a, [> `Null ]) tdt
    | Node :
        'a link Loc.t * int Loc.t * 'a * 'a link Loc.t
        -> ('a, [> `Node ]) tdt

  and 'a link = Link : ('a, [< `Null | `Node ]) tdt -> 'a link [@@unboxed]

  let create ~lt =
    let root = Loc.make ~padded:true (Link Null) in
    Multicore_magic.copy_as_padded { lt; root }

  module Xt = struct
    let npl_of ~xt = function
      | Link Null -> 0
      | Link (Node (_, npl, _, _)) -> Xt.get ~xt npl

    let rec merge ~xt ~lt h1 h2 =
      match (h1, h2) with
      | Link Null, h2 -> h2
      | h1, Link Null -> h1
      | Link (Node (_, _, v1, _) as h1), Link (Node (_, _, v2, _) as h2) ->
          let (Node (h1l, npl, _, h1r) as h1 : (_, [ `Node ]) tdt), h2 =
            if lt v1 v2 then (h1, h2) else (h2, h1)
          in
          let l = Xt.get ~xt h1l in
          if l == Link Null then Xt.set ~xt h1l (Link h2)
          else begin
            let r = merge ~xt ~lt (Xt.get ~xt h1r) (Link h2) in
            match (npl_of ~xt l, npl_of ~xt r) with
            | l_npl, r_npl when l_npl < r_npl ->
                Xt.set ~xt h1l r;
                Xt.set ~xt h1r l;
                Xt.set ~xt npl (l_npl + 1)
            | _, r_npl ->
                Xt.set ~xt h1r r;
                Xt.set ~xt npl (r_npl + 1)
          end;
          Link h1

    let insert ~xt h x =
      let h1 =
        Node (Loc.make (Link Null), Loc.make 1, x, Loc.make (Link Null))
      in
      Xt.set ~xt h.root (merge ~xt ~lt:h.lt (Link h1) (Xt.get ~xt h.root))

    let delete_min_opt ~xt h =
      match Xt.get ~xt h.root with
      | Link Null -> None
      | Link (Node (h1, _, x, h2)) ->
          Xt.set ~xt h.root (merge ~xt ~lt:h.lt (Xt.get ~xt h1) (Xt.get ~xt h2));
          Some x
  end

  let insert h x = Kcas.Xt.commit { tx = Xt.insert h x }
  let delete_min_opt h = Kcas.Xt.commit { tx = Xt.delete_min_opt h }
end

open Multicore_bench

let run_one ~budgetf ~n_domains ~preload () =
  let n_ops =
    Float.to_int
      (Float.of_int (50 * Util.iter_factor) /. Float.log2 (Float.of_int preload))
  in

  let t = Leftist_heap.create ~lt:(( < ) : int -> int -> bool) in

  let n_ops_todo = Countdown.create ~n_domains () in

  let before () =
    Countdown.non_atomic_set n_ops_todo n_ops;
    while Option.is_some @@ Leftist_heap.delete_min_opt t do
      ()
    done;
    let state = Random.State.make_self_init () in
    for _ = 1 to preload do
      Leftist_heap.insert t (Random.State.bits state)
    done
  in
  let init _ = Random.State.make_self_init () in
  let work domain_index state =
    let rec work () =
      let n = Countdown.alloc n_ops_todo ~domain_index ~batch:100 in
      if n <> 0 then
        let rec loop n =
          if 0 < n then
            let value = Random.State.bits state in
            if value land 1 = 0 then begin
              Leftist_heap.insert t value;
              loop (n - 1)
            end
            else begin
              Leftist_heap.delete_min_opt t |> ignore;
              loop (n - 1)
            end
          else work ()
        in
        loop n
    in
    work ()
  in

  let config =
    Printf.sprintf "%d worker%s, %d preload" n_domains
      (if n_domains = 1 then "" else "s")
      preload
  in

  Times.record ~budgetf ~n_domains ~before ~init ~work ()
  |> Times.to_thruput_metrics ~n:n_ops ~singular:"operation" ~config

let run_suite ~budgetf =
  Util.cross [ 10; 100; 1000 ] [ 1; 2; 4 ]
  |> List.concat_map @@ fun (preload, n_domains) ->
     run_one ~budgetf ~n_domains ~preload ()
