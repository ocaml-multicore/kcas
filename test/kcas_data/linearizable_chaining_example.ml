(** This demonstrates an approach to composing non-blocking linearizable data
    structures inspired by the paper

      Concurrent Size
      by Gal Sela and Erez Petrank
      https://arxiv.org/pdf/2209.07100.pdf

    First a [Hashtbl] is implemented that allows [idempotent_add] and
    [idempotent_remove] operations to be specified.  The hash table makes sure
    that any operations that might witness the addition or the removal of a key
    will perform those operations before returning.

    Then a [Hashtbl_with_order] is implemented on top of the [Hashtbl] by
    specifying the [idempotent_add] and [idempotent_remove] operation such that
    they update a lock-free doubly-linked list to maintain a list of the keys in
    the hash table in insertion [order].  In other words, we composed a hash
    table with a doubly-linked list, both lock-free and linearizable, resulting
    in a lock-free linearizable hash table that maintains the insertion order.

    Finally a STM tests is used test against linearizability violations.

    Note that this technique does not require the use of Kcas or software
    transactional memory, but Kcas makes it easy to demonstrate the technique,
    because it makes it easy to implement idempotent non-blocking operations
    based on existing non-blocking data structures, such as the doubly-linked
    list used in this example. *)

open Kcas
open Kcas_data

module type Hashtbl_base = sig
  type (!'k, !'v) t

  val find_opt : ('k, 'v) t -> 'k -> 'v option
  val add : ('k, 'v) t -> 'k -> 'v -> bool
  val remove : ('k, 'v) t -> 'k -> bool
end

module Hashtbl : sig
  include Hashtbl_base

  val create :
    ?idempotent_add:('k -> 'v -> ('k, 'v) t -> unit) ->
    ?idempotent_remove:('k -> 'v -> ('k, 'v) t -> unit) ->
    unit ->
    ('k, 'v) t
end = struct
  type ('k, 'v) t = {
    idempotent_add : 'k -> 'v -> ('k, 'v) t -> unit;
    idempotent_remove : 'k -> 'v -> ('k, 'v) t -> unit;
    hashtbl : ('k, ('k, 'v) value) Hashtbl.t;
  }

  and ('k, 'v) value =
    | Add of { event : ('k, 'v) t -> unit; value : 'v }
    | Remove of { event : ('k, 'v) t -> unit }

  let create ?(idempotent_add = fun _ _ _ -> ())
      ?(idempotent_remove = fun _ _ _ -> ()) () =
    let hashtbl = Hashtbl.create () in
    { idempotent_add; idempotent_remove; hashtbl }

  let find_opt t key =
    match Hashtbl.find_opt t.hashtbl key with
    | None -> None
    | Some (Add r) ->
        r.event t;
        Some r.value
    | Some (Remove r) ->
        r.event t;
        None

  let add t key value =
    let event = t.idempotent_add key value in
    let value = Add { event; value } in
    let tx ~xt =
      begin
        match Hashtbl.Xt.find_opt ~xt t.hashtbl key with
        | None -> true
        | Some (Add r) ->
            r.event t;
            false
        | Some (Remove r) ->
            r.event t;
            true
      end
      && begin
           Hashtbl.Xt.replace ~xt t.hashtbl key value;
           true
         end
    in
    Xt.commit { tx }
    && begin
         event t;
         true
       end

  let remove t key =
    let tx ~xt =
      begin
        match Hashtbl.Xt.find_opt ~xt t.hashtbl key with
        | None -> false
        | Some (Add r) ->
            r.event t;
            let event = t.idempotent_remove key r.value in
            let value = Remove { event } in
            Hashtbl.Xt.replace ~xt t.hashtbl key value;
            true
        | Some (Remove r) ->
            r.event t;
            false
      end
    in
    Xt.commit { tx }
    &&
    let tx ~xt =
      match Hashtbl.Xt.find_opt ~xt t.hashtbl key with
      | None -> ()
      | Some (Add _) -> ()
      | Some (Remove r) ->
          r.event t;
          Hashtbl.Xt.remove ~xt t.hashtbl key
    in
    Xt.commit { tx };
    true
end

module Hashtbl_with_order : sig
  include Hashtbl_base

  val create : unit -> ('k, 'v) t
  val order : ('k, 'v) t -> 'k list
end = struct
  type ('k, 'v) t = {
    table : ('k, 'k Dllist.node * 'v) Hashtbl.t;
    order : 'k Dllist.t;
  }

  let create () =
    let order = Dllist.create () in
    let idempotent_add _key (node, _value) =
      let node = Loc.make (Some node) in
      let tx ~xt =
        match Xt.exchange ~xt node None with
        | None -> ()
        | Some node -> Dllist.Xt.move_l ~xt node order
      in
      fun _table -> Xt.commit { tx }
    in
    let idempotent_remove _key (node, _value) =
      let node = Loc.make (Some node) in
      let tx ~xt =
        match Xt.exchange ~xt node None with
        | None -> ()
        | Some node -> Dllist.Xt.remove ~xt node
      in
      fun _table -> Xt.commit { tx }
    in
    let table = Hashtbl.create ~idempotent_add ~idempotent_remove () in
    { table; order }

  let find_opt t key =
    Hashtbl.find_opt t.table key |> Option.map (fun (_, v) -> v)

  let add t key value = Hashtbl.add t.table key (Dllist.create_node key, value)
  let remove t key = Hashtbl.remove t.table key
  let order t = Dllist.to_list_l t.order
end

module Spec = struct
  type cmd = Add of int | Remove of int | Order

  let show_cmd = function
    | Add key -> "Add " ^ string_of_int key
    | Remove key -> "Remove " ^ string_of_int key
    | Order -> "Order"

  type state = int list
  type sut = (int, unit) Hashtbl_with_order.t

  let arb_cmd _s =
    QCheck.(
      [
        (* Generate keys in small range so that [remove] hits. *)
        Gen.int_range 1 5 |> Gen.map (fun key -> Add key);
        Gen.int_range 1 5 |> Gen.map (fun key -> Remove key);
        Gen.return Order;
      ]
      |> Gen.oneof |> make ~print:show_cmd)

  let init_state = []
  let init_sut () = Hashtbl_with_order.create ()
  let cleanup _ = ()

  let next_state c s =
    match c with
    | Add key -> if List.for_all (( != ) key) s then key :: s else s
    | Remove key -> List.filter (( != ) key) s
    | Order -> s

  let precond _ _ = true

  let run c d =
    let open STM in
    match c with
    | Add key -> Res (bool, Hashtbl_with_order.add d key ())
    | Remove key -> Res (bool, Hashtbl_with_order.remove d key)
    | Order -> Res (list int, Hashtbl_with_order.order d)

  let postcond c (s : state) res =
    let open STM in
    match (c, res) with
    | Add key, Res ((Bool, _), res) -> res = List.for_all (( != ) key) s
    | Remove key, Res ((Bool, _), res) -> res = List.exists (( == ) key) s
    | Order, Res ((List Int, _), res) -> res = s
    | _, _ -> false
end

let () =
  Stm_run.run ~count:1000 ~verbose:true ~name:"Hashtbl_with_order" (module Spec)
  |> exit
