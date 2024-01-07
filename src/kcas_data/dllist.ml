open Kcas

(** Tagged GADT for representing doubly-linked lists.

    The [lhs] and [rhs] fields are the first two fields in both a [List] and a
    [Node] so that it is possible (by using an unsafe cast) to access the fields
    without knowing whether the target is a [List] or a [Node]. *)
type ('a, _) tdt =
  | List : {
      lhs : 'a cursor Loc.t;
          (** [lhs] points to the rightmost node of this list or to the list
              itself in case the list is empty. *)
      rhs : 'a cursor Loc.t;
          (** [rhs] points to the leftmost node of this list or to the list
              itself in case the list is empty. *)
    }
      -> ('a, [> `List ]) tdt
  | Node : {
      lhs : 'a cursor Loc.t;
          (** [lhs] points to the node on the left side of this node, to the
              list if this node is the leftmost node, or to the node itself in
              case this node is not in any list. *)
      rhs : 'a cursor Loc.t;
          (** [rhs] points to the node on the right side of this node, to the
              list if this node is the rightmost node, or to the node itself in
              case this node is not in any list. *)
      value : 'a;
    }
      -> ('a, [> `Node ]) tdt

and 'a cursor = At : ('a, [< `List | `Node ]) tdt -> 'a cursor [@@unboxed]

type 'a t = ('a, [ `List ]) tdt
type 'a node = ('a, [ `Node ]) tdt

external as_list : ('a, _) tdt -> 'a t = "%identity"
external as_node : ('a, _) tdt -> 'a node = "%identity"

let[@inline] get (Node { value; _ } : 'a node) = value

let[@inline] lhs_of list_or_node =
  let (List list_r) = as_list list_or_node in
  list_r.lhs

let[@inline] rhs_of list_or_node =
  let (List list_r) = as_list list_or_node in
  list_r.rhs

let[@inline] value_of (Node node_r : 'a node) = node_r.value

let create () =
  let lhs = Loc.make ~padded:true (Obj.magic ()) in
  let rhs = Loc.make ~padded:true (Obj.magic ()) in
  let list = Multicore_magic.copy_as_padded (List { lhs; rhs }) in
  Loc.set lhs (At list);
  Loc.set rhs (At list);
  list

let create_node value =
  let node =
    let lhs = Loc.make (Obj.magic ()) in
    let rhs = Loc.make (Obj.magic ()) in
    Node { lhs; rhs; value }
  in
  Loc.set (lhs_of node) (At node);
  Loc.set (rhs_of node) (At node);
  node

let create_node_with ~lhs ~rhs value =
  Node { lhs = Loc.make (At lhs); rhs = Loc.make (At rhs); value }

module Xt = struct
  let get_l ~xt (At at) = Xt.get ~xt (lhs_of at)
  let get_r ~xt (At at) = Xt.get ~xt (rhs_of at)

  let remove ~xt node =
    let (At rhs) = Xt.exchange ~xt (rhs_of node) (At node) in
    if At rhs != At node then begin
      let (At lhs) = Xt.exchange ~xt (lhs_of node) (At node) in
      Xt.set ~xt (lhs_of rhs) (At lhs);
      Xt.set ~xt (rhs_of lhs) (At rhs)
    end

  let is_empty ~xt list = Xt.get ~xt (lhs_of list) == At list

  let add_node_l ~xt node list =
    let (At rhs) = Xt.get ~xt (rhs_of list) in
    assert (Loc.fenceless_get (lhs_of node) == At list);
    Loc.set (rhs_of node) (At rhs);
    Xt.set ~xt (rhs_of list) (At node);
    Xt.set ~xt (lhs_of rhs) (At node);
    node

  let add_l ~xt value list =
    let (At rhs) = Xt.get ~xt (rhs_of list) in
    let node = create_node_with ~lhs:list ~rhs value in
    Xt.set ~xt (rhs_of list) (At node);
    Xt.set ~xt (lhs_of rhs) (At node);
    node

  let add_node_r ~xt node list =
    let (At lhs) = Xt.get ~xt (lhs_of list) in
    Loc.set (lhs_of node) (At lhs);
    assert (Loc.fenceless_get (rhs_of node) == At list);
    Xt.set ~xt (lhs_of list) (At node);
    Xt.set ~xt (rhs_of lhs) (At node);
    node

  let add_r ~xt value list =
    let (At lhs) = Xt.get ~xt (lhs_of list) in
    let node = create_node_with ~lhs ~rhs:list value in
    Xt.set ~xt (lhs_of list) (At node);
    Xt.set ~xt (rhs_of lhs) (At node);
    node

  let move_l ~xt node list =
    let (At list_rhs) = Xt.exchange ~xt (rhs_of list) (At node) in
    if At list_rhs != At node then begin
      let (At node_lhs) = Xt.exchange ~xt (lhs_of node) (At list) in
      let (At node_rhs) = Xt.exchange ~xt (rhs_of node) (At list_rhs) in
      if At node_lhs != At node then begin
        Xt.set ~xt (rhs_of node_lhs) (At node_rhs);
        Xt.set ~xt (lhs_of node_rhs) (At node_lhs)
      end;
      Xt.set ~xt (lhs_of list_rhs) (At node)
    end

  let move_r ~xt node list =
    let (At list_lhs) = Xt.exchange ~xt (lhs_of list) (At node) in
    if At list_lhs != At node then begin
      let (At node_rhs) = Xt.exchange ~xt (rhs_of node) (At list) in
      let (At node_lhs) = Xt.exchange ~xt (lhs_of node) (At list_lhs) in
      if At node_rhs != At node then begin
        Xt.set ~xt (rhs_of node_lhs) (At node_rhs);
        Xt.set ~xt (lhs_of node_rhs) (At node_lhs)
      end;
      Xt.set ~xt (rhs_of list_lhs) (At node)
    end

  let take_opt_l ~xt list =
    let (At rhs) = Xt.get ~xt (rhs_of list) in
    if At rhs == At list then None
    else
      let node = as_node rhs in
      remove ~xt node;
      Some (value_of node)

  let take_opt_r ~xt list =
    let (At lhs) = Xt.get ~xt (lhs_of list) in
    if At lhs == At list then None
    else
      let node = as_node lhs in
      remove ~xt node;
      Some (value_of node)

  let take_blocking_l ~xt list = Xt.to_blocking ~xt (take_opt_l list)
  let take_blocking_r ~xt list = Xt.to_blocking ~xt (take_opt_r list)

  let transfer_l ~xt t1 t2 =
    let (At t1_rhs) = Xt.exchange ~xt (rhs_of t1) (At t1) in
    if At t1_rhs != At t1 then begin
      let (At t1_lhs) = Xt.exchange ~xt (lhs_of t1) (At t1) in
      let (At t2_rhs) = Xt.exchange ~xt (rhs_of t2) (At t1_rhs) in
      Xt.set ~xt (lhs_of t2_rhs) (At t1_lhs);
      Xt.set ~xt (lhs_of t1_rhs) (At t2);
      Xt.set ~xt (rhs_of t1_lhs) (At t2_rhs)
    end

  let transfer_r ~xt t1 t2 =
    let (At t1_rhs) = Xt.exchange ~xt (rhs_of t1) (At t1) in
    if At t1_rhs != At t1 then begin
      let (At t1_lhs) = Xt.exchange ~xt (lhs_of t1) (At t1) in
      let (At t2_lhs) = Xt.exchange ~xt (lhs_of t2) (At t1_lhs) in
      Xt.set ~xt (rhs_of t2_lhs) (At t1_rhs);
      Xt.set ~xt (rhs_of t1_lhs) (At t2);
      Xt.set ~xt (lhs_of t1_rhs) (At t2_lhs)
    end

  let swap ~xt t1 t2 =
    let (At t1_rhs) = Xt.get ~xt (rhs_of t1) in
    if At t1_rhs == At t1 then transfer_l ~xt t2 t1
    else
      let (At t2_lhs) = Xt.get ~xt (lhs_of t2) in
      if At t2_lhs == At t2 then transfer_l ~xt t1 t2
      else
        let (At t1_lhs) = Xt.exchange ~xt (lhs_of t1) (At t2_lhs) in
        let (At t2_rhs) = Xt.exchange ~xt (rhs_of t2) (At t1_rhs) in
        Xt.set ~xt (lhs_of t2) (At t1_lhs);
        Xt.set ~xt (rhs_of t1) (At t2_rhs);
        Xt.set ~xt (lhs_of t2_rhs) (At t1);
        Xt.set ~xt (rhs_of t2_lhs) (At t1);
        Xt.set ~xt (lhs_of t1_rhs) (At t2);
        Xt.set ~xt (rhs_of t1_lhs) (At t2)

  let[@tail_mod_cons] rec to_list_as_l ~xt f list (At at) =
    if At at == At list then []
    else f (as_node at) :: to_list_as_l ~xt f list (Xt.get ~xt (rhs_of at))

  let to_list_as_l ~xt f list =
    to_list_as_l ~xt f list (Xt.get ~xt (rhs_of list))

  let to_list_l ~xt list = to_list_as_l ~xt get list
  let to_nodes_l ~xt list = to_list_as_l ~xt Fun.id list

  let[@tail_mod_cons] rec to_list_as_r ~xt f list (At at) =
    if At at == At list then []
    else f (as_node at) :: to_list_as_r ~xt f list (Xt.get ~xt (lhs_of at))

  let to_list_as_r ~xt f list =
    to_list_as_r ~xt f list (Xt.get ~xt (lhs_of list))

  let to_list_r ~xt list = to_list_as_r ~xt get list
  let to_nodes_r ~xt list = to_list_as_r ~xt Fun.id list
end

let get_l (At at) = Loc.get (lhs_of at)
let get_r (At at) = Loc.get (rhs_of at)
let remove node = Kcas.Xt.commit { tx = Xt.remove node }
let is_empty list = Loc.get (lhs_of list) == At list

let add_l value list =
  let node = create_node_with ~lhs:list ~rhs:list value in
  Kcas.Xt.commit { tx = Xt.add_node_l node list }

let add_r value list =
  let node = create_node_with ~lhs:list ~rhs:list value in
  Kcas.Xt.commit { tx = Xt.add_node_r node list }

let move_l node list = Kcas.Xt.commit { tx = Xt.move_l node list }
let move_r node list = Kcas.Xt.commit { tx = Xt.move_r node list }
let take_opt_l list = Kcas.Xt.commit { tx = Xt.take_opt_l list }
let take_opt_r list = Kcas.Xt.commit { tx = Xt.take_opt_r list }

let take_blocking_l ?timeoutf list =
  Kcas.Xt.commit ?timeoutf { tx = Xt.take_blocking_l list }

let take_blocking_r ?timeoutf list =
  Kcas.Xt.commit ?timeoutf { tx = Xt.take_blocking_r list }

let swap t1 t2 = Kcas.Xt.commit { tx = Xt.swap t1 t2 }
let transfer_l t1 t2 = Kcas.Xt.commit { tx = Xt.transfer_l t1 t2 }
let transfer_r t1 t2 = Kcas.Xt.commit { tx = Xt.transfer_r t1 t2 }
let to_list_l list = Kcas.Xt.commit { tx = Xt.to_list_l list }
let to_list_r list = Kcas.Xt.commit { tx = Xt.to_list_r list }
let to_nodes_l list = Kcas.Xt.commit { tx = Xt.to_nodes_l list }
let to_nodes_r list = Kcas.Xt.commit { tx = Xt.to_nodes_r list }

exception Empty

let take_l list = match take_opt_l list with None -> raise Empty | Some v -> v
let take_r list = match take_opt_r list with None -> raise Empty | Some v -> v

let take_all list =
  let copy =
    let lhs = Loc.make ~padded:true (At list) in
    let rhs = Loc.make ~padded:true (At list) in
    List { lhs; rhs } |> Multicore_magic.copy_as_padded
  in
  let open Kcas in
  let tx ~xt =
    let (At lhs) = Xt.exchange ~xt (lhs_of list) (At list) in
    if At lhs == At list then begin
      Loc.set (lhs_of copy) (At copy);
      Loc.set (rhs_of copy) (At copy)
    end
    else
      let (At rhs) = Xt.exchange ~xt (rhs_of list) (At list) in
      Xt.set ~xt (rhs_of lhs) (At copy);
      Xt.set ~xt (lhs_of rhs) (At copy);
      Loc.set (lhs_of copy) (At lhs);
      Loc.set (rhs_of copy) (At rhs)
  in
  Xt.commit { tx };
  copy
