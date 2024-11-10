open Kcas
open Kcas_data

type ('k, 'v) t = {
  space : int Loc.t;
  table : ('k, 'k Dllist.node * 'v) Hashtbl.t;
  order : 'k Dllist.t;
}

let check_capacity capacity =
  if capacity < 0 then invalid_arg "Lru_cache: capacity must be non-negative"

let create ?hashed_type capacity =
  check_capacity capacity;
  {
    space = Loc.make capacity;
    table = Hashtbl.create ?hashed_type ();
    order = Dllist.create ();
  }

module Xt = struct
  let capacity_of ~xt c = Xt.get ~xt c.space + Hashtbl.Xt.length ~xt c.table

  let set_capacity ~xt c new_capacity =
    check_capacity new_capacity;
    let old_length = Hashtbl.Xt.length ~xt c.table in
    let old_space = Xt.get ~xt c.space in
    let old_capacity = old_space + old_length in
    for _ = 1 to old_length - new_capacity do
      Dllist.Xt.take_blocking_r ~xt c.order |> Hashtbl.Xt.remove ~xt c.table
    done;
    Xt.set ~xt c.space (Int.max 0 (old_space + new_capacity - old_capacity))

  let get_opt ~xt c key =
    Hashtbl.Xt.find_opt ~xt c.table key
    |> Option.map @@ fun (node, datum) ->
       Dllist.Xt.move_l ~xt node c.order;
       datum

  let set_blocking ~xt c key datum =
    let node =
      match Hashtbl.Xt.find_opt ~xt c.table key with
      | None ->
          if 0 = Xt.update ~xt c.space (fun n -> Int.max 0 (n - 1)) then
            Dllist.Xt.take_blocking_r ~xt c.order
            |> Hashtbl.Xt.remove ~xt c.table;
          Dllist.Xt.add_l ~xt key c.order
      | Some (node, _) ->
          Dllist.Xt.move_l ~xt node c.order;
          node
    in
    Hashtbl.Xt.replace ~xt c.table key (node, datum)

  let remove ~xt c key =
    Hashtbl.Xt.find_opt ~xt c.table key
    |> Option.iter @@ fun (node, _) ->
       Hashtbl.Xt.remove ~xt c.table key;
       Dllist.Xt.remove ~xt node;
       Xt.incr ~xt c.space
end

let capacity_of c = Kcas.Xt.commit { tx = Xt.capacity_of c }
let set_capacity c n = Kcas.Xt.commit { tx = Xt.set_capacity c n }
let get_opt c k = Kcas.Xt.commit { tx = Xt.get_opt c k }
let set_blocking c k v = Kcas.Xt.commit { tx = Xt.set_blocking c k v }
let remove c k = Kcas.Xt.commit { tx = Xt.remove c k }
