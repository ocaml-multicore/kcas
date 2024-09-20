open Kcas_data

let[@tail_mod_cons] rec to_list get_lr cursor =
  match get_lr cursor with
  | Dllist.At (List _) -> []
  | Dllist.At (Node _ as node) ->
      Dllist.get node :: to_list get_lr (Dllist.At node)

let to_list get_lr list = to_list get_lr (Dllist.At list)

let[@tail_mod_cons] rec take_as_list take l =
  match take l with None -> [] | Some x -> x :: take_as_list take l

let basics () =
  let t1 = Dllist.create () in
  let t1' = Dllist.take_all t1 in
  assert (Dllist.to_list_r t1 = [] && Dllist.to_list_l t1' = []);
  Dllist.transfer_r t1' t1';
  Dllist.add_r 2 t1' |> ignore;
  Dllist.move_r (Dllist.create_node 3) t1';
  Dllist.swap t1' t1';
  Dllist.add_l 1 t1' |> ignore;
  Dllist.transfer_r t1' t1';
  let t1 = Dllist.take_all t1' in
  assert (Dllist.to_list_l t1' = [] && Dllist.to_list_r t1 = [ 3; 2; 1 ]);
  let t2 = Dllist.create () in
  Dllist.transfer_r t2 t1;
  Dllist.transfer_l t2 t1;
  Dllist.swap t1 t2;
  Dllist.swap t1 t2;
  Dllist.transfer_l t2 t2;
  Dllist.add_r 4 t2 |> ignore;
  Dllist.swap t1 t2;
  Dllist.swap t1 t2;
  Dllist.transfer_l t2 t2;
  Dllist.transfer_l t1 t2;
  Dllist.transfer_l t1 t2;
  Dllist.swap t1 t2;
  assert (Dllist.take_opt_l t2 = None);
  assert (Dllist.take_opt_l t2 = None);
  assert (take_as_list Dllist.take_opt_r t1 = [ 4; 3; 2; 1 ])

let add () =
  let l = Dllist.create () in
  Dllist.add_l 1 l |> ignore;
  Dllist.add_l 3 l |> ignore;
  Dllist.add_r 4 l |> ignore;
  assert (to_list Dllist.get_r l = [ 3; 1; 4 ])

let move () =
  let t1 = Dllist.create () in
  let n1 = Dllist.add_l 5.3 t1 in
  Dllist.move_l n1 t1;
  assert (Dllist.to_list_l t1 = [ 5.3 ]);
  Dllist.move_r n1 t1;
  assert (Dllist.to_list_l t1 = [ 5.3 ]);
  let n2 = Dllist.add_l 5.2 t1 in
  assert (Dllist.to_list_l t1 = [ 5.2; 5.3 ]);
  Dllist.move_r n2 t1;
  assert (Dllist.to_list_l t1 = [ 5.3; 5.2 ]);
  Dllist.move_l n2 t1;
  assert (Dllist.to_list_l t1 = [ 5.2; 5.3 ]);
  let t2 = Dllist.create () in
  Dllist.move_l n1 t2;
  assert (Dllist.to_list_l t1 = [ 5.2 ]);
  assert (Dllist.to_list_l t2 = [ 5.3 ]);
  Dllist.move_r n2 t2;
  assert (Dllist.to_list_l t2 = [ 5.3; 5.2 ]);
  Dllist.move_l n1 t1;
  assert (Dllist.to_list_l t2 = [ 5.2 ]);
  assert (Dllist.to_list_l t1 = [ 5.3 ])

let () =
  Alcotest.run "Dllist"
    [
      ("basics", [ Alcotest.test_case "" `Quick basics ]);
      ("add", [ Alcotest.test_case "" `Quick add ]);
      ("move", [ Alcotest.test_case "" `Quick move ]);
    ]
