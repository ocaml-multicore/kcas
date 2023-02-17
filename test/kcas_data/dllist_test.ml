open Kcas_data

let[@tail_mod_cons] rec take_as_list take l =
  match take l with None -> [] | Some x -> x :: take_as_list take l

let () =
  let t1 = Dllist.create () in
  let t1' = Dllist.take_all t1 in
  assert (Dllist.to_list_r t1 = [] && Dllist.to_list_l t1' = []);
  Dllist.transfer_r t1' t1';
  Dllist.add_r 2 t1' |> ignore;
  Dllist.add_r 3 t1' |> ignore;
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

let () =
  let l = Dllist.create () in
  Dllist.add_l 1 l |> ignore;
  Dllist.add_l 3 l |> ignore;
  Dllist.add_r 4 l |> ignore;
  assert (take_as_list Dllist.take_opt_l l = [ 3; 1; 4 ])
