open Kcas
open Kcas_data

let replace_and_remove () =
  let t = Hashtbl.create () in
  let n = try int_of_string Sys.argv.(1) with _ -> 10 * Util.iter_factor in
  for i = 1 to n do
    Hashtbl.replace t i i
  done;
  assert (Hashtbl.length t = n);
  assert (Seq.fold_left (fun n _ -> n + 1) 0 (Hashtbl.to_seq t) = n);
  for i = 1 to n do
    assert (Hashtbl.find t i = i)
  done;
  for i = 1 to n do
    Hashtbl.remove t i
  done;
  assert (Hashtbl.length t = 0)

let large_tx () =
  let t = Hashtbl.create () in
  let n = 1_000 in
  let tx ~xt =
    for i = 1 to n do
      Hashtbl.Xt.replace ~xt t i i
    done
  in
  Xt.commit { tx };
  assert (Seq.fold_left (fun n _ -> n + 1) 0 (Hashtbl.to_seq t) = n);
  let tx ~xt =
    for i = 1 to n do
      assert (Hashtbl.Xt.find_opt ~xt t i = Some i)
    done
  in
  Xt.commit { tx }

let large_ops () =
  let t = Hashtbl.create () in
  Hashtbl.add t "key" 1;
  Hashtbl.add t "key" 2;
  Hashtbl.add t "key" 3;
  assert (
    Hashtbl.fold (fun k v kvs -> (k, v) :: kvs) t []
    = [ ("key", 1); ("key", 2); ("key", 3) ]);
  let stats = Hashtbl.stats t in
  assert (stats.num_bindings = 3);
  assert (stats.num_buckets > 0);
  assert (stats.max_bucket_length = 3);
  assert (stats.bucket_histogram.(3) = 1);
  assert (Hashtbl.find_all t "key" = [ 3; 2; 1 ]);
  let t' = Hashtbl.copy t in
  assert (Hashtbl.find_all t' "key" = [ 3; 2; 1 ]);
  let t' = Hashtbl.rebuild ~hashed_type:(Hashtbl.hashed_type_of t) t in
  assert (Hashtbl.find_all t' "key" = [ 3; 2; 1 ]);
  assert (
    Hashtbl.to_seq t |> List.of_seq = [ ("key", 3); ("key", 2); ("key", 1) ]);
  let u = Hashtbl.to_seq t |> Hashtbl.of_seq in
  Hashtbl.swap t u;
  assert (Hashtbl.find t "key" = 1);
  assert (Hashtbl.find u "key" = 3);
  Hashtbl.filter_map_inplace (fun _ v -> if v = 1 then None else Some (-v)) u;
  assert (Hashtbl.find_all u "key" = [ -3; -2 ]);
  Hashtbl.swap u t;
  assert (Hashtbl.length t = 2);
  (match
     Hashtbl.filter_map_inplace
       (fun _ v -> if v = -2 then raise Exit else None)
       t
   with
  | _ -> assert false
  | exception Exit -> ());
  assert (Hashtbl.find_all t "key" = [ -3; -2 ]);
  assert (Hashtbl.length t = 2)

let basics () =
  let t = Hashtbl.create () in
  assert (Hashtbl.length t = 0);
  Hashtbl.replace t "foo" 101;
  Hashtbl.remove t "bar";
  assert (Hashtbl.length t = 1);
  Hashtbl.replace t "bar" 19;
  assert (Hashtbl.mem t "foo");
  assert (not (Hashtbl.mem t "bal"));
  Hashtbl.replace t "foo" 76;
  assert (Hashtbl.length t = 2);
  assert (Hashtbl.find_opt t "lol" = None);
  assert (
    Hashtbl.to_seq t |> List.of_seq |> List.sort compare
    = [ ("bar", 19); ("foo", 76) ]);
  Hashtbl.remove t "foo";
  assert (Hashtbl.length t = 1);
  assert (Hashtbl.to_seq t |> List.of_seq |> List.sort compare = [ ("bar", 19) ]);
  Hashtbl.reset t;
  assert (not (Hashtbl.mem t "nope"))

let () =
  Alcotest.run "Hashtbl"
    [
      ("replace and remove", [ Alcotest.test_case "" `Quick replace_and_remove ]);
      ("large tx", [ Alcotest.test_case "" `Quick large_tx ]);
      ("large ops", [ Alcotest.test_case "" `Quick large_ops ]);
      ("basics", [ Alcotest.test_case "" `Quick basics ]);
    ]
