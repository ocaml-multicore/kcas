open Kcas
open Kcas_data

let basics () =
  let mv = Mvar.create (Some 101) in
  assert (not (Mvar.is_empty mv));
  assert (Mvar.take mv = 101);
  assert (Mvar.is_empty mv);
  assert (Mvar.take_opt mv = None);
  Mvar.put mv 42;
  let running = Mvar.create None in
  let d =
    Domain.spawn @@ fun () ->
    Mvar.put running ();
    Xt.commit { tx = Mvar.Xt.put mv 76 }
  in
  assert (Mvar.take running = ());
  assert (Xt.commit { tx = Mvar.Xt.take mv } = 42);
  Domain.join d;
  assert (Mvar.take mv = 76)

let () =
  Alcotest.run "Mvar" [ ("basics", [ Alcotest.test_case "" `Quick basics ]) ]
