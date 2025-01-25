open Picos_std_structured
open Kcas
open Kcas_data

let basics () =
  Scheduler.run ~n_domains:2 @@ fun () ->
  Flock.join_after @@ fun () ->
  let mv = Mvar.create (Some 101) in
  assert (not (Mvar.is_empty mv));
  assert (Mvar.take mv = 101);
  assert (Mvar.is_empty mv);
  assert (Mvar.take_opt mv = None);
  Mvar.put mv 42;
  let running = Mvar.create None in
  begin
    Flock.fork @@ fun () ->
    Mvar.put running ();
    Xt.commit { tx = Mvar.Xt.put mv 76 }
  end;
  assert (Mvar.take running = ());
  assert (Xt.commit { tx = Mvar.Xt.take mv } = 42);
  assert (Mvar.take mv = 76)

let () =
  Alcotest.run "Mvar" [ ("basics", [ Alcotest.test_case "" `Quick basics ]) ]
