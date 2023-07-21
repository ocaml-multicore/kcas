module Loc = Kcas.Loc
module Op = Kcas.Op

let conflicting_overlap () =
  (* [cas_2] acts on the same location as [cas_1]
     and has conflicting values. kCAS should be failing.
  *)
  let v_1 = Loc.make 0 in
  let cas_1 = Op.make_cas v_1 0 1 in
  let cas_2 = Op.make_cas v_1 2 3 in

  match Op.atomically [ cas_1; cas_2 ] with
  | exception _ -> ()
  | _ -> assert false

let conflicting_duplicate () =
  (* [cas_2] acts on the same location as [cas_1]
     and has conflicting values. kCAS should fail.

     It may seem innocuous, since the final value of v_1
     matches even if the CAS succeeds. But it's not. Both
     threads think they have successfully CASed v_1 and may
     rely on that knowledge afterwards (e.g. use old value
     as index in array).
  *)
  let v_1 = Loc.make 0 in
  let cas_1 = Op.make_cas v_1 0 1 in
  let cas_2 = Op.make_cas v_1 0 1 in
  match Op.atomically [ cas_1; cas_2 ] with
  | exception _ -> ()
  | _ -> assert false

let meldable_overlap () =
  (* [cas_2] acts on the same location as [cas_1].
     This is not something that we can handle in the
     general case, but in this particular one,
     parameters allow the two CASes to be melded into
     one operation.

     Or, perhaps, melding should be left to the user.
  *)
  let v_1 = Loc.make 0 in
  let cas_1 = Op.make_cas v_1 0 1 in
  let cas_2 = Op.make_cas v_1 1 2 in
  match Op.atomically [ cas_1; cas_2 ] with
  | exception _ -> ()
  | _ -> assert false

let _ =
  Alcotest.run "Overlapping loc"
    [
      ( "conflicting overlap",
        [ Alcotest.test_case "" `Quick conflicting_overlap ] );
      ( "conflicting duplicate",
        [ Alcotest.test_case "" `Quick conflicting_duplicate ] );
      ("meldable overlap", [ Alcotest.test_case "" `Quick meldable_overlap ]);
    ]
