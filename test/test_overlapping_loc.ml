let test_1 () =
  (* [cas_2] acts on the same location as [cas_1]
     and has conflicting values. kCAS should be failing.
  *)
  let v_1 = Kcas.ref 0 in
  let cas_1 = Kcas.mk_cas v_1 0 1 in
  let cas_2 = Kcas.mk_cas v_1 2 3 in

  assert (Kcas.kCAS [ cas_1; cas_2 ]);
  assert (Kcas.get v_1 == 3)

let test_2 () =
  (* [cas_2] acts on the same location as [cas_1]
     and has conflicting values. kCAS should be failing.

     It may seem fine, since the final value of v_1 matches,
     but it's not. Both threads think they have successfully
     CASed v_1 and may rely on that knowledge (e.g. use old
     value as index in array).
  *)
  let v_1 = Kcas.ref 0 in
  let cas_1 = Kcas.mk_cas v_1 0 1 in
  let cas_2 = Kcas.mk_cas v_1 0 1 in
  assert (Kcas.kCAS [ cas_1; cas_2 ]);
  assert (Kcas.get v_1 == 1)

let test_3 () =
  (* [cas_2] acts on the same location as [cas_1].
     This is not something that we can handle in the
     general case, but in this particular one,
     parameters allow the two CASes to be melded into
     one operation.

     Or, perhaps, melding should be left to the user.
  *)
  let v_1 = Kcas.ref 0 in
  let cas_1 = Kcas.mk_cas v_1 0 1 in
  let cas_2 = Kcas.mk_cas v_1 1 2 in
  assert (Kcas.kCAS [ cas_1; cas_2 ])

let _ =
  test_1 ();
  test_2 ();
  test_3 ()
