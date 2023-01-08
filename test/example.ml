(* construct atomic variables *)
let atomic_1, atomic_2 = (Kcas.ref 0, Kcas.ref 3) in

(* construct kcas operation *)
let kcas = [ Kcas.mk_cas atomic_1 0 1; Kcas.mk_cas atomic_2 3 4 ] in

(* apply constructed kcas*)
ignore (Kcas.kCAS kcas);

(* atomic_1 = 1, atomic_2 = 4 *)
assert (Kcas.get atomic_1 = 1);
assert (Kcas.get atomic_2 = 4)
