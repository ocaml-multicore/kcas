(* construct atomic variables *)
let atomic_1, atomic_2 = (Kcas.Loc.make 0, Kcas.Loc.make 3) in

(* construct kcas operation *)
let kcas = [ Kcas.Op.make_cas atomic_1 0 1; Kcas.Op.make_cas atomic_2 3 4 ] in

(* apply constructed kcas *)
ignore (Kcas.Op.atomically kcas);

(* atomic_1 = 1, atomic_2 = 4 *)
assert (Kcas.Loc.get atomic_1 = 1);
assert (Kcas.Loc.get atomic_2 = 4);

Printf.printf "Example OK!\n%!"
