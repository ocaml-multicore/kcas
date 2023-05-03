open Kcas

let () =
  let x = Loc.make 0 in
  let y = Loc.make 0 in

  let a_thread =
    ()
    |> Thread.create @@ fun () ->
       Loc.get_as (fun x -> Retry.unless (x <> 0)) x;
       Loc.set y 22
  in

  Loc.set x 20;
  Loc.get_as (fun y -> Retry.unless (y <> 0)) y;

  Thread.join a_thread;

  assert (Loc.get x + Loc.get y = 42);

  Printf.printf "Test threads OK!\n%!"
