open Kcas_data

module Int = struct
  include Int

  let hash = Fun.id
end

let bench ~n_domains ~n_ops ~n_keys ~percent_read =
  let t =
    Hashtbl.create ~hashed_type:(module Int) ()
    |> Multicore_magic.copy_as_padded
  in

  for i = 0 to n_keys - 1 do
    Hashtbl.replace t i i
  done;

  let barrier = Atomic.make n_domains in

  let n_ops_todo = Atomic.make n_ops |> Multicore_magic.copy_as_padded in
  let rec alloc_ops () =
    let n = Atomic.get n_ops_todo in
    if n = 0 then 0
    else
      let batch = Int.min n 1000 in
      if Atomic.compare_and_set n_ops_todo n (n - batch) then batch
      else alloc_ops ()
  in

  let worker _ =
    let state = Random.State.make_self_init () in

    Atomic.decr barrier;
    while Atomic.get barrier <> 0 do
      Domain.cpu_relax ()
    done;

    let start = Unix.gettimeofday () in
    let rec work () =
      let n = alloc_ops () in
      if n <> 0 then begin
        for _ = 1 to n do
          let value = Random.State.bits state in
          let op = (value asr 20) mod 100 in
          let key = value mod n_keys in
          if op < percent_read then Hashtbl.find_opt t key |> ignore
          else begin
            Hashtbl.remove t key;
            Hashtbl.add t key value
          end
        done;
        work ()
      end
    in
    work ();
    Unix.gettimeofday () -. start
  in

  let domains = Array.make (n_domains - 1) worker |> Array.map Domain.spawn in

  let total =
    let accum = worker 0 in
    Array.map Domain.join domains |> Array.fold_left ( +. ) accum
  in

  Printf.printf "%f ns/tx\n" (1_000_000_000.0 *. total /. Float.of_int n_ops)

let () =
  let n_domains =
    try int_of_string Sys.argv.(1)
    with _ -> Int.max 2 (Domain.recommended_domain_count ()) / 2
  in
  let n_ops =
    try int_of_string Sys.argv.(2) with _ -> 100 * Util.iter_factor
  in
  let n_keys = try int_of_string Sys.argv.(3) with _ -> 1_000 in
  let percent_read = try int_of_string Sys.argv.(4) with _ -> 70 in
  bench ~n_domains ~n_ops ~n_keys ~percent_read
