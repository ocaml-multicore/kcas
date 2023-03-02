open Kcas

let loop_count = try int_of_string Sys.argv.(1) with _ -> 1_000

let mode =
  Some
    (try
       if Sys.argv.(2) = "obstruction-free" then Mode.obstruction_free
       else Mode.lock_free
     with _ -> Mode.lock_free)

(* Number of shared counters being used to try to cause interference *)

(* Number of private accumulators used for extra work *)
let n_counters = try int_of_string Sys.argv.(3) with _ -> 2
let n_accumulators = try int_of_string Sys.argv.(4) with _ -> 2
let sleep_time = try int_of_string Sys.argv.(5) with _ -> 85

(* Set to true when the accumulator work is done and counter threads may exit.
   This way we ensure that the counter threads are causing interference for the
   whole duration of the test. *)
let exit = ref false

(* Counters are first initialized with a dummy location *)
let counters =
  let dummy_location_to_be_replaced = Loc.make 0 in
  Array.make n_counters dummy_location_to_be_replaced

(* Barrier used to synchronize counter threads and the accumulator thread *)
let barrier = Barrier.make (n_counters + 1)

let counter_thread i () =
  (* We allocate actual counter locations within the domain to avoid false
     sharing *)
  let counter = Loc.make ?mode 0 in
  counters.(i) <- counter;

  let tx ~xt = Xt.incr ~xt counter in

  Barrier.await barrier;

  while not !exit do
    (* Increment the accumulator to cause interference *)
    Xt.commit { tx };

    (* Delay for a bit.  If we don't delay enough, we can starve the
       accumulator. *)
    for _ = 1 to sleep_time do
      Domain.cpu_relax ()
    done
  done

let accumulator_thread () =
  (* Accumulators allocated in the domain to avoid false sharing *)
  let accumulators = Array.init n_accumulators (fun _ -> Loc.make 0) in

  let tx ~xt =
    (* Compute sum of counters - these accesses can be interfered with *)
    let sum_of_counters =
      Array.fold_left (fun sum counter -> sum + Xt.get ~xt counter) 0 counters
    in

    (* And do some other work (updating accumulators) *)
    Array.iter
      (fun accumulator ->
        Xt.fetch_and_add ~xt accumulator sum_of_counters |> ignore)
      accumulators
  in

  Barrier.await barrier;

  for _ = 1 to loop_count do
    Xt.commit { tx }
  done;

  exit := true

let () =
  accumulator_thread :: List.init n_counters counter_thread
  |> List.map Domain.spawn |> List.iter Domain.join;
  Printf.printf "Test loc modes OK!\n%!"
