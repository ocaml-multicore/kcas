module Times = struct
  type t = { inverted : bool; times : float array; runs : int }

  let record ~n_domains ~budgetf ?(n_warmups = 3) ?(n_runs_min = 7)
      ?(before = Fun.id) ~init ~work ?(after = Fun.id) () =
    let barrier_init = Barrier.make n_domains in
    let barrier_before = Barrier.make n_domains in
    let barrier_after = Barrier.make n_domains in
    let results =
      Array.init n_domains @@ fun _ ->
      Stack.create () |> Multicore_magic.copy_as_padded
    in
    let budget_used = ref false |> Multicore_magic.copy_as_padded in
    let runs = ref 0 |> Multicore_magic.copy_as_padded in
    Gc.full_major ();
    let budget_start = Mtime_clock.elapsed () in
    let prepare_for_await () =
      let open struct
        type state = Init | Released | Awaiting of { mutable released : bool }
      end in
      let state = Atomic.make Init in
      let release () =
        if Multicore_magic.fenceless_get state != Released then
          match Atomic.exchange state Released with
          | Awaiting r -> r.released <- true
          | _ -> ()
      in
      let await () =
        if Multicore_magic.fenceless_get state != Released then
          let awaiting = Awaiting { released = false } in
          if Atomic.compare_and_set state Init awaiting then
            match awaiting with
            | Awaiting r ->
                (* Avoid sleeping *)
                while not r.released do
                  Domain.cpu_relax ()
                done
            | _ -> ()
      in
      Domain_local_await.{ release; await }
    in
    let main domain_i =
      Domain_local_await.using ~prepare_for_await ~while_running:(fun () ->
          for _ = 1 to n_warmups do
            if domain_i = 0 then begin
              before ();
              Gc.major ()
            end;
            let state = init domain_i in
            Barrier.await barrier_before;
            work domain_i state;
            Barrier.await barrier_after;
            if domain_i = 0 then after ()
          done;
          while !runs < n_runs_min || not !budget_used do
            Barrier.await barrier_init;
            if domain_i = 0 then begin
              before ();
              if
                let budget_stop = Mtime_clock.elapsed () in
                let elapsedf =
                  Mtime.Span.to_float_ns
                    (Mtime.Span.abs_diff budget_stop budget_start)
                  *. (1. /. 1_000_000_000.0)
                in
                budgetf < elapsedf
              then budget_used := true;
              incr runs;
              Gc.major ()
            end;
            let state = init domain_i in
            Barrier.await barrier_before;
            let start = Mtime_clock.elapsed () in
            work domain_i state;
            let stop = Mtime_clock.elapsed () in
            Barrier.await barrier_after;
            if domain_i = 0 then after ();
            Stack.push
              (Mtime.Span.to_float_ns (Mtime.Span.abs_diff stop start)
              *. (1. /. 1_000_000_000.0))
              results.(domain_i)
          done)
    in
    let domains =
      Array.init n_domains @@ fun domain_i ->
      Domain.spawn @@ fun () -> main domain_i
    in
    Array.iter Domain.join domains;
    let n = Stack.length results.(0) in
    let times = Array.create_float n in
    for run_i = 0 to n - 1 do
      times.(run_i) <- 0.0;
      for domain_i = 0 to n_domains - 1 do
        times.(run_i) <- times.(run_i) +. Stack.pop results.(domain_i)
      done
    done;
    { inverted = false; times; runs = !runs }

  let invert { inverted; times; runs } =
    {
      inverted = not inverted;
      times = Array.map (fun v -> 1.0 /. v) times;
      runs;
    }
end

module Stats = struct
  type t = {
    mean : float;
    median : float;
    sd : float;
    inverted : bool;
    best : float;
    runs : int;
  }

  let scale factor { mean; median; sd; inverted; best; runs } =
    {
      mean = mean *. factor;
      median = median *. factor;
      sd = sd *. factor;
      inverted;
      best = best *. factor;
      runs;
    }

  let mean_of times =
    Array.fold_left ( +. ) 0.0 times /. Float.of_int (Array.length times)

  let sd_of times mean =
    Float.sqrt
      (mean_of (Array.map (fun v -> Float.abs (v -. mean) ** 2.) times))

  let median_of times =
    Array.sort Float.compare times;
    let n = Array.length times in
    if n land 1 = 0 then (times.((n asr 1) - 1) +. times.(n asr 1)) /. 2.0
    else times.(n asr 1)

  let of_times Times.{ inverted; times; runs } =
    let mean = mean_of times in
    let sd = sd_of times mean in
    let median = median_of times in
    let best =
      if inverted then Array.fold_left Float.max Float.min_float times
      else Array.fold_left Float.min Float.max_float times
    in
    { mean; sd; median; inverted; best; runs }

  let to_nonbreaking s =
    s |> String.split_on_char ' '
    |> String.concat " " (* a non-breaking space *)

  let to_json ~name ~description ~units t =
    let trend =
      if t.inverted then `String "higher-is-better"
      else `String "lower-is-better"
    in
    [
      `Assoc
        [
          ("name", `String (to_nonbreaking name));
          ("value", `Float t.median);
          ("units", `String units);
          ("trend", trend);
          ("description", `String description);
          ("#best", `Float t.best);
          ("#mean", `Float t.mean);
          ("#median", `Float t.median);
          ("#sd", `Float t.sd);
          ("#runs", `Int t.runs);
        ];
    ]
end
