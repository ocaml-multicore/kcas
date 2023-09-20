module Times = struct
  type t = { inverted : bool; times : float array }

  let record ~n_domains ?(n_warmups = 3) ?(n_runs = 17) ?(before = Fun.id) ~init
      ~work ?(after = Fun.id) () =
    let barrier_before = Barrier.make n_domains in
    let barrier_after = Barrier.make n_domains in
    let results = Array.init n_domains @@ fun _ -> Stack.create () in
    let main domain_i =
      for _ = 1 to n_warmups do
        if domain_i = 0 then before ();
        let state = init domain_i in
        Barrier.await barrier_before;
        work domain_i state;
        Barrier.await barrier_after;
        if domain_i = 0 then after ()
      done;
      for _run_i = 0 to n_runs - 1 do
        if domain_i = 0 then before ();
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
      done
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
    { inverted = false; times }

  let invert t =
    { inverted = not t.inverted; times = Array.map (fun v -> 1.0 /. v) t.times }
end

module Stats = struct
  type t = { mean : float; median : float; sd : float; inverted : bool }

  let scale factor t =
    {
      mean = t.mean *. factor;
      median = t.median *. factor;
      sd = t.sd *. factor;
      inverted = t.inverted;
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

  let of_times (t : Times.t) =
    let mean = mean_of t.times in
    let sd = sd_of t.times mean in
    let median = median_of t.times in
    { mean; sd; median; inverted = t.inverted }

  let to_nonbreaking s =
    s |> String.split_on_char ' '
    |> String.concat " " (* a non-breaking space *)

  let to_json ~name ~description ~units t =
    let metric value =
      `Assoc
        [
          ("name", `String (to_nonbreaking name));
          ("value", `Float value);
          ("units", `String units);
          ( "trend",
            `String
              (if t.inverted then "higher-is-better" else "lower-is-better") );
          ("description", `String description);
        ]
    in
    [ metric t.median ]
end
