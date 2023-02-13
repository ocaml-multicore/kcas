let median l =
  let l = List.sort Float.compare l in
  let len = List.length l in
  assert (len > 0);
  let shift = len mod 2 in
  let b = List.nth l (len / 2) in
  let a = List.nth l ((len / 2) - 1 + shift) in
  (a +. b) /. 2.

let summarize l =
  let get_mean l =
    List.fold_right (fun a v -> a +. v) l 0. /. (float_of_int @@ List.length l)
  in
  let mean = get_mean l in
  let sd = get_mean @@ List.map (fun v -> abs_float (v -. mean) ** 2.) l in
  let median = median l in
  (`Mean mean, `Median median, `Stddev sd)

let discount_first_run l =
  List.rev l |> function [] -> assert false | _ :: l -> l

let run f n =
  let rec run acc = function
    | 0 -> acc
    | n ->
        Gc.full_major ();
        let t1 = Unix.gettimeofday () in
        let () = f () in
        let d = Unix.gettimeofday () -. t1 in
        run (d :: acc) (n - 1)
  in
  let r = run [] (n + 1) in
  summarize (discount_first_run r)


let as_json name (_, `Median median, `Stddev stddev) = 
    Printf.sprintf {|
     {
      "name": "%s",
      "metrics": [
        {
          "name": "median time",
          "value": %f,
          "units": "s",
          "description": "median of execution times"
        },
        {
          "name": "stddev of time",
          "value": %f,
          "units": "s",
          "description": "standard deviation of execution times"
        } 
      ] 
    }
    |} name median stddev 