open Kcas_data
open Bench

module Int = struct
  include Int

  let hash = Fun.id
end

let run_one ~n_domains ?(factor = 1) ?(n_ops = 50 * factor * Util.iter_factor)
    ?(n_keys = 1000) ~percent_read () =
  let t =
    Hashtbl.create ~hashed_type:(module Int) ()
    |> Multicore_magic.copy_as_padded
  in

  for i = 0 to n_keys - 1 do
    Hashtbl.replace t i i
  done;

  let n_ops_todo = Atomic.make n_ops |> Multicore_magic.copy_as_padded in

  let init _ = Random.State.make_self_init () in

  let work _ state =
    let rec work () =
      let n = Util.alloc n_ops_todo in
      if n <> 0 then
        let rec loop n =
          if 0 < n then
            let value = Random.State.bits state in
            let op = (value asr 20) mod 100 in
            let key = value mod n_keys in
            if op < percent_read then begin
              Hashtbl.find_opt t key |> ignore;
              loop (n - 1)
            end
            else begin
              Hashtbl.remove t key;
              Hashtbl.add t key value;
              loop (n - 2)
            end
          else work ()
        in
        loop n
    in
    work ()
  in
  let after () = Atomic.set n_ops_todo n_ops in

  let times = Times.record ~n_domains ~init ~work ~after () in

  let name metric =
    Printf.sprintf "%s/%d worker%s, %d%% reads" metric n_domains
      (if n_domains = 1 then "" else "s")
      percent_read
  in

  List.concat
    [
      Stats.of_times times
      |> Stats.scale (1_000_000_000.0 /. Float.of_int n_ops)
      |> Stats.to_json
           ~name:(name "time per operation")
           ~description:"Average time to find, remove, or add a binding"
           ~units:"ns";
      Times.invert times |> Stats.of_times
      |> Stats.scale (Float.of_int (n_ops * n_domains) /. 1_000_000.0)
      |> Stats.to_json
           ~name:(name "operations over time")
           ~description:
             "Number of operations performed over time using all domains"
           ~units:"M/s";
    ]

let run_suite ~factor =
  Util.cross [ 90; 50; 10 ] [ 1; 2; 4 ]
  |> List.concat_map @@ fun (percent_read, n_domains) ->
     run_one ~n_domains ~percent_read ~factor ()
