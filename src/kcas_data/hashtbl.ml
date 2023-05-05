open Kcas

(** Optimized operations on internal association lists with custom equality. *)
module Assoc = struct
  type ('k, 'v) t = ('k * 'v) list

  let iter_rev f = function
    | [] -> ()
    | [ kv ] -> f kv
    | kvs -> kvs |> List.rev |> List.iter f

  let find_opt equal k = function
    | [] -> None
    | (k', v) :: kvs ->
        if equal k k' then Some v
        else
          kvs
          |> List.find_map @@ fun (k', v) -> if equal k k' then Some v else None

  let find_all equal k =
    List.filter_map @@ fun (k', v) -> if equal k k' then Some v else None

  let mem equal k = function
    | [] -> false
    | (k', _) :: kvs ->
        equal k k' || kvs |> List.exists @@ fun (k', _) -> equal k k'

  let[@tail_mod_cons] rec remove equal change k = function
    | [] -> []
    | ((k', _) as kv') :: kvs' ->
        if equal k k' then (
          change := `Removed;
          kvs')
        else kv' :: remove equal change k kvs'

  let[@tail_mod_cons] rec replace equal change k v = function
    | [] ->
        change := `Added;
        [ (k, v) ]
    | ((k', v') as kv') :: kvs' as original ->
        if equal k k' then
          if v == v' then original
          else (
            change := `Replaced;
            (k, v) :: kvs')
        else kv' :: replace equal change k v kvs'

  let[@tail_mod_cons] rec filter_map fn delta = function
    | [] -> []
    | ((k, v) as original_kv) :: kvs -> (
        match fn k v with
        | None ->
            decr delta;
            filter_map fn delta kvs
        | Some v' ->
            let kv = if v == v' then original_kv else (k, v') in
            kv :: filter_map fn delta kvs)
end

type ('k, 'v) pending =
  | Nothing
  | Rehash of {
      state : int Loc.t;
      new_capacity : int;
      new_buckets : ('k, 'v) Assoc.t Loc.t array Loc.t;
    }
  | Snapshot of { state : int Loc.t; snapshot : ('k * 'v) list array Loc.t }
  | Filter_map of {
      state : int Loc.t;
      fn : 'k -> 'v -> 'v option;
      raised : exn Loc.t;
      new_buckets : ('k, 'v) Assoc.t Loc.t array Loc.t;
    }

type ('k, 'v) t = {
  pending : ('k, 'v) pending Loc.t;
  length : Accumulator.t;
  buckets : ('k, 'v) Assoc.t Loc.t array Loc.t;
  hash : 'k -> int;
  equal : 'k -> 'k -> bool;
  min_buckets : int;
  max_buckets : int;
}

type 'k hashed_type = (module Stdlib.Hashtbl.HashedType with type t = 'k)

let lo_buckets = 1 lsl 5
let hi_buckets = (Sys.max_array_length lsr 1) + 1
let () = assert (Bits.is_pow_2 hi_buckets)
let min_buckets_default = lo_buckets
let max_buckets_default = Int.min hi_buckets (1 lsl 30 (* Limit of [hash] *))

module HashedType = struct
  let pack (type k) hash equal : k hashed_type =
    (module struct
      type t = k

      let hash = hash
      and equal = equal
    end)

  let unpack (type k) ((module HashedType) : k hashed_type) =
    (HashedType.hash, HashedType.equal)

  let is_same_as (type k) hash equal ((module HashedType) : k hashed_type) =
    hash == HashedType.hash && equal == HashedType.equal
end

let create ?hashed_type ?min_buckets ?max_buckets ?n_way () =
  let min_buckets =
    match min_buckets with
    | None -> min_buckets_default
    | Some c -> Int.max lo_buckets c |> Int.min hi_buckets |> Bits.ceil_pow_2
  in
  let max_buckets =
    match max_buckets with
    | None -> Int.max min_buckets max_buckets_default
    | Some c -> Int.max min_buckets c |> Int.min hi_buckets |> Bits.ceil_pow_2
  and hash, equal =
    match hashed_type with
    | None -> (Stdlib.Hashtbl.seeded_hash (Random.bits ()), ( = ))
    | Some hashed_type -> HashedType.unpack hashed_type
  and pending = Loc.make Nothing
  and buckets = Loc.make [||]
  and length = Accumulator.make ?n_way 0 in
  Loc.set buckets @@ Loc.make_array min_buckets [];
  { pending; length; buckets; hash; equal; min_buckets; max_buckets }

let n_way_of t = Accumulator.n_way_of t.length
let min_buckets_of t = t.min_buckets
let max_buckets_of t = t.max_buckets
let hashed_type_of t = HashedType.pack t.hash t.equal

let bucket_of hash key buckets =
  Array.unsafe_get buckets (hash key land (Array.length buckets - 1))

exception Done

module Xt = struct
  let find_opt ~xt t k =
    Xt.get ~xt t.buckets |> bucket_of t.hash k |> Xt.get ~xt
    |> Assoc.find_opt t.equal k

  let find_all ~xt t k =
    Xt.get ~xt t.buckets |> bucket_of t.hash k |> Xt.get ~xt
    |> Assoc.find_all t.equal k

  let mem ~xt t k =
    Xt.get ~xt t.buckets |> bucket_of t.hash k |> Xt.get ~xt
    |> Assoc.mem t.equal k

  let get_or_alloc array_loc alloc =
    let tx ~xt =
      let array = Xt.get ~xt array_loc in
      if array != [||] then array
      else
        let array = alloc () in
        Xt.set ~xt array_loc array;
        array
    in
    Xt.commit { tx }

  (** Pending operations are performed incrementally in small batches. *)
  let batch_size = 3

  let perform_pending ~xt t =
    (* TODO: Implement pending operations such that multiple domains may be
       working to complete them in parallel by extending the [state] to an array
       of multiple partition [states]. *)
    let must_be_done_in_this_tx = Xt.is_in_log ~xt t.pending in
    match Xt.exchange ~xt t.pending Nothing with
    | Nothing -> ()
    | Rehash { state; new_capacity; new_buckets } -> (
        let new_buckets =
          get_or_alloc new_buckets @@ fun () -> Loc.make_array new_capacity []
        in
        let old_buckets = Xt.exchange ~xt t.buckets new_buckets in
        let hash = t.hash and mask = new_capacity - 1 in
        let rehash_a_few_buckets ~xt =
          (* We process buckets in descending order as that is slightly faster
             with the transaction log.  It also makes sure that we know when the
             operation has already been performed independently of the
             buckets array we read above. *)
          let i = Xt.fetch_and_add ~xt state (-batch_size) in
          if i <= 0 then raise Done;
          for i = i - 1 downto Bits.max_0 (i - batch_size) do
            Array.unsafe_get old_buckets i
            |> Xt.get ~xt
            |> Assoc.iter_rev @@ fun ((k, _) as kv) ->
               Xt.modify ~xt
                 (Array.unsafe_get new_buckets (hash k land mask))
                 (List.cons kv)
          done
        in
        try
          if must_be_done_in_this_tx then
            (* If the old buckets have already been accessed, we cannot perform
               rehashing outside of the transaction.  In this case rehashing
               becomes linearithmic, O(n*log(n)), because that is the best that
               the transaction log promises.  However, as we access the bucket
               locations mostly in order, we often actually get linear time,
               O(n), performance. *)
            let initial_state = Array.length old_buckets in
            while true do
              (* If state is modified outside our expensive tx would fail. *)
              Retry.unless (Loc.fenceless_get state = initial_state);
              rehash_a_few_buckets ~xt
            done
          else
            (* When possible, rehashing is performed cooperatively a few buckets
               at a time.  This gives expected linear time, O(n). *)
            while true do
              Xt.commit { tx = rehash_a_few_buckets }
            done
        with Done -> ())
    | Snapshot { state; snapshot } -> (
        assert (not must_be_done_in_this_tx);
        let buckets = Xt.get ~xt t.buckets in
        (* Check state to ensure that buckets have not been updated. *)
        Retry.unless (0 <= Loc.fenceless_get state);
        let snapshot =
          get_or_alloc snapshot @@ fun () ->
          Array.make (Array.length buckets) []
        in
        let snapshot_a_few_buckets ~xt =
          let i = Xt.fetch_and_add ~xt state (-batch_size) in
          if i <= 0 then raise Done;
          for i = i - 1 downto Bits.max_0 (i - batch_size) do
            Array.unsafe_get buckets i |> Xt.get ~xt
            |> Array.unsafe_set snapshot i
          done
        in
        try
          while true do
            Xt.commit { tx = snapshot_a_few_buckets }
          done
        with Done -> ())
    | Filter_map { state; fn; raised; new_buckets } -> (
        assert (not must_be_done_in_this_tx);
        let old_buckets = Xt.get ~xt t.buckets in
        (* Check state to ensure that buckets have not been updated. *)
        Retry.unless (0 <= Loc.fenceless_get state);
        let new_capacity = Array.length old_buckets in
        let new_buckets =
          get_or_alloc new_buckets @@ fun () -> Loc.make_array new_capacity []
        in
        let filter_map_a_few_buckets ~xt =
          let i = Xt.fetch_and_add ~xt state (-batch_size) in
          if i <= 0 then raise Done;
          let a_few_buckets_delta = ref 0 in
          for i = i - 1 downto Bits.max_0 (i - batch_size) do
            Xt.get ~xt (Array.unsafe_get old_buckets i)
            |> Assoc.filter_map fn a_few_buckets_delta
            |> Xt.set ~xt (Array.unsafe_get new_buckets i)
          done;
          !a_few_buckets_delta
        in
        let total_delta = ref 0 in
        try
          while true do
            total_delta :=
              !total_delta + Xt.commit { tx = filter_map_a_few_buckets }
          done
        with
        | Done ->
            Accumulator.Xt.add ~xt t.length !total_delta;
            Xt.set ~xt t.buckets new_buckets
        | exn -> Loc.compare_and_set raised Done exn |> ignore)

  let make_rehash old_capacity new_capacity =
    let state = Loc.make old_capacity and new_buckets = Loc.make [||] in
    Rehash { state; new_capacity; new_buckets }

  let reset ~xt t =
    perform_pending ~xt t;
    Xt.set ~xt t.buckets [||];
    Accumulator.Xt.set ~xt t.length 0;
    Xt.set ~xt t.pending @@ make_rehash 0 t.min_buckets

  let clear ~xt t = reset ~xt t

  let remove ~xt t k =
    perform_pending ~xt t;
    let buckets = Xt.get ~xt t.buckets in
    let mask = Array.length buckets - 1 in
    let bucket = Array.unsafe_get buckets (t.hash k land mask) in
    let change = ref `None in
    Xt.modify ~xt bucket (fun kvs ->
        let kvs' = Assoc.remove t.equal change k kvs in
        if !change != `None then kvs' else kvs);
    if !change == `Removed then (
      Accumulator.Xt.decr ~xt t.length;
      if t.min_buckets <= mask && Random.bits () land mask = 0 then
        let capacity = mask + 1 in
        let length = Accumulator.Xt.get ~xt t.length in
        if length * 4 < capacity then
          Xt.set ~xt t.pending @@ make_rehash capacity (capacity asr 1))

  let add ~xt t k v =
    perform_pending ~xt t;
    let buckets = Xt.get ~xt t.buckets in
    let mask = Array.length buckets - 1 in
    let bucket = Array.unsafe_get buckets (t.hash k land mask) in
    Xt.modify ~xt bucket (List.cons (k, v));
    Accumulator.Xt.incr ~xt t.length;
    if mask + 1 < t.max_buckets && Random.bits () land mask = 0 then
      let capacity = mask + 1 in
      let length = Accumulator.Xt.get ~xt t.length in
      if capacity < length then
        Xt.set ~xt t.pending @@ make_rehash capacity (capacity * 2)

  let replace ~xt t k v =
    perform_pending ~xt t;
    let buckets = Xt.get ~xt t.buckets in
    let mask = Array.length buckets - 1 in
    let bucket = Array.unsafe_get buckets (t.hash k land mask) in
    let change = ref `None in
    Xt.modify ~xt bucket (fun kvs ->
        let kvs' = Assoc.replace t.equal change k v kvs in
        if !change != `None then kvs' else kvs);
    if !change == `Added then (
      Accumulator.Xt.incr ~xt t.length;
      if mask + 1 < t.max_buckets && Random.bits () land mask = 0 then
        let capacity = mask + 1 in
        let length = Accumulator.Xt.get ~xt t.length in
        if capacity < length then
          Xt.set ~xt t.pending @@ make_rehash capacity (capacity * 2))

  let length ~xt t = Accumulator.Xt.get ~xt t.length
end

let find_opt t k =
  Loc.get t.buckets |> bucket_of t.hash k |> Loc.fenceless_get
  |> Assoc.find_opt t.equal k

let find_all t k =
  Loc.get t.buckets |> bucket_of t.hash k |> Loc.fenceless_get
  |> Assoc.find_all t.equal k

let find t k = match find_opt t k with None -> raise Not_found | Some v -> v

let mem t k =
  Loc.get t.buckets |> bucket_of t.hash k |> Loc.fenceless_get
  |> Assoc.mem t.equal k

let clear t = Kcas.Xt.commit { tx = Xt.clear t }
let reset t = Kcas.Xt.commit { tx = Xt.reset t }
let remove t k = Kcas.Xt.commit { tx = Xt.remove t k }
let add t k v = Kcas.Xt.commit { tx = Xt.add t k v }
let replace t k v = Kcas.Xt.commit { tx = Xt.replace t k v }
let length t = Accumulator.get t.length

let snapshot ?length t =
  let state = Loc.make 0 and snapshot = Loc.make [||] in
  let pending = Snapshot { state; snapshot } in
  let tx ~xt =
    Xt.perform_pending ~xt t;
    length
    |> Option.iter (fun length -> length := Accumulator.Xt.get ~xt t.length);
    Loc.set state (Array.length (Kcas.Xt.get ~xt t.buckets));
    Kcas.Xt.set ~xt t.pending pending
  in
  Kcas.Xt.commit { tx };
  Kcas.Xt.commit { tx = Xt.perform_pending t };
  Loc.fenceless_get snapshot

let to_seq t =
  let snapshot = snapshot t in
  let rec loop i kvs () =
    match kvs with
    | [] ->
        if i = Array.length snapshot then Seq.Nil
        else loop (i + 1) (Array.unsafe_get snapshot i) ()
    | kv :: kvs -> Seq.Cons (kv, loop i kvs)
  in
  loop 0 []

let to_seq_keys t = to_seq t |> Seq.map fst
let to_seq_values t = to_seq t |> Seq.map snd

let of_seq ?hashed_type ?min_buckets ?max_buckets ?n_way xs =
  let t = create ?hashed_type ?min_buckets ?max_buckets ?n_way () in
  Seq.iter (fun (k, v) -> replace t k v) xs;
  t

let rebuild ?hashed_type ?min_buckets ?max_buckets ?n_way t =
  let min_buckets =
    match min_buckets with
    | None -> min_buckets_of t
    | Some c -> Int.max lo_buckets c |> Int.min hi_buckets |> Bits.ceil_pow_2
  in
  let max_buckets =
    match max_buckets with
    | None -> Int.max min_buckets (max_buckets_of t)
    | Some c -> Int.max min_buckets c |> Int.min hi_buckets |> Bits.ceil_pow_2
  and n_way = match n_way with None -> n_way_of t | Some n -> n
  and length = ref 0 in
  let snapshot = snapshot ~length t in
  let is_same_hashed_type =
    match hashed_type with
    | None -> true
    | Some hashed_type -> HashedType.is_same_as t.hash t.equal hashed_type
  and length = !length in
  if is_same_hashed_type && min_buckets <= length && length <= max_buckets then (
    let pending = Loc.make Nothing
    and buckets = Loc.make [||]
    and length = Accumulator.make ~n_way length in
    Loc.set buckets @@ Array.map Loc.make snapshot;
    { t with pending; length; buckets; min_buckets; max_buckets })
  else
    let t = create ?hashed_type ~min_buckets ~max_buckets ~n_way () in
    snapshot
    |> Array.iter (fun bucket ->
           bucket |> List.rev |> List.iter (fun (k, v) -> add t k v));
    t

let copy t = rebuild t

let fold f t a =
  Array.fold_left (List.fold_left (fun a (k, v) -> f k v a)) a (snapshot t)

let iter f t = fold (fun k v () -> f k v) t ()

let filter_map_inplace fn t =
  let state = Loc.make 0
  and raised = Loc.make Done
  and new_buckets = Loc.make [||] in
  let pending = Filter_map { state; fn; raised; new_buckets } in
  let tx ~xt =
    Xt.perform_pending ~xt t;
    Loc.set state (Array.length (Kcas.Xt.get ~xt t.buckets));
    Kcas.Xt.set ~xt t.pending pending
  in
  Kcas.Xt.commit { tx };
  Kcas.Xt.commit { tx = Xt.perform_pending t };
  match Loc.fenceless_get raised with Done -> () | exn -> raise exn

let stats t =
  let length = ref 0 in
  let snapshot = snapshot ~length t in
  let num_bindings = !length in
  let num_buckets = Array.length snapshot in
  let bucket_lengths = Array.map List.length snapshot in
  let max_bucket_length = Array.fold_left Int.max 0 bucket_lengths in
  let bucket_histogram = Array.make (max_bucket_length + 1) 0 in
  bucket_lengths
  |> Array.iter (fun i -> bucket_histogram.(i) <- 1 + bucket_histogram.(i));
  Stdlib.Hashtbl.
    { num_bindings; num_buckets; max_bucket_length; bucket_histogram }
