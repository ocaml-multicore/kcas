module type Awaiter = sig
  type t

  val signal : t -> unit
end

module Make (Awaiter : Awaiter) : sig
  module Awaiter : Awaiter

  type 'a loc

  val make : 'a -> 'a loc
  val get : 'a loc -> 'a

  type cas = CAS : 'a loc * 'a * 'a -> cas
  type cmp = CMP : 'a loc * 'a -> cmp

  val atomically : cas list -> cmp list -> bool
end = struct
  module Awaiter = Awaiter

  type 'a loc = 'a state Atomic.t

  and 'a state = {
    mutable before : 'a;
    mutable after : 'a;
    casn : casn;
    awaiters : Awaiter.t list;
  }

  and cass =
    | CASS : {
        loc : 'a loc;
        desired : 'a state;
        mutable awaiters : Awaiter.t list;
      }
        -> cass

  and cmps = CMPS : { loc : 'a loc; current : 'a state } -> cmps
  and casn = status Atomic.t

  and status =
    | Undetermined of { cass : cass list; cmps : cmps list }
    | After
    | Before

  let make after =
    Atomic.make
      { before = after; after; casn = Atomic.make After; awaiters = [] }

  type cas = CAS : 'a loc * 'a * 'a -> cas
  type cmp = CMP : 'a loc * 'a -> cmp

  let finish casn desired =
    match Atomic.get casn with
    | After -> true
    | Before -> false
    | Undetermined undetermined as current ->
        let desired =
          if
            desired == After
            && undetermined.cmps
               |> List.exists @@ fun (CMPS cmps) ->
                  Atomic.get cmps.loc != cmps.current
          then Before
          else desired
        in
        if Atomic.compare_and_set casn current desired then begin
          if desired == After then begin
            undetermined.cass
            |> List.iter @@ fun (CASS cass) ->
               List.iter Awaiter.signal cass.awaiters;
               cass.desired.before <- cass.desired.after
          end
          else begin
            undetermined.cass
            |> List.iter @@ fun (CASS cass) ->
               cass.desired.after <- cass.desired.before
          end
        end;
        Atomic.get casn == After

  let rec gkmz casn = function
    | [] -> finish casn After
    | CASS cass :: continue as retry -> begin
        let current = Atomic.get cass.loc in
        if cass.desired == current then gkmz casn continue
        else
          let current_value = get_from current in
          if current_value != cass.desired.before then finish casn Before
          else
            match Atomic.get casn with
            | Undetermined _ ->
                cass.awaiters <- current.awaiters;
                if Atomic.compare_and_set cass.loc current cass.desired then
                  gkmz casn continue
                else gkmz casn retry
            | After -> true
            | Before -> false
      end

  and get_from : 'a. 'a state -> 'a =
   fun state ->
    match Atomic.get state.casn with
    | Undetermined { cass; _ } ->
        if gkmz state.casn cass then state.after else state.before
    | After -> state.after
    | Before -> state.before

  let atomically logical_cas_list logical_cmp_list =
    let casn = Atomic.make After in
    let cass =
      logical_cas_list
      |> List.map @@ function
         | CAS (loc, before, after) ->
             let next = { before; after; casn; awaiters = [] } in
             CASS { loc; desired = next; awaiters = [] }
    in
    let cmps =
      logical_cmp_list
      |> List.map @@ function
         | CMP (loc, expected) ->
             let current = Atomic.get loc in
             if get_from current != expected then raise Exit
             else CMPS { loc; current }
    in
    Atomic.set casn (Undetermined { cass; cmps });
    gkmz casn cass

  let atomically logical_cas_list logical_cmp_list =
    try atomically logical_cas_list logical_cmp_list with Exit -> false

  let get loc = get_from (Atomic.get loc)
end

let () =
  let module Awaiter = struct
    type t = unit

    let signal = ignore
  end in
  let module STM = Make (Awaiter) in
  let x = STM.make 82 in
  let y = STM.make 40 in
  assert (STM.atomically [ CAS (x, 82, 42) ] [ CMP (y, 40) ]);
  assert (STM.get x == 42 && STM.get y == 40);
  ()
