module Kcas : sig
  type 'a loc

  val make : 'a -> 'a loc
  val get : 'a loc -> 'a

  type cas = CAS : 'a loc * 'a * 'a -> cas
  type cmp = CMP : 'a loc * 'a -> cmp

  val atomically : cas list -> cmp list -> bool
end = struct
  type 'a loc = 'a state Atomic.t
  and 'a state = { before : 'a; after : 'a; casn : casn }
  and cass = CASS : 'a loc * 'a state -> cass
  and casn = status Atomic.t

  and status =
    | Undetermined of { cass : cass list; cmps : cass list }
    | After
    | Before

  let make after =
    Atomic.make { before = after; after; casn = Atomic.make After }

  type cas = CAS : 'a loc * 'a * 'a -> cas
  type cmp = CMP : 'a loc * 'a -> cmp

  let finish casn desired =
    match Atomic.get casn with
    | After -> true
    | Before -> false
    | Undetermined { cmps; _ } as current ->
        let desired =
          if
            desired == After
            && cmps
               |> List.exists @@ fun (CASS (loc, state)) ->
                  Atomic.get loc != state
          then Before
          else desired
        in
        Atomic.compare_and_set casn current desired |> ignore;
        Atomic.get casn == After

  let rec gkmz casn = function
    | [] -> finish casn After
    | CASS (loc, desired) :: continue as retry -> begin
        let current = Atomic.get loc in
        if desired == current then gkmz casn continue
        else
          let current_value = get_from current in
          if current_value != desired.before then finish casn Before
          else
            match Atomic.get casn with
            | Undetermined _ ->
                if Atomic.compare_and_set loc current desired then
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
         | CAS (loc, before, after) -> CASS (loc, { before; after; casn })
    in
    let cmps =
      logical_cmp_list
      |> List.map @@ function
         | CMP (loc, expected) ->
             let current = Atomic.get loc in
             if get_from current != expected then raise Exit
             else CASS (loc, current)
    in
    Atomic.set casn (Undetermined { cass; cmps });
    gkmz casn cass

  let atomically logical_cas_list logical_cmp_list =
    try atomically logical_cas_list logical_cmp_list with Exit -> false

  let get loc = get_from (Atomic.get loc)
end
