module Kcas : sig
  type 'a loc

  val make : 'a -> 'a loc
  val get : 'a loc -> 'a

  type cas = CAS : 'a loc * 'a * 'a -> cas | CMP : 'a loc * 'a -> cas

  val atomically : cas list -> bool
end = struct
  type 'a loc = 'a state Atomic.t
  and 'a state = { before : 'a; after : 'a; casn : casn }
  and cass = CASS : 'a loc * 'a state -> cass
  and casn = status Atomic.t
  and status = Undetermined of cass list | After | Before

  let make after =
    Atomic.make { before = after; after; casn = Atomic.make After }

  type cas = CAS : 'a loc * 'a * 'a -> cas | CMP : 'a loc * 'a -> cas

  let is_cmp casn state = state.casn != casn

  let finish casn desired =
    match Atomic.get casn with
    | After -> true
    | Before -> false
    | Undetermined cass as current ->
        let desired =
          if
            desired == After
            && cass
               |> List.exists @@ fun (CASS (loc, state)) ->
                  is_cmp casn state && Atomic.get loc != state
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
        else if is_cmp casn desired then finish casn Before
        else
          let current_value =
            if is_after current.casn then current.after else current.before
          in
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

  and is_after casn =
    match Atomic.get casn with
    | Undetermined cass -> gkmz casn cass
    | After -> true
    | Before -> false

  let get loc =
    let state = Atomic.get loc in
    if is_after state.casn then state.after else state.before

  let atomically logical_cas_list =
    let casn = Atomic.make After in
    let cass =
      logical_cas_list
      |> List.map @@ function
         | CAS (loc, before, after) -> CASS (loc, { before; after; casn })
         | CMP (loc, expected) ->
             let current = Atomic.get loc in
             if get loc != expected || Atomic.get loc != current then raise Exit
             else CASS (loc, current)
    in
    Atomic.set casn (Undetermined cass);
    gkmz casn cass

  let atomically logical_cas_list =
    try atomically logical_cas_list with Exit -> false

  let get loc =
    let state = Atomic.get loc in
    if is_after state.casn then state.after else state.before
end
