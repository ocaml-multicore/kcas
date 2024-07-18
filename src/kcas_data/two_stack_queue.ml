open Kcas

type 'a t = { head : 'a head Loc.t; tail : 'a tail Loc.t }

and ('a, _) tdt =
  | Cons : {
      counter : int;
      value : 'a;
      suffix : 'a head;
    }
      -> ('a, [> `Cons ]) tdt
  | Head : { counter : int } -> ('a, [> `Head ]) tdt
  | Snoc : {
      counter : int;
      prefix : 'a tail;
      value : 'a;
    }
      -> ('a, [> `Snoc ]) tdt
  | Tail : {
      counter : int;
      mutable move : ('a, [ `Snoc | `Used ]) tdt;
    }
      -> ('a, [> `Tail ]) tdt
  | Used : ('a, [> `Used ]) tdt

and 'a head = H : ('a, [< `Cons | `Head ]) tdt -> 'a head [@@unboxed]
and 'a tail = T : ('a, [< `Snoc | `Tail ]) tdt -> 'a tail [@@unboxed]

(* *)

let create () =
  let head = Loc.make ~padded:true (H (Head { counter = 1 })) in
  let tail = Loc.make ~padded:true (T (Tail { counter = 0; move = Used })) in
  { head; tail } |> Multicore_magic.copy_as_padded

(* *)

let rec rev (suffix : (_, [< `Cons ]) tdt) = function
  | T (Snoc { counter; prefix; value }) ->
      rev (Cons { counter; value; suffix = H suffix }) prefix
  | T (Tail _) -> suffix

let[@inline] rev = function
  | (Snoc { counter; prefix; value } : (_, [< `Snoc ]) tdt) ->
      rev
        (Cons { counter; value; suffix = H (Head { counter = counter + 1 }) })
        prefix

(* *)

let rec push backoff t value =
  match Loc.fenceless_get t.tail with
  | T (Snoc snoc_r) as prefix -> push_with backoff t snoc_r.counter prefix value
  | T (Tail tail_r as tail) ->
      begin
        match tail_r.move with
        | Used -> ()
        | Snoc move_r as move -> begin
            match Loc.fenceless_get t.head with
            | H (Head head_r as head) when head_r.counter < move_r.counter ->
                let after = rev move in
                if Loc.compare_and_set t.head (H head) (H after) then
                  tail_r.move <- Used
            | _ -> ()
          end
      end;
      push_with backoff t tail_r.counter (T tail) value

and push_with backoff t counter prefix value =
  let after = Snoc { counter = counter + 1; prefix; value } in
  if not (Loc.compare_and_set t.tail prefix (T after)) then
    push (Backoff.once backoff) t value

let[@inline] push t value = push Backoff.default t value

(* *)

exception Empty

let rec pop backoff t =
  match Loc.get t.head with
  | H (Cons cons_r) as before ->
      let after = cons_r.suffix in
      if Loc.compare_and_set t.head before after then cons_r.value
      else pop (Backoff.once backoff) t
  | H (Head head_r as head) -> begin
      match Loc.fenceless_get t.tail with
      | T (Snoc snoc_r as move) ->
          if head_r.counter = snoc_r.counter then
            if Loc.compare_and_set t.tail (T move) snoc_r.prefix then
              snoc_r.value
            else pop backoff t
          else
            let tail = Tail { counter = snoc_r.counter; move } in
            if
              Loc.fenceless_get t.head == H head
              && Loc.compare_and_set t.tail (T move) (T tail)
            then pop_moving backoff t head move tail
            else pop backoff t
      | T (Tail tail_r as tail) -> begin
          match tail_r.move with
          | Used -> pop_emptyish backoff t head
          | Snoc _ as move -> pop_moving backoff t head move tail
        end
    end

and pop_moving backoff t (Head head_r as head : (_, [< `Head ]) tdt)
    (Snoc move_r as move : (_, [< `Snoc ]) tdt)
    (Tail tail_r : (_, [< `Tail ]) tdt) =
  if head_r.counter < move_r.counter then
    match rev move with
    | Cons cons_r ->
        if Loc.compare_and_set t.head (H head) cons_r.suffix then begin
          tail_r.move <- Used;
          cons_r.value
        end
        else pop (Backoff.once backoff) t
  else pop_emptyish backoff t head

and pop_emptyish backoff t head =
  if Loc.get t.head == H head then raise_notrace Empty else pop backoff t

let[@inline] pop_opt t =
  match pop Backoff.default t with
  | value -> Some value
  | exception Empty -> None

let[@inline] pop t = pop Backoff.default t

(* *)

let rec length t =
  let head = Loc.get t.head in
  let tail = Loc.fenceless_get t.tail in
  if head != Loc.get t.head then length t
  else
    let head_at =
      match head with H (Cons r) -> r.counter | H (Head r) -> r.counter
    in
    let tail_at =
      match tail with T (Snoc r) -> r.counter | T (Tail r) -> r.counter
    in
    tail_at - head_at + 1
