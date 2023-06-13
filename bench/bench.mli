module Times : sig
  type t

  val record :
    n_domains:int ->
    ?n_warmups:int ->
    ?n_runs:int ->
    ?before:(unit -> unit) ->
    init:(int -> 's) ->
    work:(int -> 's -> unit) ->
    ?after:(unit -> unit) ->
    unit ->
    t

  val invert : t -> t
end

module Stats : sig
  type t

  val of_times : Times.t -> t
  val scale : float -> t -> t

  val to_json :
    name:string -> description:string -> units:string -> t -> Yojson.Safe.t list
end
