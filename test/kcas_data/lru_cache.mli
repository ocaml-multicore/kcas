open Kcas
open Kcas_data

type ('k, 'v) t

val create : ?hashed_type:'k Hashtbl.hashed_type -> int -> ('k, 'v) t

module Xt :
  Lru_cache_intf.Ops
    with type ('k, 'v) t := ('k, 'v) t
    with type ('x, 'fn) fn := xt:'x Xt.t -> 'fn

include
  Lru_cache_intf.Ops
    with type ('k, 'v) t := ('k, 'v) t
    with type ('x, 'fn) fn := 'fn
