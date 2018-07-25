open Lambda.Parsetree

module Rpc: (module type of Lambda_rpc)
module Pb: (module type of Lambda_pb)

module Primitives: Map.S with type key = string
module Gamma: Map.S with type key = string

type error = [`Msg of string]

val to_request: expr * Type.t * int64 -> (Lambda_types.request, error) result

val of_request:
  ?gamma:Type.abstract Gamma.t ->
  ?primitives:primitive Primitives.t ->
  Lambda_types.request -> expr * Type.t * int64

val output_of_request: Lambda_types.request -> int64

val of_reply:
  ?gamma:Type.abstract Gamma.t -> Lambda_types.reply ->
  (value, [ `Msg of string ]) result

val to_reply: (value, [ `Msg of string ]) result -> Lambda_types.reply
