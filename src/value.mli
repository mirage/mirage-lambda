type ('a, 'b) either = ('a, 'b) T.either =
  | L of 'a
  | R of 'b

type t =
  | Unit
  | Int of int
  | Int32 of int32
  | Int64 of int64
  | Bool of bool
  | String of string
  | Bytes of Bytes.t
  | List of Parsetree.Type.t * t list
  | Array of Parsetree.Type.t * t array
  | Option of Parsetree.Type.t * t option
  | Pair of (t * t)
  | Either of (t, t) either * Parsetree.Type.t * Parsetree.Type.t
  | Result of (t, t) result * Parsetree.Type.t * Parsetree.Type.t
  | Return of (t * Parsetree.Type.t)
  | Abstract

val unsafe_value: Parsetree.value -> t
