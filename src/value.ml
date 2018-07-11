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

let option_map f = function
  | Some v -> Some (f v)
  | None -> None

let pair_map fa fb = fun (a, b) -> (fa a, fb b)

let either_map fa fb = function
  | L a -> L (fa a)
  | R b -> R (fb b)

let result_map fa fb = function
  | Ok a -> Ok (fa a)
  | Error b -> Error (fb b)

let unsafe_value : Parsetree.value -> t = fun (Parsetree.V x) ->
  let rec go : type a. a T.t -> a -> t = fun proof value -> match proof with
    | T.Unit -> Unit
    | T.Int -> Int value
    | T.Int32 -> Int32 value
    | T.Int64 -> Int64 value
    | T.Bool -> Bool value
    | T.String -> String value
    | T.Bytes -> Bytes value
    | T.List t -> List (Typedtree.Type.untype t, List.map (go t) value)
    | T.Array t -> Array (Typedtree.Type.untype t, Array.map (go t) value)
    | T.Option t -> Option (Typedtree.Type.untype t, option_map (go t) value)
    | T.Pair (ta, tb) -> Pair (pair_map (go ta) (go tb) value)
    | T.Either (ta, tb) -> Either (either_map (go ta) (go tb) value, Typedtree.Type.untype ta, Typedtree.Type.untype tb)
    | T.Result (ta, tb) -> Result (result_map (go ta) (go tb) value, Typedtree.Type.untype ta, Typedtree.Type.untype tb)
    | _ -> invalid_arg "Unsafe_value.unsafe_value: invalid type" in
  go x.t x.v
