open Lambda_protobuf
open Parsetree

module Gamma = Map.Make(String)
module Primitives = Map.Make(Int32)

let typ_from ?(gamma = Gamma.empty) =
  let rec go : Types.type_ -> Type.t = function
    | Types.Unit -> Type.unit
    | Types.Int -> Type.int
    | Types.Int32 -> Type.int32
    | Types.Int64 -> Type.int64
    | Types.Bool -> Type.bool
    | Types.String -> Type.string
    | Types.Lwt -> Type.lwt
    | Types.List { value; } -> Type.list (go value)
    | Types.Array { value; } -> Type.array (go value)
    | Types.Option { value; } -> Type.option (go value)
    | Types.Apply { a; b; } -> Type.apply (go a) (go b)
    | Types.Arrow { a; b; } -> Type.(go a @-> go b)
    | Types.Pair { a; b; } -> Type.(go a ** go b)
    | Types.Either { a; b; } -> Type.(go a || go b)
    | Types.Result { a; b; } -> Type.result (go a) (go b)
    | Types.Abstract { witness; } ->
      try Type.unsafe_abstract (Gamma.find witness gamma)
      with Not_found -> Fmt.invalid_arg "Abstract type %s not found" witness
  in go

let binop_from : Types.binop -> binop = function
  | Types.Add -> `Add
  | Types.Sub -> `Sub
  | Types.Mul -> `Mul
  | Types.Div -> `Div
  | Types.Pair -> `Pair
  | Types.Eq -> `Eq

let unop_from : ?gamma:Type.abstract Gamma.t -> Types.unop -> unop = fun ?gamma -> function
  | Types.Fst -> Fst
  | Types.Snd -> Snd
  | Types.L { value; } -> L (typ_from ?gamma value)
  | Types.R { value; } -> R (typ_from ?gamma value)
  | Types.Ok { value; } -> Ok (typ_from ?gamma value)
  | Types.Error { value; } -> Error (typ_from ?gamma value)

let rec value_from : Types.value -> value = function
  | Types.Unit ->
    V { v = (); t = T.Unit; pp = (fun ppf () -> Fmt.pf ppf "()"); eq = (fun () () -> true); }
  | Types.Int { value; } ->
    V { v = Int32.to_int value; t = T.Int; pp = Fmt.int; eq = Pervasives.(=); }
  | Types.Int32 { value; } ->
    V { v = value; t = T.Int32; pp = Fmt.int32; eq = Pervasives.(=) }
  | Types.Int64 { value; } ->
    V { v = value; t = T.Int64; pp = Fmt.int64; eq = Pervasives.(=) }
  | Types.Option { value = Some value; } ->
    let V { v; t; pp; eq; } = value_from value in
    let eq a b = match a, b with
      | Some a, Some b -> eq a b
      | None, None -> true
      | _, _ -> false in
    V { v = Some v; t = T.Option t; pp = Fmt.option pp; eq; }
  | _ -> assert false

module Option = struct let map f = function Some v -> Some (f v) | None -> None end

let expr_from
  : ?gamma:Type.abstract Gamma.t ->
    ?primitives:(value list -> value) Primitives.t ->
    Types.expr -> expr
  = fun ?gamma ?(primitives = Primitives.empty) ->
    let rec go : Types.expr -> expr = function
      | Types.Val { value = Types.Unit; } -> unit
      | Types.Val { value = Types.Int { value; }; } -> int (Int32.to_int value)
      | Types.Val { value = Types.Int32 { value; }; } -> int32 value
      | Types.Val { value = Types.Int64 { value; }; } -> int64 value
      | Types.Val { value = Types.Bool { value = true; }; } -> true_
      | Types.Val { value = Types.Bool { value = false; }; } -> false_
      | Types.Val { value = x; } ->
        let V { v; t; pp; eq; } = value_from x in
        value v t pp eq
      | Types.Prm { value = { name
                            ; arguments
                            ; return
                            ; smartptr } } ->
        (try
           primitive
             name
             (List.map (typ_from ?gamma) arguments)
             (typ_from ?gamma return)
             (Primitives.find smartptr primitives)
         with Not_found -> Fmt.invalid_arg "Primitive %s:%ld not found" name smartptr)
      | Types.Lst { typ; expr; } ->
        list ?typ:(Option.map (typ_from ?gamma) typ) (List.map go expr)
      | Types.Arr { typ; expr; } ->
        array ?typ:(Option.map (typ_from ?gamma) typ) (Array.of_list (List.map go expr))
      | Types.Opt { typ = Some typ; expr = None; } ->
        none (typ_from ?gamma typ)
      | Types.Opt { expr = Some expr; _ } ->
        some (go expr)
      | Types.Opt { typ = None; expr = None; } ->
        invalid_arg "Impossible to de-serialize a None value without a type"
      | Types.Var { var = id; } ->
        var (Int32.to_int id)
      | Types.Lam { typ; var; expr; } ->
        lambda [ var, typ_from ?gamma typ ] (go expr)
      | Types.Rec { ret; name; argument; expr; } ->
        fix (name, typ_from ?gamma argument) (typ_from ?gamma ret) (go expr)
      | Types.App { a; b; } ->
        apply (go a) (go b)
      | Types.Bin { op = Types.Add; a; b; } ->
        (go a) + (go b)
      | Types.Bin { op = Types.Sub; a; b; } ->
        (go a) - (go b)
      | Types.Bin { op = Types.Mul; a; b; } ->
        (go a) * (go b)
      | Types.Bin { op = Types.Div; a; b; } ->
        (go a) / (go b)
      | Types.Bin { op = Types.Pair; a; b; } ->
        pair (go a) (go b)
      | Types.Bin { op = Types.Eq; a; b; } ->
        (go a) = (go b) (* XXX(dinosaure): take care we use Parsetree.(=). *)
      | Types.Uno { op = Types.Fst; x; } ->
        fst (go x)
      | Types.Uno { op = Types.Snd; x; } ->
        snd (go x)
      | Types.Uno { op = Types.L { value; }; x; } ->
        left (typ_from ?gamma value) (go x)
      | Types.Uno { op = Types.R { value; }; x; } ->
        right (typ_from ?gamma value) (go x)
      | Types.Uno { op = Types.Ok { value; }; x; } ->
        ok (typ_from ?gamma value) (go x)
      | Types.Uno { op = Types.Error { value }; x; } ->
        error (typ_from ?gamma value) (go x)
      | Types.Let { typ; name; expr; body; } ->
        let_var (typ_from ?gamma typ) name (go expr) (go body)
      | Types.Swt { a; b; s; } ->
        match_ (go s) (go a) (go b)
      | Types.If { a; b; s; } ->
        if_ (go s) (go a) (go b) in
    go
