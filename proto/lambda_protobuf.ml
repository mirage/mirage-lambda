module Types = Lambda_types
module Pb = Lambda_pb
module Rpc = Lambda_rpc

type ('a, 'b) either = ('a, 'b) Lambda.Value.either =
  | L of 'a
  | R of 'b

let opt_eq ~eq a b = match a, b with
  | Some a, Some b -> eq a b
  | None, None -> true
  | _, _ -> false

let lst_eq ~eq a b =
  try List.for_all2 eq a b
  with _ -> false

let arr_eq ~eq a b =
  try List.for_all2 eq (Array.to_list a) (Array.to_list b)
  with _ -> false

let pair_eq ~eqa ~eqb (x, y) (a, b) = eqa x a && eqb y b

let either_eq ~eql ~eqr a b = match a, b with
  | L a, L b -> eql a b
  | R a, R b -> eqr a b
  | _, _ -> false

let result_eq ~eq_ok ~eq_error a b = match a, b with
  | Ok a, Ok b -> eq_ok a b
  | Error a, Error b -> eq_error a b
  | _, _ -> false

let pp_either
  : ppl:'a Fmt.t -> ppr:'b Fmt.t -> ('a, 'b) Lambda.Value.either Fmt.t
  = fun ~ppl ~ppr ppf -> function
  | L l -> Fmt.pf ppf "(L %a)" ppl l
  | R r -> Fmt.pf ppf "(R %a)" ppr r

open Lambda.Parsetree

module Gamma = Map.Make(String)
module Primitives = Map.Make(String)

let typ_from ?(gamma = Gamma.empty) =
  let rec go : Types.type_ -> Type.t = function
    | Types.Unit -> Type.unit
    | Types.Int -> Type.int
    | Types.Int32 -> Type.int32
    | Types.Int64 -> Type.int64
    | Types.Bool -> Type.bool
    | Types.String -> Type.string
    | Types.Bytes -> Type.bytes
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

let typ_to =
  let rec go : Type.t -> Types.type_ = function
    | Type.Unit -> Types.Unit
    | Type.Int -> Types.Int
    | Type.Int32 -> Types.Int32
    | Type.Int64 -> Types.Int64
    | Type.Bool -> Types.Bool
    | Type.String -> Types.String
    | Type.Bytes -> Types.Bytes
    | Type.Lwt -> Types.Lwt
    | Type.List t -> Types.List { value = go t }
    | Type.Array t -> Types.Array { value = go t }
    | Type.Option t -> Types.Option { value = go t }
    | Type.Apply (a, b) -> Types.Apply { a = go a; b = go b; }
    | Type.Arrow (a, b) -> Types.Arrow { a = go a; b = go b; }
    | Type.Pair (a, b) -> Types.Pair { a = go a; b = go b; }
    | Type.Either (a, b) -> Types.Either { a = go a; b = go b; }
    | Type.Result (a, b) -> Types.Result { a = go a; b = go b; }
    | Type.Abstract (Type.A witness) -> Types.Abstract { witness = witness.Lambda.Eq.name }
  in go

let binop_from : Types.binop -> binop = function
  | Types.Add -> `Add
  | Types.Sub -> `Sub
  | Types.Mul -> `Mul
  | Types.Div -> `Div
  | Types.Pair -> `Pair
  | Types.Eq -> `Eq

let binop_to : binop -> Types.binop = function
  | `Add -> Types.Add
  | `Sub -> Types.Sub
  | `Mul -> Types.Mul
  | `Div -> Types.Div
  | `Pair -> Types.Pair
  | `Eq -> Types.Eq

let unop_from : ?gamma:Type.abstract Gamma.t -> Types.unop -> unop = fun ?gamma -> function
  | Types.Fst -> Fst
  | Types.Snd -> Snd
  | Types.L { value; } -> L (typ_from ?gamma value)
  | Types.R { value; } -> R (typ_from ?gamma value)
  | Types.Ok { value; } -> Ok (typ_from ?gamma value)
  | Types.Error { value; } -> Error (typ_from ?gamma value)

let unop_to : unop -> Types.unop = function
  | Fst -> Types.Fst
  | Snd -> Types.Snd
  | L t -> Types.L { value = typ_to t }
  | R t -> Types.R { value = typ_to t }
  | Ok t -> Types.Ok { value = typ_to t }
  | Error t -> Types.Error { value = typ_to t }

let rec value_from : Types.value -> value = function
  | Types.Unit ->
    V { v = (); t = Unit;
        pp = (fun ppf () -> Fmt.pf ppf "()"); eq = (fun () () -> true); }
  | Types.Int { value; } ->
    V { v = Int32.to_int value; t = Int; pp = Fmt.int; eq = Pervasives.(=); }
  | Types.Int32 { value; } ->
    V { v = value; t = Int32; pp = Fmt.int32; eq = Pervasives.(=) }
  | Types.Int64 { value; } ->
    V { v = value; t = Int64; pp = Fmt.int64; eq = Pervasives.(=) }
  | Types.Bool { value; } ->
    V { v = value; t = Bool; pp = Fmt.bool; eq = Pervasives.(=) }
  | Types.String { value; } ->
    V { v = value; t = String; pp = Fmt.string; eq = Pervasives.(=) }
  | Types.Bytes { value; } ->
    V { v = Bytes.of_string value; t = Bytes; pp = Fmt.using Bytes.unsafe_to_string Fmt.string; eq = Pervasives.(=) }
  | Types.Option { typ; value = Some value; } ->
    let V { v; t; pp; eq; } = value_from value in
    let Lambda.Type.V t' = Lambda.Type.typ (typ_from typ) in
    (match Lambda.Type.equal t t' with
     | Some Lambda.Eq.Refl -> V { v = Some v; t = Option t;
                                  pp = Fmt.option pp; eq = opt_eq ~eq }
     | None -> Fmt.invalid_arg "Cannot unify %a and %a"
                 Lambda.Type.pp t
                 Lambda.Type.pp t')
  | Types.Option { typ; value = None; } ->
    let Lambda.Type.V t = Lambda.Type.typ (typ_from typ) in
    let pp = Lambda.Type.pp_val t in
    let eq = Lambda.Type.eq_val t in
    V { v = None; t = Option t; pp = Fmt.option pp; eq = opt_eq ~eq }
  | Types.List { typ; value = []; } ->
    let Lambda.Type.V t = Lambda.Type.typ (typ_from typ) in
    let pp = Lambda.Type.pp_val t in
    let eq = Lambda.Type.eq_val t in
    V { v = []; t = List t; pp = Fmt.list pp; eq = lst_eq ~eq }
  | Types.Array { typ; value = []; } ->
    let Lambda.Type.V t = Lambda.Type.typ (typ_from typ) in
    let pp = Lambda.Type.pp_val t in
    let eq = Lambda.Type.eq_val t in
    V { v = [||]; t = Array t; pp = Fmt.array pp; eq = arr_eq ~eq }
  | Types.List { typ; value; } ->
    let Lambda.Type.V t' = Lambda.Type.typ (typ_from typ) in
    let pp = Lambda.Type.pp_val (List t') in
    let eq = Lambda.Type.eq_val (List t') in
    let rec go acc = function
      | [] ->
        let V { v; t; pp; eq; } = acc in
        (match Lambda.Type.equal t (List t') with
         | Some Lambda.Eq.Refl -> V { v = List.rev v; t; pp; eq; }
         | None -> Fmt.invalid_arg "Cannot unify %a and %a"
                     Lambda.Type.pp t
                     Lambda.Type.pp (List t'))
      | x :: r ->
        let V { v = x; t; pp = _; eq = _; } = value_from x in
        let V { v = acc; t = tacc; pp; eq; } = acc in
        (match Lambda.Type.equal t t', Lambda.Type.equal (List t) tacc with
         | Some Lambda.Eq.Refl, Some Lambda.Eq.Refl ->
           go (V { v = x :: acc; t = List t; pp; eq; }) r
         | _, _ -> Fmt.invalid_arg "Cannot unify %a and %a"
                     Lambda.Type.pp t
                     Lambda.Type.pp t) in
    go (V { v = []; t = List t'; pp; eq; }) value
  | Types.Array { typ; value; } ->
    let Lambda.Type.V t' = Lambda.Type.typ (typ_from typ) in
    let pp = Lambda.Type.pp_val (List t') in
    let eq = Lambda.Type.eq_val (List t') in
    let cast (V { v; t; _ }) = match Lambda.Type.equal (List t') t with
      | Some Lambda.Eq.Refl ->
        let pp = Lambda.Type.pp_val (Array t') in
        let eq = Lambda.Type.eq_val (Array t') in
        V { v = Array.of_list v; t = Array t'; pp; eq; }
      | None -> Fmt.invalid_arg "Cannot unify %a and %a"
                  Lambda.Type.pp (List t')
                  Lambda.Type.pp t in
    let rec go acc = function
      | [] ->
        let V { v; t; pp; eq; } = acc in
        (match Lambda.Type.equal t (List t') with
         | Some Lambda.Eq.Refl -> V { v = List.rev v; t; pp; eq; }
         | None -> Fmt.invalid_arg "Cannot unify %a and %a"
                     Lambda.Type.pp t
                     Lambda.Type.pp (List t'))
      | x :: r ->
        let V { v = x; t; pp = _; eq = _; } = value_from x in
        let V { v = acc; t = tacc; pp; eq; } = acc in
        (match Lambda.Type.equal t t', Lambda.Type.equal (List t) tacc with
         | Some Lambda.Eq.Refl, Some Lambda.Eq.Refl ->
           go (V { v = x :: acc; t = List t; pp; eq; }) r
         | _, _ -> Fmt.invalid_arg "Cannot unify %a and %a"
                     Lambda.Type.pp t
                     Lambda.Type.pp t) in
    go (V { v = []; t = List t'; pp; eq; }) value |> cast
  | Types.Pair { a; b; } ->
    let V { v = a; t = ta; pp = ppa; eq = eqa; } = value_from a in
    let V { v = b; t = tb; pp = ppb; eq = eqb; } = value_from b in
    V { v = (a, b); t = Pair (ta, tb); pp = Fmt.pair ppa ppb; eq = pair_eq ~eqa ~eqb }
  | Types.Either { value = Types.Left { value; }; typ_l; typ_r; } ->
    let V { v; t; pp = ppl; eq = eql; } = value_from value in
    let Lambda.Type.V tl = Lambda.Type.typ (typ_from typ_l) in
    let Lambda.Type.V tr = Lambda.Type.typ (typ_from typ_r) in

    let ppr, eqr = Lambda.Type.pp_val tr, Lambda.Type.eq_val tr in

    (match Lambda.Type.equal t tl with
     | Some Lambda.Eq.Refl -> V { v = Lambda.Value.L v; t = Either (tl, tr); pp = pp_either ~ppl ~ppr; eq = either_eq ~eql ~eqr }
     | None -> Fmt.invalid_arg "Cannot unify %a and %a"
                 Lambda.Type.pp t
                 Lambda.Type.pp tl)
  | Types.Either { value = Types.Right { value; }; typ_l; typ_r; } ->
    let V { v; t; pp = ppr; eq = eqr; } = value_from value in
    let Lambda.Type.V tl = Lambda.Type.typ (typ_from typ_l) in
    let Lambda.Type.V tr = Lambda.Type.typ (typ_from typ_r) in

    let ppl, eql = Lambda.Type.pp_val tl, Lambda.Type.eq_val tl in

    (match Lambda.Type.equal t tr with
     | Some Lambda.Eq.Refl -> V { v = Lambda.Value.R v; t = Either (tl, tr); pp = pp_either ~ppl ~ppr; eq = either_eq ~eql ~eqr }
     | None -> Fmt.invalid_arg "Cannot unify %a and %a"
                 Lambda.Type.pp t
                 Lambda.Type.pp tl)
  | Types.Result { value = Types.Ok { value; }; typ_ok; typ_error; } ->
    let V { v; t; pp = pp_ok; eq = eq_ok; } = value_from value in
    let Lambda.Type.V tok = Lambda.Type.typ (typ_from typ_ok) in
    let Lambda.Type.V terror = Lambda.Type.typ (typ_from typ_error) in

    let pp_error, eq_error = Lambda.Type.pp_val terror, Lambda.Type.eq_val terror in

    (match Lambda.Type.equal t tok with
     | Some Lambda.Eq.Refl -> V { v = Pervasives.Ok v; t = Result (tok, terror); pp = Fmt.result ~ok:pp_ok ~error:pp_error; eq = result_eq ~eq_ok ~eq_error }
     | None -> Fmt.invalid_arg "Cannot unify %a and %a"
                 Lambda.Type.pp t
                 Lambda.Type.pp tok)
  | Types.Result { value = Types.Error { value; }; typ_ok; typ_error; } ->
    let V { v; t; pp = pp_error; eq = eq_error; } = value_from value in
    let Lambda.Type.V tok = Lambda.Type.typ (typ_from typ_ok) in
    let Lambda.Type.V terror = Lambda.Type.typ (typ_from typ_error) in

    let pp_ok, eq_ok = Lambda.Type.pp_val tok, Lambda.Type.eq_val tok in

    (match Lambda.Type.equal t terror with
     | Some Lambda.Eq.Refl -> V { v = Pervasives.Error v; t = Result (tok, terror); pp = Fmt.result ~ok:pp_ok ~error:pp_error; eq = result_eq ~eq_ok ~eq_error }
     | None -> Fmt.invalid_arg "Cannot unify %a and %a"
                 Lambda.Type.pp t
                 Lambda.Type.pp terror)
  | Types.Return { value; _ } -> value_from value
  | Types.Abstract -> invalid_arg "Impossible to un-serialize an abstract value"

module Option = struct let map f = function Some v -> Some (f v) | None -> None end

let value_to : value -> Types.value = fun v ->
  let rec go : Lambda.Value.t -> Types.value = function
    | Lambda.Value.Unit -> Types.Unit
    | Lambda.Value.Int value -> Types.Int { value = Int32.of_int value; }
    | Lambda.Value.Int32 value -> Types.Int32 { value; }
    | Lambda.Value.Int64 value -> Types.Int64 { value; }
    | Lambda.Value.Bool value -> Types.Bool { value; }
    | Lambda.Value.String value -> Types.String { value; }
    | Lambda.Value.List (typ, value) -> Types.List { typ = typ_to typ; value = List.map go value; }
    | Lambda.Value.Array (typ, value) -> Types.Array { typ = typ_to typ; value = Array.map go value |> Array.to_list; }
    | Lambda.Value.Option (typ, value) -> Types.Option { typ = typ_to typ; value = Option.map go value; }
    | Lambda.Value.Bytes value -> Types.Bytes { value = Bytes.to_string value; }
    | Lambda.Value.Pair (a, b) -> Types.Pair { a = go a; b = go b; }
    | Lambda.Value.Either (L value, typl, typr) -> Types.Either { value = Types.Left { value = go value }; typ_l = typ_to typl; typ_r = typ_to typr }
    | Lambda.Value.Either (R value, typl, typr) -> Types.Either { value = Types.Right { value = go value }; typ_l = typ_to typl; typ_r = typ_to typr }
    | Lambda.Value.Result (Ok value, typ_ok, typ_error) -> Types.Result { value = Types.Ok { value = go value }; typ_ok = typ_to typ_ok; typ_error = typ_to typ_error }
    | Lambda.Value.Result (Error value, typ_ok, typ_error) -> Types.Result { value = Types.Error { value = go value }; typ_ok = typ_to typ_ok; typ_error = typ_to typ_error }
    | Lambda.Value.Return (value, typ) -> Types.Return { typ = typ_to typ; value = go value; }
    | Lambda.Value.Abstract -> Types.Abstract in
  go (Lambda.Value.unsafe_value v)

let expr_from
  : ?gamma:Type.abstract Gamma.t ->
    ?primitives:primitive Primitives.t ->
    Types.expr -> expr
  = fun ?gamma ?(primitives = Primitives.empty) ->
    let rec go : Types.expr -> expr = function
      | Types.Val { value = Types.Unit; } -> unit
      | Types.Val { value = Types.Int { value; }; } -> int (Int32.to_int value)
      | Types.Val { value = Types.Int32 { value; }; } -> int32 value
      | Types.Val { value = Types.Int64 { value; }; } -> int64 value
      | Types.Val { value = Types.Bool { value = true; }; } -> true_
      | Types.Val { value = Types.Bool { value = false; }; } -> false_
      | Types.Val { value = Types.String { value; }; } -> string value
      | Types.Val { value = x; } ->
        let V { v; t; pp; eq; } = value_from x in
        value v t pp eq
      | Types.Prm { value = { name
                            ; arguments
                            ; return } } ->
        (match Primitives.find name primitives with
         | primitive ->
           if String.equal primitive.name name
           && List.for_all2 equal_typ (List.map (typ_from ?gamma) arguments) primitive.args
           && equal_typ (typ_from ?gamma return) primitive.ret
           then prim primitive
           else Fmt.invalid_arg "Remote primitive %s mismatch with local primitive" name
         | exception Not_found -> Fmt.invalid_arg "Primitive %s not found" name)
      | Types.Lst { typ; expr; } ->
        list ?typ:(Option.map (typ_from ?gamma) typ) (List.map go expr)
      | Types.Arr { typ; expr; } ->
        array ?typ:(Option.map (typ_from ?gamma) typ) (Array.of_list (List.map go expr))
      | Types.Opt { typ; expr = None; } ->
        none (typ_from ?gamma typ)
      | Types.Opt { expr = Some expr; _ } ->
        some (go expr)
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
        (go a) = (go b) (* XXX(dinosaure): we use Parsetree.(=). *)
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
      | Types.Ret { expr; } ->
        return (go expr)
      | Types.Bnd { expr; func; } ->
        bind (go expr) (go func)
      | Types.If { a; b; s; } ->
        if_ (go s) (go a) (go b) in
    go

let rec expr_to : expr -> Types.expr = function
  | Val v -> Types.Val { value = value_to v; }
  | Prm { name; args; ret; _ } -> Types.Prm { value = { name; arguments = List.map typ_to args; return = typ_to ret; }; }
  | Lst (typ, lst) -> Types.Lst { typ = Option.map typ_to typ; expr = List.map expr_to lst; }
  | Arr (typ, arr) -> Types.Arr { typ = Option.map typ_to typ; expr = Array.map expr_to arr |> Array.to_list; }
  | Opt (Some typ, opt) -> Types.Opt { typ = typ_to typ; expr = Option.map expr_to opt; }
  | Opt (None, _) -> invalid_arg "Optional construction needs to be typed"
  | Ret expr -> Types.Ret { expr = expr_to expr; }
  | Bnd (expr, func) -> Types.Bnd { expr = expr_to expr; func = expr_to func; }
  | Var { id; } -> Types.Var { var = Int32.of_int id; }
  | Lam (typ, var, expr) -> Types.Lam { typ = typ_to typ; var; expr = expr_to expr; }
  | Rec { r = ret; p = (name, argument); e = expr; } ->
    Types.Rec { ret = typ_to ret
              ; name
              ; argument = typ_to argument
              ; expr = expr_to expr; }
  | App (a, b) -> Types.App { a = expr_to a; b = expr_to b; }
  | Bin (op, a, b) -> Types.Bin { op = binop_to op; a = expr_to a; b = expr_to b; }
  | Uno (op, x) -> Types.Uno { op = unop_to op; x = expr_to x; }
  | Let (typ, name, expr, body) ->
    Types.Let { typ = typ_to typ; name; expr = expr_to expr; body = expr_to body; }
  | Swt { a; b; s; } -> Types.Swt { a = expr_to a; b = expr_to b; s = expr_to s; }
  | If (s, a, b) -> Types.If { a = expr_to a; b = expr_to b; s = expr_to s; }

let request_from
  : ?gamma:Type.abstract Gamma.t ->
    ?primitives:primitive Primitives.t ->
    Types.request -> expr * Type.t * int64
  = fun ?gamma ?primitives request ->
    expr_from ?gamma ?primitives request.Types.expr, typ_from ?gamma request.Types.typ, request.Types.output

let request_to
  : expr * Type.t * int64 -> Types.request
  = fun (expr, typ, output) ->
    { Types.expr = expr_to expr
    ; typ = typ_to typ
    ; output }

let reply_from
  : Types.reply -> (value, [ `Msg of string ]) result
  = function
    | Types.Value value -> Ok (value_from value)
    | Types.Error err -> Error (`Msg err)

let reply_to
  : (value, [ `Msg of string ]) result -> Types.reply
  = function
    | Ok value -> Types.Value (value_to value)
    | Error (`Msg err) -> Types.Error err
