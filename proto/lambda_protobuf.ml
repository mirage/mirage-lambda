module Types = Lambda_types
module Pb = Lambda_pb
module Rpc = Lambda_rpc

type error = [`Msg of string]

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

let to_typ ~gamma x =
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
  in
  go x

let of_typ =
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
    | Type.Abstract (Type.A (eq, _)) -> Types.Abstract { witness = eq.Lambda.Eq.name }
  in go

let to_binop : binop -> Types.binop = function
  | `Add -> Types.Add
  | `Sub -> Types.Sub
  | `Mul -> Types.Mul
  | `Div -> Types.Div
  | `Pair -> Types.Pair
  | `Eq -> Types.Eq
  | `Get -> Types.Get
  | `ShiftL -> Types.Shiftl
  | `ShiftR -> Types.Shiftr
  | `Or -> Types.Or
  | `Xor -> Types.Xor
  | `And -> Types.And

let to_unop : unop -> Types.unop = function
  | Prj -> Types.Prj
  | Fst -> Types.Fst
  | Snd -> Types.Snd
  | L t -> Types.L { value = of_typ t }
  | R t -> Types.R { value = of_typ t }
  | Ok t -> Types.Ok { value = of_typ t }
  | Error t -> Types.Error { value = of_typ t }
  | Not -> Types.Not

let of_value ~gamma ?(unwrap = true) x =
  let to_typ = to_typ ~gamma in
  let rec go: Types.value -> value = function
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
      V { v = value; t = Bytes;
          pp = Fmt.using Bytes.unsafe_to_string Fmt.string; eq = Pervasives.(=) }
    | Types.Option { typ; value = Some value; } ->
      let V { v; t; pp; eq; } = go value in
      let Lambda.Type.V t' = Lambda.Type.typ (to_typ typ) in
      (match Lambda.Type.equal t t' with
       | Some Lambda.Eq.Refl -> V { v = Some v; t = Option t;
                                    pp = Fmt.option pp; eq = opt_eq ~eq }
       | None -> Fmt.invalid_arg "Cannot unify %a and %a"
                   Lambda.Type.pp t
                   Lambda.Type.pp t')
    | Types.Option { typ; value = None; } ->
      let Lambda.Type.V t = Lambda.Type.typ (to_typ typ) in
      let pp = Lambda.Type.pp_val t in
      let eq = Lambda.Type.eq_val t in
      V { v = None; t = Option t; pp = Fmt.option pp; eq = opt_eq ~eq }
    | Types.List { typ; value = []; } ->
      let Lambda.Type.V t = Lambda.Type.typ (to_typ typ) in
      let pp = Lambda.Type.pp_val t in
      let eq = Lambda.Type.eq_val t in
      V { v = []; t = List t; pp = Fmt.list pp; eq = lst_eq ~eq }
    | Types.Array { typ; value = []; } ->
      let Lambda.Type.V t = Lambda.Type.typ (to_typ typ) in
      let pp = Lambda.Type.pp_val t in
      let eq = Lambda.Type.eq_val t in
      V { v = [||]; t = Array t; pp = Fmt.array pp; eq = arr_eq ~eq }
    | Types.List { typ; value; } ->
      let Lambda.Type.V t' = Lambda.Type.typ (to_typ typ) in
      let pp = Lambda.Type.pp_val (List t') in
      let eq = Lambda.Type.eq_val (List t') in
      let rec aux acc = function
        | [] ->
          let V { v; t; pp; eq; } = acc in
          (match Lambda.Type.equal t (List t') with
           | Some Lambda.Eq.Refl -> V { v = List.rev v; t; pp; eq; }
           | None -> Fmt.invalid_arg "Cannot unify %a and %a"
                       Lambda.Type.pp t
                       Lambda.Type.pp (List t'))
        | x :: r ->
          let V { v = x; t; pp = _; eq = _; } = go x in
          let V { v = acc; t = tacc; pp; eq; } = acc in
          (match Lambda.Type.equal t t', Lambda.Type.equal (List t) tacc with
           | Some Lambda.Eq.Refl, Some Lambda.Eq.Refl ->
             aux (V { v = x :: acc; t = List t; pp; eq; }) r
           | _, _ -> Fmt.invalid_arg "Cannot unify %a and %a"
                       Lambda.Type.pp t
                       Lambda.Type.pp t) in
      aux (V { v = []; t = List t'; pp; eq; }) value
    | Types.Array { typ; value; } ->
      let Lambda.Type.V t' = Lambda.Type.typ (to_typ typ) in
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
      let rec aux acc = function
        | [] ->
          let V { v; t; pp; eq; } = acc in
          (match Lambda.Type.equal t (List t') with
           | Some Lambda.Eq.Refl -> V { v = List.rev v; t; pp; eq; }
           | None -> Fmt.invalid_arg "Cannot unify %a and %a"
                       Lambda.Type.pp t
                       Lambda.Type.pp (List t'))
        | x :: r ->
          let V { v = x; t; pp = _; eq = _; } = go x in
          let V { v = acc; t = tacc; pp; eq; } = acc in
          (match Lambda.Type.equal t t', Lambda.Type.equal (List t) tacc with
           | Some Lambda.Eq.Refl, Some Lambda.Eq.Refl ->
             aux (V { v = x :: acc; t = List t; pp; eq; }) r
           | _, _ -> Fmt.invalid_arg "Cannot unify %a and %a"
                       Lambda.Type.pp t
                       Lambda.Type.pp t) in
      aux (V { v = []; t = List t'; pp; eq; }) value |> cast
    | Types.Pair { a; b; } ->
      let V { v = a; t = ta; pp = ppa; eq = eqa; } = go a in
      let V { v = b; t = tb; pp = ppb; eq = eqb; } = go b in
      V { v = (a, b); t = Pair (ta, tb); pp = Fmt.pair ppa ppb; eq = pair_eq ~eqa ~eqb }
    | Types.Either { value = Types.Left { value; }; typ_l; typ_r; } ->
      let V { v; t; pp = ppl; eq = eql; } = go value in
      let Lambda.Type.V tl = Lambda.Type.typ (to_typ typ_l) in
      let Lambda.Type.V tr = Lambda.Type.typ (to_typ typ_r) in

      let ppr, eqr = Lambda.Type.pp_val tr, Lambda.Type.eq_val tr in

      (match Lambda.Type.equal t tl with
       | Some Lambda.Eq.Refl ->
         V { v = Lambda.Value.L v; t = Either (tl, tr); pp = pp_either ~ppl ~ppr;
             eq = either_eq ~eql ~eqr }
       | None -> Fmt.invalid_arg "Cannot unify %a and %a"
                   Lambda.Type.pp t
                   Lambda.Type.pp tl)
    | Types.Either { value = Types.Right { value; }; typ_l; typ_r; } ->
      let V { v; t; pp = ppr; eq = eqr; } = go value in
      let Lambda.Type.V tl = Lambda.Type.typ (to_typ typ_l) in
      let Lambda.Type.V tr = Lambda.Type.typ (to_typ typ_r) in

      let ppl, eql = Lambda.Type.pp_val tl, Lambda.Type.eq_val tl in

      (match Lambda.Type.equal t tr with
       | Some Lambda.Eq.Refl ->
         V { v = Lambda.Value.R v; t = Either (tl, tr); pp = pp_either ~ppl ~ppr;
             eq = either_eq ~eql ~eqr }
       | None -> Fmt.invalid_arg "Cannot unify %a and %a"
                   Lambda.Type.pp t
                   Lambda.Type.pp tl)
    | Types.Result { value = Types.Ok { value; }; typ_ok; typ_error; } ->
      let V { v; t; pp = pp_ok; eq = eq_ok; } = go value in
      let Lambda.Type.V tok = Lambda.Type.typ (to_typ typ_ok) in
      let Lambda.Type.V terror = Lambda.Type.typ (to_typ typ_error) in

      let pp_error, eq_error = Lambda.Type.pp_val terror, Lambda.Type.eq_val terror in

      (match Lambda.Type.equal t tok with
       | Some Lambda.Eq.Refl ->
         V { v = Pervasives.Ok v; t = Result (tok, terror);
             pp = Fmt.result ~ok:pp_ok ~error:pp_error;
             eq = result_eq ~eq_ok ~eq_error }
       | None -> Fmt.invalid_arg "Cannot unify %a and %a"
                   Lambda.Type.pp t
                   Lambda.Type.pp tok)
    | Types.Result { value = Types.Error { value; }; typ_ok; typ_error; } ->
      let V { v; t; pp = pp_error; eq = eq_error; } = go value in
      let Lambda.Type.V tok = Lambda.Type.typ (to_typ typ_ok) in
      let Lambda.Type.V terror = Lambda.Type.typ (to_typ typ_error) in

      let pp_ok, eq_ok = Lambda.Type.pp_val tok, Lambda.Type.eq_val tok in

      (match Lambda.Type.equal t terror with
       | Some Lambda.Eq.Refl ->
         V { v = Pervasives.Error v; t = Result (tok, terror);
             pp = Fmt.result ~ok:pp_ok ~error:pp_error;
             eq = result_eq ~eq_ok ~eq_error }
       | None -> Fmt.invalid_arg "Cannot unify %a and %a"
                   Lambda.Type.pp t
                   Lambda.Type.pp terror)
    | Types.Return { value; _ } ->
      if unwrap
      (* Client automatically unwrap toplevel lwt values. *)
      then go value
      else
        let V { v; t; _ } = go value in
        let v = Lambda.Expr.return v in
        let t = Lambda.Type.lwt t in
        Lambda.L.uncast (Obj.magic t) v
  in
  go x

module Option = struct let map f = function Some v -> Some (f v) | None -> None end

let to_value : value -> (Types.value, [`Msg of string]) result = fun v ->
  let rec go : Lambda.Value.t -> Types.value = function
    | Lambda.Value.Unit -> Types.Unit
    | Lambda.Value.Int value -> Types.Int { value = Int32.of_int value; }
    | Lambda.Value.Int32 value -> Types.Int32 { value; }
    | Lambda.Value.Int64 value -> Types.Int64 { value; }
    | Lambda.Value.Bool value -> Types.Bool { value; }
    | Lambda.Value.String value -> Types.String { value; }
    | Lambda.Value.List (typ, value) ->
      Types.List { typ = of_typ typ; value = List.map go value; }
    | Lambda.Value.Array (typ, value) ->
      Types.Array { typ = of_typ typ; value = Array.map go value |> Array.to_list; }
    | Lambda.Value.Option (typ, value) ->
      Types.Option { typ = of_typ typ; value = Option.map go value; }
    | Lambda.Value.Bytes value -> Types.Bytes { value; }
    | Lambda.Value.Pair (a, b) -> Types.Pair { a = go a; b = go b; }
    | Lambda.Value.Either (L value, typl, typr) ->
      Types.Either { value = Types.Left { value = go value };
                     typ_l = of_typ typl; typ_r = of_typ typr }
    | Lambda.Value.Either (R value, typl, typr) ->
      Types.Either { value = Types.Right { value = go value };
                     typ_l = of_typ typl; typ_r = of_typ typr }
    | Lambda.Value.Result (Ok value, typ_ok, typ_error) ->
      Types.Result { value = Types.Ok { value = go value };
                     typ_ok = of_typ typ_ok; typ_error = of_typ typ_error }
    | Lambda.Value.Result (Error value, typ_ok, typ_error) ->
      Types.Result { value = Types.Error { value = go value };
                     typ_ok = of_typ typ_ok; typ_error = of_typ typ_error }
    | Lambda.Value.Return (value, typ) ->
      Types.Return { typ = of_typ typ; value = go value; }
  in
  match Lambda.Value.unsafe_value v with
  | Ok v         -> Ok (go v)
  | Error _ as e -> e

let of_expr
  : ?gamma:Type.abstract Gamma.t ->
    ?primitives:primitive Primitives.t ->
    Types.expr -> expr
  = fun ?(gamma = Gamma.empty) ?(primitives = Primitives.empty) ->
    let of_value = of_value ~gamma ~unwrap:false in
    let to_typ = to_typ ~gamma in
    let rec go : Types.expr -> expr = function
      | Types.Val { value = Types.Unit; } -> unit
      | Types.Val { value = Types.Int { value; }; } -> int (Int32.to_int value)
      | Types.Val { value = Types.Int32 { value; }; } -> int32 value
      | Types.Val { value = Types.Int64 { value; }; } -> int64 value
      | Types.Val { value = Types.Bool { value = true; }; } -> true_
      | Types.Val { value = Types.Bool { value = false; }; } -> false_
      | Types.Val { value = Types.String { value; }; } -> string value
      | Types.Val { value = x; } ->
        let V { v; t; pp; eq; } = of_value x in
        value v t pp eq
      | Types.Prm { value = { name
                            ; arguments
                            ; return } } ->
        (match Primitives.find name primitives with
         | primitive ->
           if String.equal primitive.name name
           && List.for_all2 equal_typ (List.map (to_typ ) arguments) primitive.args
           && equal_typ (to_typ return) primitive.ret
           then prim primitive
           else Fmt.invalid_arg "Remote primitive %s mismatch with local primitive" name
         | exception Not_found -> Fmt.invalid_arg "Primitive %s not found" name)
      | Types.Lst { typ; expr; } ->
        list ?typ:(Option.map to_typ typ) (List.map go expr)
      | Types.Arr { typ; expr; } ->
        array ?typ:(Option.map to_typ typ) (Array.of_list (List.map go expr))
      | Types.Opt { typ; expr = None; } ->
        none (to_typ typ)
      | Types.Opt { expr = Some expr; _ } ->
        some (go expr)
      | Types.Var { var = id; } ->
        var (Int32.to_int id)
      | Types.Lam { typ; var; expr; } ->
        lambda [ var, to_typ typ ] (go expr)
      | Types.Rec { ret; name; argument; expr; } ->
        fix (name, to_typ argument) (to_typ ret) (go expr)
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
      | Types.Bin { op = Types.Get; a; b } ->
        get (go a) (go b)
      | Types.Bin { op = Types.Shiftl; a; b; } ->
        (go a) << (go b)
      | Types.Bin { op = Types.Shiftr; a; b; } ->
        (go a) >> (go b)
      | Types.Bin { op = Types.Or; a; b; } ->
        (go a) lor (go b)
      | Types.Bin { op = Types.Xor; a; b; } ->
        (go a) lxor (go b)
      | Types.Bin { op = Types.And; a; b; } ->
        (go a) land (go b)
      | Types.Uno { op = Types.Fst; x; } ->
        fst (go x)
      | Types.Uno { op = Types.Prj; x; } ->
        prj (go x)
      | Types.Uno { op = Types.Snd; x; } ->
        snd (go x)
      | Types.Uno { op = Types.L { value; }; x; } ->
        left (to_typ value) (go x)
      | Types.Uno { op = Types.R { value; }; x; } ->
        right (to_typ value) (go x)
      | Types.Uno { op = Types.Ok { value; }; x; } ->
        ok (to_typ value) (go x)
      | Types.Uno { op = Types.Error { value }; x; } ->
        error (to_typ value) (go x)
      | Types.Uno { op = Types.Not; x; } ->
        lnot (go x)
      | Types.Let { typ; name; expr; body; } ->
        let_var (to_typ typ) name (go expr) (go body)
      | Types.Swt { a; b; s; } ->
        match_ (go s) (go a) (go b)
      | Types.Ret { expr; } ->
        return (go expr)
      | Types.Bnd { expr; func; } ->
        bind (go expr) (go func)
      | Types.If { a; b; s; } ->
        if_ (go s) (go a) (go b)
    in
    go

exception Error of [`Msg of string]

let err e = raise (Error e)

let (>>?) x f = match x with
  | Result.Error e -> err e
  | Result.Ok x    -> f x

let to_expr : expr -> (Types.expr, [`Msg of string]) result = fun e ->
  let rec go = function
    | Val v -> to_value v >>? fun value -> Types.Val { value }
    | Prm { name; args; ret; _ } ->
      Types.Prm { value = { name; arguments = List.map of_typ args;
                            return = of_typ ret; }; }
    | Lst (typ, lst) ->
      Types.Lst { typ = Option.map of_typ typ; expr = List.map go lst; }
    | Arr (typ, arr) ->
      Types.Arr { typ = Option.map of_typ typ;
                  expr = Array.map go arr |> Array.to_list; }
    | Opt (Some typ, opt) ->
      Types.Opt { typ = of_typ typ; expr = Option.map go opt; }
    | Opt (None, _) -> invalid_arg "Optional construction needs to be typed"
    | Ret expr -> Types.Ret { expr = go expr; }
    | Bnd (expr, func) -> Types.Bnd { expr = go expr; func = go func; }
    | Var { id; } -> Types.Var { var = Int32.of_int id; }
    | Lam (typ, var, expr) ->
      Types.Lam { typ = of_typ typ; var; expr = go expr; }
    | Rec { r = ret; p = (name, argument); e = expr; } ->
      Types.Rec { ret = of_typ ret
                ; name
                ; argument = of_typ argument
                ; expr = go expr; }
    | App (a, b) -> Types.App { a = go a; b = go b; }
    | Bin (op, a, b) ->
      Types.Bin { op = to_binop op; a = go a; b = go b; }
    | Uno (op, x) -> Types.Uno { op = to_unop op; x = go x; }
    | Let (typ, name, expr, body) ->
      Types.Let { typ = of_typ typ; name; expr = go expr; body = go body; }
    | Swt { a; b; s; } -> Types.Swt { a = go a; b = go b; s = go s; }
    | If (s, a, b) -> Types.If { a = go a; b = go b; s = go s; }
  in
  try Ok (go e)
  with Error e -> Error e

let of_request
  : ?gamma:Type.abstract Gamma.t ->
    ?primitives:primitive Primitives.t ->
    Types.request -> expr * Type.t * int64
  = fun ?(gamma = Gamma.empty) ?primitives request ->
    of_expr ~gamma ?primitives request.Types.expr,
    to_typ ~gamma request.Types.typ, request.Types.output

let to_request
  : expr * Type.t * int64 -> (Types.request, [`Msg of string]) result
  = fun (expr, typ, output) ->
    match to_expr expr with
    | Error _ as e -> e
    | Ok expr -> Ok { Types.expr; typ = of_typ typ ; output }

let of_reply
  : ?gamma:Type.abstract Gamma.t -> Types.reply -> (value, [ `Msg of string ]) result
  = fun ?(gamma = Gamma.empty) -> function
    | Types.Value value -> Ok (of_value ~gamma value)
    | Types.Error err   -> Error (`Msg err)

let to_reply
  : (value, [ `Msg of string ]) result -> Types.reply
  = function
    | Error (`Msg err) -> Types.Error err
    | Ok value         ->
      match to_value value with
      | Error (`Msg e) -> Types.Error e
      | Ok v           -> Types.Value v
