(*
 * Copyright (c) 2018 Thomas Gazagnaire <thomas@gazagnaire.org>
 * and Romain Calascibetta <romain.calascibetta@gmail.com>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

let src = Logs.Src.create "lambda"
module Log = (val Logs.src_log src : Logs.LOG)

module Type = struct

  type ('l, 'r) either = ('l, 'r) Eq.either =
    | L of 'l
    | R of 'r

  type 'a abstract = {
    name: string;
    wit : 'a Eq.Witness.t;
  }

  type 'a t =
    | Int     : int t
    | Bool    : bool t
    | String  : string t
    | Abstract: 'a abstract -> 'a t
    | Lambda  : 'a t * 'b t -> ('a -> 'b) t
    | Pair    : 'a t * 'b t -> ('a * 'b) t
    | Either  : 'a t * 'b t -> ('a, 'b) either t

  let rec eq: type a b. a t -> b t -> (a, b) Eq.refl option = fun a b ->
    match a, b with
    | Int,            Int            -> Some Eq.Refl
    | Bool,           Bool           -> Some Eq.Refl
    | String,         String         -> Some Eq.Refl
    | Abstract a    , Abstract b     -> Eq.Witness.eq a.wit b.wit
    | Lambda (a, a'), Lambda (b, b') -> Eq.(eq a b >>= fun Refl -> eq a' b')
    | Either (a, a'), Either (b, b') -> Eq.(eq a b >?= fun Refl -> eq a' b')
    | Pair  (a, a') , Pair  (b, b')  -> Eq.(eq a b >&= fun Refl -> eq a' b')
    | _, _ -> None

  let pp_infix = Untyped.pp_infix

  let rec pp : type a. a t Fmt.t = fun ppf -> function
    | Int           -> Fmt.pf ppf "int"
    | Bool          -> Fmt.pf ppf "bool"
    | String        -> Fmt.pf ppf "string"
    | Abstract a    -> Fmt.string ppf a.name
    | Lambda (a, b) -> Fmt.pf ppf "%a" (pp_infix ~infix:"->" pp pp) (a, b)
    | Pair (a, b)   -> Fmt.pf ppf "%a" (pp_infix ~infix:"*" pp pp) (a, b)
    | Either (a, b) -> Fmt.pf ppf "%a" (pp_infix ~infix:"or" pp pp) (a, b)

  let rec pp_val: type a. a t -> a Fmt.t = fun t ppf x ->
    match t with
    | Int           -> Fmt.int ppf x
    | Bool          -> Fmt.bool ppf x
    | String        -> Fmt.string ppf x
    | Abstract _    -> Fmt.pf ppf "<abstract>"
    | Lambda _      -> Fmt.pf ppf "<function>"
    | Pair (a, b)   -> Fmt.Dump.pair (pp_val a) (pp_val b) ppf x
    | Either (a, b) -> match x with
      | L x -> pp_val a ppf x
      | R x -> pp_val b ppf x

  let int = Int
  let bool = Bool
  let string = String
  let lambda a b = Lambda (a, b)
  let pair a b = Pair (a, b)
  let either a b = Either (a, b)

  let abstract name =
    let wit = Eq.Witness.v () in
    Abstract {name; wit}

  let abstract_param a v =
    Untyped.Param.(Abs {name=a.name; v; wit=a.wit})

  let ( @->) = lambda
  let ( ** ) = pair
  let ( || ) = either

  let rec untype: type a. a t -> Untyped.typ = function
    | Int           -> Int
    | Bool          -> Bool
    | String        -> String
    | Abstract a    -> Abstract (A {name=a.name; wit=a.wit})
    | Pair (a, b)   -> Pair (untype a, untype b)
    | Either (a, b) -> Either (untype a, untype b)
    | Lambda (a, b) -> Lambda (untype a, untype b)

  type v = V : 'a t -> v

  let rec typ: Untyped.typ -> v = function
    | Int    -> V Int
    | Bool   -> V Bool
    | String -> V String
    | Abstract a ->
      let Untyped.Type.A a = a in
      let name = a.name in
      let wit = a.wit in
      V (Abstract {name; wit})
    | Lambda (a, b) ->
      let V a = typ a in
      let V b = typ b in
      V (Lambda (a, b))
    | Pair (a, b) ->
      let V a = typ a in
      let V b = typ b in
      V (Pair (a, b))
    | Either (a, b) ->
      let V a = typ a in
      let V b = typ b in
      V (Either (a, b))

end

module Env = struct

  type 'e t =
    | [] : unit t
    | (::) : 'a Type.t * 'e t -> ('e * 'a) t

  let rec eq: type a b. a t -> b t -> (a, b) Eq.refl option = fun a b ->
    match a, b with
    | [], []               -> Some Eq.Refl
    | (at :: a), (bt :: b) -> Eq.(eq a b >*= fun Refl -> Type.eq at bt)
    | _ -> None

  type v = V : 'a t -> v

  let rec typ: Untyped.typ list -> v = function
    | [] -> V []
    | x :: r ->
      let Type.V x = Type.typ x in
      let V r = typ r in
      V (x :: r)

end

module Var = struct

  type ('e, 'a) t =
    | Z : ('e * 'a, 'a) t
    | S : ('e, 'a) t -> ('e * 'b, 'a) t

  (*   let rec int_of_var : type e a. (e, a) t -> int = function
       | Z -> 1
       | S n -> 1 + (int_of_var n) *)

  let o = Z
  let x = ()

  let ( $ ) x () = S x

  (* let v = o$x$x$x$x$x = 5 (church style) *)

  type v = V : ('e, 'a) t * 'e Env.t * 'a Type.t -> v

  let rec typ: Untyped.var -> Untyped.typ list -> v = fun v e ->
    match v.id, e with
    | 0, t :: r ->
      let Type.V t = Type.typ t in
      let Env.V r = Env.typ r in
      V (Z, (t :: r), t)

    | n, t :: r ->
      let V (x', r', tr) = typ {id=n-1} r in
      let Type.V t' = Type.typ t in
      V (S x', (t' :: r'), tr)

    | _, g ->
      Fmt.invalid_arg "Variable %a unbound in %a."
        Untyped.dump_var v Fmt.(Dump.list Untyped.Type.pp) g

end

module Param = struct

  let error v t =
    Fmt.invalid_arg "Unable to cast %a with type %a."
      Untyped.Param.pp v Type.pp t

  let rec cast: type a. Untyped.Param.t -> a Type.t -> a
    = fun v t ->
      match v, t with
      | Int n, Type.Int -> n
      | Bool b, Type.Bool -> b
      | String s, Type.String -> s
      | Abstract a, Type.Abstract ta ->
        let Untyped.Param.Abs a = a in
        (match Eq.Witness.eq a.wit ta.wit with
         | Some Eq.Refl -> a.v
         | _ -> error v t)
      | Pair (a, b), Type.Pair (ta, tb) ->
        let a = cast a ta in
        let b = cast b tb in
        (a, b)
      | Either (tr, L x), Type.Either (tl, tr') ->
        let Type.V tr = Type.typ tr in
        (match Type.eq tr tr' with
         | Some Eq.Refl -> L (cast x tl)
         | _ -> error v t)
      | Either (tl, R x), Type.Either (tl', tr) ->
        let Type.V tl = Type.typ tl in
        (match Type.eq tl tl' with
         | Some Eq.Refl -> R (cast x tr)
         | _ -> error v t)
      | Lambda { typ = (uta, utr); exp; }, Type.Lambda (ta, tr) ->
        let Type.V ta' = Type.typ uta in
        let Type.V tr' = Type.typ utr in
        let exp = fun a ->
          let ua = untype ta a in
          let ur = exp ua in
          let r = cast ur tr in
          match Type.eq ta ta', Type.eq tr tr' with
          | Some Eq.Refl, Some Eq.Refl -> r
          | _ -> error v t
        in exp
      | v, t -> error v t

  and untype:
    type a. a Type.t -> a -> Untyped.Param.t
    = fun t v -> match t, v with
    | Type.Int            , n      -> Int n
    | Type.Bool           , b      -> Bool b
    | Type.String         , s      -> String s
    | Type.Abstract a     , v      -> Abstract (Type.abstract_param a v)
    | Type.Pair (ta, tb)  , (a, b) -> Pair (untype ta a, untype tb b)
    | Type.Either (tl, tr), L l    -> Either (Type.untype tr, L (untype tl l))
    | Type.Either (tl, tr), R r    -> Either (Type.untype tl, R (untype tr r))
    | Type.Lambda (ta, tr), f      ->
      let exp = (fun ua ->
          let a = cast ua ta in
          untype tr (f a)
        ) in
      Lambda { typ = (Type.untype ta, Type.untype tr); exp }

end

module Expr = struct

  type ('l, 'r, 'v) binop =
    | Add : (int, int, int) binop
    | Mul : (int, int, int) binop
    | Sub : (int, int, int) binop
    | Div : (int, int, int) binop
    | Pair : ('a, 'b, 'a * 'b) binop
    | Eq  : ('a, 'a, bool) binop

  type ('u, 'v) unop =
    | Fst : ('a * 'b, 'a) unop
    | Snd : ('a * 'b, 'b) unop
    | L : ('a, ('a, 'b) Type.either) unop
    | R : ('b, ('a, 'b) Type.either) unop

  type ('r, 'e) prim =
    { name : string
    ; exp  : (env:'e -> 'r)
    ; env  : 'e Env.t
    ; typ  : 'r Type.t }

  and ('e, 'a) t =
    | Con : 'a -> ('e, 'a) t
    | Prm : ('r, 'e) prim -> ('e, 'r) t
    | Uno : ('a, 'res) unop * ('e, 'a) t -> ('e, 'res) t
    | Bin : ('a, 'b, 'res) binop * ('e, 'a) t * ('e, 'b) t -> ('e, 'res) t
    | Var : ('e, 'a) Var.t -> ('e, 'a) t
    | Abs : 'a Type.t * ('e * 'a, 'b) t -> ('e, 'a -> 'b) t
    | App : ('e, 'a -> 'b) t * ('e, 'a) t -> ('e, 'b) t
    | Let : 'a Type.t * ('e, 'a) t * ('e * 'a, 'b) t -> ('e, 'b) t
    | Rec : { z : ('e, 'a) t
            ; s : ('e * 'a, ('a, 'r) Type.either) t } -> ('e, 'r) t
    | Swt : { s : ('e, ('l, 'r) Type.either) t
            ; a : ('e * 'l, 'a) t
            ; b : ('e * 'r, 'a) t } -> ('e, 'a) t
    | If  : ('e, bool) t * ('e, 'a) t * ('e, 'a) t -> ('e, 'a) t

  let rec get: type e a. (e, a) Var.t -> e -> a = fun v e ->
    match v, e with
    | Var.Z, (_, x)   -> x
    | Var.S n, (r, _) -> get n r

  let rec eval: type e a. (e, a) t -> e -> a = fun x e ->
    match x with
    | Con i -> i
    | Prm { exp; _ } -> exp ~env:e
    | Uno (L, x) -> L (eval x e)
    | Uno (R, x) -> R (eval x e)
    | Uno (Fst, x) -> fst (eval x e)
    | Uno (Snd, x) -> snd (eval x e)
    | Var x -> get x e
    | Abs (_, r) -> (fun v -> eval r (e, v))
    | App (f, a) -> (eval f e) (eval a e)
    | Bin (Add, l, r) -> (eval l e) + (eval r e)
    | Bin (Sub, l, r) -> (eval l e) - (eval r e)
    | Bin (Mul, l, r) -> (eval l e) * (eval r e)
    | Bin (Div, l, r) -> (eval l e) / (eval r e)
    | Bin (Pair, l, r) -> (eval l e, eval r e)
    | Bin (Eq, l, r) -> (eval l e) = (eval r e)
    | Let (_, a, f) ->
      eval f (e, (eval a e))
    | Swt { s; a; b; } ->
      (match eval s e with
       | L l -> eval a (e, l)
       | R r -> eval b (e, r))
    | Rec { z; s; } ->
      let z' = eval z e in
      let rec loop a = match eval s (e, a) with
        | L a -> loop a
        | R r -> r
      in
      loop z'
    | If (t, a, b) ->
      (match eval t e with
       | true -> eval a e
       | false -> eval b e)

  type e = E: (unit, 'a) t * 'a Type.t -> e
  type v = V: 'a * 'a Type.t -> v
  type a_expr = Expr: ('e, 'a) t * 'e Env.t * 'a Type.t -> a_expr
  type a_prim = Prim: ('a, 'b) prim * 'b Env.t * 'a Type.t -> a_prim

  type kind =
    | EnvMismatch of { g  : Env.v
                     ; g' : Env.v }
    | TypMismatch of { a  : Type.v
                     ; b  : Type.v }
    | ExpectedPair of a_expr
    | ExpectedLambda of a_expr
    | ExpectedEither of a_expr
    | UnexpectedAnonymousLambda

  type error = Untyped.expr * Untyped.typ list * kind list

  exception Break of error

  let error expr ts es = raise (Break (expr, ts, es))

  let pp_typ ppf (Type.V t) = Type.pp ppf t

  let pp_kind ppf = function
    | EnvMismatch _      -> Fmt.pf ppf "env mismatch"
    | TypMismatch {a; b} ->
      Fmt.pf ppf "%a is not compatible with %a" pp_typ a pp_typ b
    | ExpectedPair _     -> Fmt.pf ppf "a pair was expected"
    | ExpectedLambda _   -> Fmt.pf ppf "a lambda was expected"
    | ExpectedEither _   -> Fmt.pf ppf "either was expected"
    | UnexpectedAnonymousLambda -> Fmt.pf ppf "unexpected anonymous lambda"

  let pp_error ppf (e, ts, ks) =
    Fmt.pf ppf "error while evaluating:@ %a@ %a@ %a"
      Untyped.pp e
      Fmt.(Dump.list Untyped.Type.pp) ts
      Fmt.(Dump.list pp_kind) ks

  let uncurry args =
    List.fold_right (fun t -> function
        | Expr (a, Env.(ta :: e), tr) ->
          let Type.V t' = Type.typ t in
          (match Type.eq ta t' with
           | Some Eq.Refl ->
             Expr (Abs (t', a), e, Lambda (t', tr))
           | None ->
             Fmt.invalid_arg "Type mismatch: %a <> %a." Type.pp ta Type.pp t')
        | Expr _ -> invalid_arg "Impossible to uncurry, environment mismatch")
      args

  let env_to_list g e =
    let rec go:
      type e. Untyped.param list -> e Env.t -> e -> Untyped.param list
      = fun a g e -> match g, e with
        | Env.(t :: tr), (vr, v) ->
          let c = Param.untype t v in
          go (c :: a) tr vr
        | Env.([]), () -> a
    in
    go [] g e

  let cast_primitive ~name g args rettype untype_primitive =
    let rec chop n l = match n, l with
      | 0, l -> l
      | n, _ :: r -> chop (n - 1) r
      | _, _ -> assert false in
    let miss = List.length g in
    let Env.V g' = Env.typ List.(rev_append args g) in
    Prim ({ name
          ; env = g'
          ; typ = rettype
          ; exp = (fun ~env ->
                let ua = env_to_list g' env in
                let uc = untype_primitive (chop miss ua) in
                Param.cast uc rettype
              ) },
          g', rettype)

  let typ e =
    let rec aux: Untyped.expr -> Untyped.typ list -> a_expr = fun e g ->
      match e with
      | Con (Int i) ->
        let Env.V g' = Env.typ g in
        Expr (Con i, g', Int)
      | Con (Bool b) ->
        let Env.V g' = Env.typ g in
        Expr (Con b, g', Bool)
      | Con (String s) ->
        let Env.V g' = Env.typ g in
        Expr (Con s, g', String)
      | Con (Abstract a) ->
        let Env.V g' = Env.typ g in
        let Untyped.Param.Abs {name; v; wit} = a in
        let a = { Type.name; wit } in
        Expr (Con v, g', Abstract a)
      | Con (Either (tr, L x)) ->
        let Env.V g' = Env.typ g in
        let Type.V tr' = Type.typ tr in
        let Expr (x', g'', tx') = aux (Con x) g in
        (match x', Env.eq g' g'' with
         | Con x', Some Eq.Refl ->
           Expr (Con Type.(L x'), g', Either (tx', tr'))
         | _ ->
           Log.err (fun l -> l "Either L");
           error e g [ EnvMismatch { g = Env.V g'; g' = Env.V g'' } ])
      | Con (Either (tl, R x)) ->
        let Env.V g' = Env.typ g in
        let Type.V tl' = Type.typ tl in
        let Expr (x', g'', tx') = aux (Con x) g in
        (match x', Env.eq g' g'' with
         | Con x', Some Eq.Refl ->
           Expr (Con Type.(R x'), g', Either (tl', tx'))
         | _ ->
           Log.err (fun l -> l "Either R");
           error e g [ EnvMismatch { g = Env.V g'; g' = Env.V g'' } ])
      | Con (Lambda _) ->
        Log.err (fun l -> l "Lambda");
        error e g [ UnexpectedAnonymousLambda ]
      | Prm { name; typ = (args, ret); exp; } ->
        let Type.V rettype = Type.typ ret in
        let Prim (x, y, z) = cast_primitive ~name g args rettype exp in
        uncurry args (Expr (Prm x, y, z))
      | Uno (L tr, x) ->
        let Env.V g' = Env.typ g in
        let Type.V tr' = Type.typ tr in
        let Expr (x', g'', tx') = aux x g in
        (match Env.eq g' g'' with
         | Some Eq.Refl ->
           Expr (Uno (L, x'), g', Either (tx', tr'))
         | _ ->
           Log.err (fun l -> l "Uno L");
           error e g [ EnvMismatch { g = Env.V g'; g' = Env.V g'' } ])
      | Uno (R tl, x) ->
        let Env.V g' = Env.typ g in
        let Type.V tl' = Type.typ tl in
        let Expr (x', g'', tx') = aux x g in
        (match Env.eq g' g'' with
         | Some Eq.Refl ->
           Expr (Uno (R, x'), g', Either (tl', tx'))
         | _ ->
           Log.err (fun l -> l "Uno R");
           error e g [ EnvMismatch { g = Env.V g'; g' = Env.V g'' } ])
      | Uno (Fst, x) ->
        let Env.V g' = Env.typ g in
        (match aux x g with
         | Expr (x', g'', Type.Pair (ta', _)) ->
           (match Env.eq g' g'' with
            | Some Eq.Refl ->
              Expr (Uno (Fst, x'), g', ta')
            | _ ->
              Log.err (fun l -> l "Uno Fst");
              error e g [ EnvMismatch { g = Env.V g'; g' = Env.V g'' } ])
         | wexpr ->
           Log.err (fun l -> l "Uno Fst");
           error e g [ ExpectedPair wexpr ])
      | Uno (Snd, x) ->
        let Env.V g' = Env.typ g in
        (match aux x g with
         | Expr (x', g'', Type.Pair (_, tb')) ->
           (match Env.eq g' g'' with
            | Some Eq.Refl ->
              Expr (Uno (Snd, x'), g', tb')
            | _ ->
              Log.err (fun l -> l "Uno Snd");
              error e g [ EnvMismatch { g = Env.V g'; g' = Env.V g'' } ])
         | wexpr ->
           Log.err (fun l -> l "Uno Snd");
           error e g [ ExpectedPair wexpr ])
      | Con (Pair (a, b)) ->
        let Env.V g' = Env.typ g in
        let Expr (a', g'', ta') = aux (Con a) g in
        let Expr (b', g''', tb') = aux (Con b) g in
        (match a', b', Env.eq g' g'', Env.eq g'' g''' with
         | Con a', Con b', Some (Eq.Refl as g1), Some (Eq.Refl as g2) ->
           (match Eq.trans g1 g2 with
            | Eq.Refl -> Expr (Con (a', b'), g', Pair (ta', tb')))
         | _ ->
           Log.err (fun l -> l "Con Pair");
           error e g [ EnvMismatch { g = Env.V g'; g' = Env.V g'' }
                     ; EnvMismatch { g = Env.V g''; g' = Env.V g''' } ])
      | Bin (`Pair, a, b) ->
        let Env.V g' = Env.typ g in
        let Expr (a', g'', ta') = aux a g in
        let Expr (b', g''', tb') = aux b g in
        (match Env.eq g' g'', Env.eq g'' g''' with
         | Some (Eq.Refl as g1), Some (Eq.Refl as g2) ->
           (match Eq.trans g1 g2 with
            | Eq.Refl -> Expr (Bin (Pair, a', b'), g', Pair (ta', tb')))
         | _, _ ->
           Log.err (fun l -> l "Bin `Pair");
           error e g [ EnvMismatch { g = Env.V g'; g' = Env.V g'' }
                     ; EnvMismatch { g = Env.V g''; g' = Env.V g''' } ])
      | Var x ->
        let Var.V (x', g', t') = Var.typ x g in
        Expr (Var x', g', t')
      | Abs (t, _, e) ->
        let Type.V t' = Type.typ t in
        (match aux e (t :: g) with
         | Expr (e', Env.(t'' :: g'), tr) ->
           (match Type.eq t' t'' with
            | Some Eq.Refl -> Expr (Abs (t', e'), g', Type.Lambda (t', tr))
            | None ->
              Log.err (fun l -> l "Abs");
              error e g [ TypMismatch { a = Type.V t'; b = Type.V t'' } ])
         | Expr (_, g'', _) ->
           Log.err (fun l -> l "Abs");
           error e g [ EnvMismatch { g = Env.V (t' :: g''); g' = Env.V g'' } ])
      | App (fu, a) ->
        (match aux fu g, aux a g with
         | Expr (f', g', (Type.Lambda (t', u'))),
           Expr (a', g'', t'') ->
           (match Env.eq g' g'', Type.eq t' t'' with
            | Some Eq.Refl, Some Eq.Refl ->
              Expr (App (f', a'), g', u')
            | _, _ ->
              Log.err (fun l -> l "App");
              error e g [ EnvMismatch { g = Env.V g'; g' = Env.V g'' }
                        ; TypMismatch { a = Type.V t'; b = Type.V t'' } ])
         | wexp, _ ->
           Log.err (fun l -> l "App");
           error e g [ ExpectedLambda wexp ])
      | Bin (#Untyped.arithmetic as binop, l, r) ->
        let binop = match binop with
          | `Add -> Add
          | `Sub -> Sub
          | `Mul -> Mul
          | `Div -> Div
        in
        (match aux l g, aux r g with
         | Expr (l', g', Type.Int),
           Expr (r', g'', Type.Int) ->
           (match Env.eq g' g'' with
            | Some Eq.Refl -> Expr (Bin (binop, l', r'), g', Int)
            | None ->
              Log.err (fun l -> l "Bin");
              error e g [ EnvMismatch { g = Env.V g'; g' = Env.V g'' } ])
         | Expr (_, _, ta), Expr (_, _, tb) ->
           Log.err (fun l -> l "Bin");
           error e g [ TypMismatch { a = Type.V Int; b = Type.V ta }
                     ; TypMismatch { a = Type.V Int; b = Type.V tb } ])
      | Let (t, a, b) ->
        let Env.V g' = Env.typ g in
        (match Type.typ t, aux a g, aux b g with
         | Type.V t', Expr (a', g'', t''), Expr (f', g''', r') ->
           (match Type.eq t' t'' , Env.eq g' g'', Env.eq (t'' :: g'') g''' with
            | Some Eq.Refl, Some Eq.Refl, Some Eq.Refl ->
              Expr (Let (t', a', f'), g', r')
            | _, _, _ ->
              Log.err (fun l -> l "Let");
              error e g [ TypMismatch { a = Type.V t'; b = Type.V t'' }
                        ; EnvMismatch { g = Env.V g'; g' = Env.V g'' }
                        ; EnvMismatch { g = Env.V (t'' :: g''); g' = Env.V g''' } ]))
      | If (t, a, b) ->
        (match aux t g, aux a g, aux b g with
         | Expr (t', g', Type.Bool),
           Expr (a', g'', u'),
           Expr (b', g''', v') ->
           (match Type.eq u' v', Env.eq g' g'', Env.eq g'' g''' with
            | Some Eq.Refl, Some (Eq.Refl as ae), Some (Eq.Refl as be) ->
              (match Eq.trans ae be with Eq.Refl -> Expr (If (t', a', b'), g', u'))
            | _, _, _ ->
              Log.err (fun l -> l "If");
              error e g [ TypMismatch { a = Type.V u'; b = Type.V v' }
                        ; EnvMismatch { g = Env.V g'; g' = Env.V g'' }
                        ; EnvMismatch { g = Env.V g''; g' = Env.V g''' } ])
         | Expr (_, _, t'), _, _ ->
           Log.err (fun l -> l "If");
           error e g [ TypMismatch { a = Type.V Bool; b = Type.V t' } ])
      | Bin (`Eq, a, b) ->
        let Expr (a', g', ta') = aux a g in
        let Expr (b', g'', tb') = aux b g in
        (match Type.eq ta' tb', Env.eq g' g'' with
         | Some Eq.Refl, Some Eq.Refl -> Expr (Bin (Eq, a', b'), g', Bool)
         | _, _ -> assert false)
      | Swt { a; b; s; } ->
        (match aux s g with
         | Expr (s', g'', Type.Either (l', r')) ->
           let l = Type.untype l' in
           let r = Type.untype r' in
           (match aux a (l :: g), aux b (r :: g) with
            | Expr (a', Env.(l'' :: ag''), ar),
              Expr (b', Env.(r'' :: bg''), br) ->
              (match Type.eq l' l'', Type.eq r' r'',
                     Env.eq ag'' g'', Env.eq bg'' g'',
                     Type.eq ar br with
              | Some Eq.Refl, Some Eq.Refl,
                Some Eq.Refl, Some Eq.Refl,
                Some Eq.Refl ->
                Expr (Swt { s = s'; a = a'; b = b'; }, g'', ar)
              | _, _, _, _, _ ->
                Log.err (fun l -> l "Swt");
                error e g [ TypMismatch { a = Type.V l'; b = Type.V l'' }
                          ; TypMismatch { a = Type.V r'; b = Type.V r'' }
                          ; EnvMismatch { g = Env.V ag''; g' = Env.V g'' }
                          ; EnvMismatch { g = Env.V bg''; g' = Env.V g'' }
                          ; TypMismatch { a = Type.V ar; b = Type.V br } ])
            | Expr (_, ag'', _),
              Expr (_, bg'', _) ->
              Log.err (fun l -> l "Swt");
              error e g [ EnvMismatch { g = Env.V ag''; g' = Env.V (l' :: ag'') }
                        ; EnvMismatch { g = Env.V bg''; g' = Env.V (r' :: bg'') } ])
         | wexpr ->
           Log.err (fun l -> l "Swt");
           error e g [ ExpectedEither wexpr ])
      | Rec (u, z, s) ->
        let Env.V g' = Env.typ g in
        let Type.V u' = Type.typ u in
        let gz = g in
        let gs = (u :: g) in
        let Env.V gz' = Env.typ gz in
        let Env.V gs' = Env.typ gs in
        let Expr (z', gz'', tz') = aux z gz in
        let Expr (s', gs'', ts') = aux s gs in
        (match Env.eq g' gz',
               Env.eq (tz' :: g') gs' with
        | Some (Eq.Refl as f1), Some (Eq.Refl as e1)->
          (match Type.eq tz' u',
                 Type.eq ts' (Either (tz', tz')),
                 Env.eq gz' gz'',
                 Env.eq gs' gs'' with
          | Some Eq.Refl,
            Some Eq.Refl,
            Some (Eq.Refl as f2),
            Some (Eq.Refl as e2) ->
            (match Eq.trans f1 f2, Eq.trans e1 e2 with
             | Eq.Refl, Eq.Refl -> Expr (Rec { z = z'; s = s'; }, g', u'))
          | _, _, _, _ ->
            Log.err (fun l -> l "Rec");
            error e g [ TypMismatch { a = Type.V tz'; b = Type.V u' }
                      ; TypMismatch { a = Type.V ts';
                                      b = Type.V (Either (tz', tz')) }
                      ; EnvMismatch { g = Env.V gz'; g' = Env.V gz'' }
                      ; EnvMismatch { g = Env.V gs'; g' = Env.V gs'' } ])
        | _, _ ->
          Log.err (fun l -> l "Rec");
          error e g [ EnvMismatch { g = Env.V g'; g' = Env.V gz' }
                    ; EnvMismatch { g = Env.V (tz' :: g'); g' = Env.V gs' } ])
    in
    match aux e [] with
    | Expr (m, Env.[], t) -> Ok (E (m ,t))
    | Expr (_, g', _) -> Error (e, [], [EnvMismatch {g=Env.V []; g'=Env.V g'}])
    | exception Break e -> Error e

  (* combinators *)

  let int (x:int) = Con x
  let bool (x:bool) = Con x
  let string (x:string) = Con x

  let lambda t e = Abs (t, e)
  let apply f a = App (f, a)

  let fix ~init:z s = Rec { z; s; }
  let var x = Var x
  let if_ b t e = If (b, t, e)

  let ( = ) a b = Bin (Eq, a, b)
  let ( + ) a b = Bin (Add, a, b)
  let ( * ) a b = Bin (Mul, a, b)
  let ( - ) a b = Bin (Sub, a, b)

  let left e = Uno (L, e)
  let right e = Uno (R, e)

  let fst e = Uno (Fst, e)
  let snd e = Uno (Snd, e)
  let pair a b = Bin (Pair, a, b)

end

(*
let env_from_args args =
  List.fold_left (fun (Env.V e) t ->
      let Type.V t' = typ t in
      Env.V (t' :: e)
    ) (Env.V []) args
*)
