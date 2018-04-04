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

  let pp_infix ~infix pp_a pp_b ppf (a, b) =
    Fmt.pf ppf "@[<1>(@[%a@]@ %s @[%a@])@]" pp_a a infix pp_b b

  include T

  let rec pp: type a. a t Fmt.t = fun ppf -> function
    | Unit            -> Fmt.pf ppf "unit"
    | Int             -> Fmt.pf ppf "int"
    | Bool            -> Fmt.pf ppf "bool"
    | String          -> Fmt.pf ppf "string"
    | Lwt             -> Fmt.pf ppf "Lwt.t"
    | Apply (a, b)    -> Fmt.pf ppf "@[%a %a@]" pp a pp b
    | Abstract (a, b) -> Fmt.pf ppf "@[%a%s@]" pp_params a b.name
    | Arrow (a, b)    -> Fmt.pf ppf "%a" (pp_infix ~infix:"->" pp pp) (a, b)
    | Pair (a, b)     -> Fmt.pf ppf "%a" (pp_infix ~infix:"*" pp pp) (a, b)
    | Either (a, b)   -> Fmt.pf ppf "%a" (pp_infix ~infix:"or" pp pp) (a, b)

  and pp_params: type a. a params Fmt.t = fun ppf p ->
    let rec aux: type a. string list -> a params -> string list =
      fun acc -> function
        | Empty       -> List.rev acc
        | Cons (h, t) -> aux (Fmt.to_to_string pp h :: acc) t
    in
    match aux [] p with
    | []  -> ()
    | [h] -> Fmt.pf ppf "%s " h
    | l   -> Fmt.pf ppf "%a " Fmt.(list ~sep:(unit ", ") string) l

  let int = Int
  let bool = Bool
  let string = String
  let arrow a b = Arrow (a, b)
  let pair a b = Pair (a, b)
  let either a b = Either (a, b)
  let apply a b = Apply (a, b)
  let unit = Unit
  let lwt x = apply x Lwt

  let abstract name =
    let wit = Eq.Witness.v () in
    Abstract (Empty, {name; wit})

  module type S1 = sig type 'a t end
  module type S2 = sig type ('a, 'b) t end

  module Abstract1 (S: S1) = struct

    let v: string -> 'a t -> 'a S.t t = fun name ->
      let wit = Eq.Witness.v () in
      fun p -> Abstract (Cons (p, Empty), {name; wit})

  end

  module Abstract2 (S: S2) = struct

    let v: string -> 'a t -> 'b t -> ('a, 'b) S.t t = fun name ->
      let wit = Eq.Witness.v () in
      fun a b -> Abstract (Cons (a, Cons (b, Empty)), {name; wit})

  end

  let ( @->) = arrow
  let ( ** ) = pair
  let ( || ) = either

  let rec untype: type a. a t -> Parsetree.typ = fun x ->
    let module P = Parsetree.Type in
    match x with
    | Unit            -> P.unit
    | Int             -> P.int
    | Bool            -> P.bool
    | String          -> P.string
    | Lwt             -> P.lwt
    | Apply (a, b)    -> P.apply (untype a) (untype b)
    | Abstract (p, a) -> P.abstract a (untype_params p)
    | Pair (a, b)     -> P.(untype a ** untype b)
    | Either (a, b)   -> P.(untype a || untype b)
    | Arrow (a, b)    -> P.(untype a @-> untype b)

  and untype_params: type a. a params -> Parsetree.typ list = function
    | Empty       -> []
    | Cons (h, t) -> untype h :: untype_params t

  type v = V : 'a t -> v
  type a_params = P: 'a params -> a_params

  let rec typ: Parsetree.typ -> v = fun x ->
    let module P = Parsetree.Type in
    match x with
    | Unit   -> V Unit
    | Int    -> V Int
    | Bool   -> V Bool
    | String -> V String
    | Lwt    -> V Lwt
    | Apply (a, b) ->
      let V b = typ b in
      let V a = typ a in
      V (Apply (a, b))
    | Abstract (p, P.A a) ->
      let P p = typ_params p in
      V (Abstract (p, a))
    | Arrow (a, b) ->
      let V a = typ a in
      let V b = typ b in
      V (Arrow (a, b))
    | Pair (a, b) ->
      let V a = typ a in
      let V b = typ b in
      V (Pair (a, b))
    | Either (a, b) ->
      let V a = typ a in
      let V b = typ b in
      V (Either (a, b))

  and typ_params: Parsetree.typ list -> a_params = function
    | []   -> P Empty
    | h::t ->
      let V h = typ h in
      let P t = typ_params t in
      P (Cons (h, t))

  let rec pp_val: type a. a t -> a Fmt.t = fun t ppf x ->
    match t with
    | Unit          -> Fmt.string ppf "()"
    | Int           -> Fmt.int ppf x
    | Bool          -> Fmt.bool ppf x
    | String        -> Fmt.string ppf x
    | Lwt           -> Fmt.string ppf "<promise>"
    | Apply _       -> Fmt.string ppf "TODO: pp apply"
    | Abstract _    -> Fmt.pf ppf "<abstract>"
    | Arrow _       -> Fmt.pf ppf "<function>"
    | Pair (a, b)   -> Fmt.Dump.pair (pp_val a) (pp_val b) ppf x
    | Either (a, b) -> match x with
      | L x -> pp_val a ppf x
      | R x -> pp_val b ppf x
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

  let rec typ: Parsetree.typ list -> v = function
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

  let rec typ: Parsetree.var -> Parsetree.typ list -> v = fun v e ->
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
        Parsetree.dump_var v Fmt.(Dump.list Parsetree.Type.pp) g

end

module Value = struct

  let error v t =
    Fmt.invalid_arg "Unable to cast %a with type %a."
      Parsetree.pp_value v Type.pp t

  let cast: type a. Parsetree.value -> a Type.t -> a = fun x t ->
    let Parsetree.V v = x in
    match Type.eq v.t t with
    | Some Eq.Refl -> v.v
    | None -> error x t

  let untype: type a. a Type.t -> a -> Parsetree.value = fun t v ->
    let pp = Type.pp_val t in
    Parsetree.V {v; t; pp}

  let untype_lwt:
    type a. a Lwt.t Type.t -> (a, Type.lwt) Type.app -> Parsetree.value =
    fun t v ->
      let pp = Type.pp_val t in
      Parsetree.V {v = Higher.Lwt.prj v; t; pp}

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
    | Lam : 'a Type.t * ('e * 'a, 'b) t -> ('e, 'a -> 'b) t
    | Rec : 'p Type.t * 'r Type.t *
            ('e * 'p, ('p, 'r) Type.either) t ->
            ('e, 'p -> 'r) t
    | App : ('e, 'a -> 'b) t * ('e, 'a) t -> ('e, 'b) t
    | Let : 'a Type.t * ('e, 'a) t * ('e * 'a, 'b) t -> ('e, 'b) t
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
    | Lam (_, r) -> (fun v -> eval r (e, v))
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
    | Rec (_, _, f) ->
      let rec loop a = match eval f (e, a) with
        | L a -> loop a
        | R r -> r
      in
      eval (Var Var.o) (e, loop)
    | If (t, a, b) ->
      (match eval t e with
       | true -> eval a e
       | false -> eval b e)

  (* combinators *)

  let unit = Con ()
  let int (x:int) = Con x
  let bool (x:bool) = Con x
  let string (x:string) = Con x

  let lambda t e = Lam (t, e)
  let apply f a = App (f, a)

  let fix t r s = Rec (t, r, s)
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

type 'a typ = 'a Type.t
type expr = E: (unit, 'a) Expr.t * 'a Type.t -> expr
type v = V: 'a * 'a Type.t -> v

let (--) = (-)
open Expr

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

type error = Parsetree.expr * Parsetree.typ list * kind list

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

let pp_error ppf (e, ts, ks) =
  Fmt.pf ppf "error while evaluating:@ %a@ %a@ %a"
    Parsetree.pp e
    Fmt.(Dump.list Parsetree.Type.pp) ts
    Fmt.(Dump.list pp_kind) ks

let uncurry args =
  List.fold_right (fun t -> function
      | Expr (a, Env.(ta :: e), tr) ->
        let Type.V t' = Type.typ t in
        (match Type.eq ta t' with
         | Some Eq.Refl ->
           Expr (Lam (t', a), e, Arrow (t', tr))
         | None ->
           Fmt.invalid_arg "Type mismatch: %a <> %a." Type.pp ta Type.pp t')
      | Expr _ -> invalid_arg "Impossible to uncurry, environment mismatch")
    args

let env_to_list g e =
  let rec aux:
    type e. Parsetree.value list -> e Env.t -> e -> Parsetree.value list
    = fun a g e -> match g, e with
      | Env.(t :: tr), (vr, v) ->
        let c = Value.untype t v in
        aux (c :: a) tr vr
      | Env.([]), () -> a
  in
  aux [] g e

let cast_primitive ~name g args rettype untype_primitive =
  let rec chop n l = match n, l with
    | 0, l -> l
    | n, _ :: r -> chop (n -- 1) r
    | _, _ -> assert false in
  let miss = List.length g in
  let Env.V g' = Env.typ List.(rev_append args g) in
  Prim ({ name
        ; env = g'
        ; typ = rettype
        ; exp = (fun ~env ->
              let ua = env_to_list g' env in
              let uc = untype_primitive (chop miss ua) in
              Value.cast uc rettype
            ) },
        g', rettype)

let typ e =
  let rec aux: Parsetree.expr -> Parsetree.typ list -> a_expr = fun e g ->
    match e with
    | Val v ->
      let Env.V e = Env.typ g in
      let Parsetree.V v = v in
      Expr (Con v.v, e, v.t)
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
    | Lam (t, _, e) ->
      let Type.V t' = Type.typ t in
      (match aux e (t :: g) with
       | Expr (e', Env.(t'' :: g'), tr) ->
         (match Type.eq t' t'' with
          | Some Eq.Refl -> Expr (Lam (t', e'), g', Type.Arrow (t', tr))
          | None ->
            Log.err (fun l -> l "Abs");
            error e g [ TypMismatch { a = Type.V t'; b = Type.V t'' } ])
       | Expr (_, g'', _) ->
         Log.err (fun l -> l "Abs");
         error e g [ EnvMismatch { g = Env.V (t' :: g''); g' = Env.V g'' } ])
    | App (fu, a) ->
      (match aux fu g, aux a g with
       | Expr (f', g', (Type.Arrow (t', u'))),
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
    | Bin (#Parsetree.arithmetic as binop, l, r) ->
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
    | Let (t, _, a, b) ->
      let Env.V g' = Env.typ g in
      let Type.V t' = Type.typ t in
      (match aux a g, aux b (t :: g) with
       | Expr (a', g'', t''), Expr (f', g''', r') ->
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
    | Rec {r=rtyp; p=(_, ptyp); e }  ->
      let Env.V g0 = Env.typ g in
      let Type.V r = Type.typ rtyp in
      let Type.V p = Type.typ ptyp in
      let te = Type.Either (p, r) in
      let tb = Type.Arrow (p, r) in
      (match aux e (ptyp :: g) with
       | Expr (e', Env.(te' :: g'), tr) ->
         (match Type.eq tr te, Type.eq te' p, Env.eq g0 g' with
          | Some Eq.Refl, Some Eq.Refl, Some Eq.Refl ->
           Expr (Rec (p, r, e'), g0, tb)
         | _ -> assert false)
       | _ -> assert false)
         (*
      (match t' with
       | Type.Either (b ,c) ->
         (match Type.eq u b  with
         | Some Eq.Refl ->
           Expr (Rec { z = var Var.o; s = assert false; }, g', u)
         | _ -> assert false)
        | _ -> assert false *)
           (*
         | (Type.Arrow (u, Either (u, u))) with
       | Some Eq.Refl -> Expr (Rec { z = var Var.o; s = s'; }, gs'', u')
       | _ -> assert false
          Log.err (fun l -> l "Rec");
          error e g [ TypMismatch { a = Type.V tz'; b = Type.V u' }
                    ; TypMismatch { a = Type.V ts';
                                    b = Type.V (Either (tz', tz')) }
                    ; EnvMismatch { g = Env.V gz'; g' = Env.V gz'' }
                    ; EnvMismatch { g = Env.V gs'; g' = Env.V gs'' } ])
            | Rec _ -> assert false *)
  in
  match aux e [] with
  | Expr (m, Env.[], t) -> Ok (E (m ,t))
  | Expr (_, g', _) -> Error (e, [], [EnvMismatch {g=Env.V []; g'=Env.V g'}])
  | exception Break e -> Error e

let err_type_mismatch m t t': (_, error) result =
  Error (m, [], [ TypMismatch { a = Type.V t; b = Type.V t' } ])
