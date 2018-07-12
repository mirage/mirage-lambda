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

let fst_ = fst
let snd_ = snd
let (--) = (-)

module Type = struct

  let pp_infix ~infix pp_a pp_b ppf (a, b) =
    Fmt.pf ppf "@[<1>(@[%a@]@ %s @[%a@])@]" pp_a a infix pp_b b

  include T

  let rec pp: type a. a t Fmt.t = fun ppf -> function
    | Unit          -> Fmt.pf ppf "unit"
    | Int           -> Fmt.pf ppf "int"
    | Int32         -> Fmt.pf ppf "int32"
    | Int64         -> Fmt.pf ppf "int64"
    | Bool          -> Fmt.pf ppf "bool"
    | String        -> Fmt.pf ppf "string"
    | Bytes         -> Fmt.pf ppf "bytes"
    | Lwt           -> Fmt.pf ppf "Lwt.t"
    | List a        -> Fmt.pf ppf "%a list" pp a
    | Option a      -> Fmt.pf ppf "%a option" pp a
    | Array a       -> Fmt.pf ppf "%a array" pp a
    | Abstract b    -> Fmt.pf ppf "@[%s@]" b.name
    | Apply (a, b)  -> Fmt.pf ppf "@[%a %a@]" pp a pp b
    | Arrow (a, b)  -> Fmt.pf ppf "%a" (pp_infix ~infix:"->" pp pp) (a, b)
    | Pair (a, b)   -> Fmt.pf ppf "%a" (pp_infix ~infix:"*" pp pp) (a, b)
    | Either (a, b) -> Fmt.pf ppf "%a" (pp_infix ~infix:"or" pp pp) (a, b)
    | Result (a, b) -> Fmt.pf ppf "(%a, %a) result" pp a pp b

  let int = Int
  let int32 = Int32
  let int64 = Int64
  let bool = Bool
  let string = String
  let bytes = Bytes
  let list a = List a
  let option a = Option a
  let array a = Array a
  let arrow a b = Arrow (a, b)
  let pair a b = Pair (a, b)
  let either a b = Either (a, b)
  let apply a b = Apply (a, b)
  let unit = Unit
  let lwt x = apply x Lwt
  let result a b = Result (a, b)

  let abstract name =
    let wit = Eq.Witness.v () in
    Abstract {name; wit}

  let abstract_injection = function
    | Abstract { name; wit; } -> Parsetree.Type.A { Eq.name; wit; }
    | _ -> Fmt.invalid_arg "Type.abstract_projection: expected abstract type"

  let ( @->) = arrow
  let ( ** ) = pair
  let ( || ) = either

  let rec untype: type a. a t -> Parsetree.typ = fun x ->
    let module P = Parsetree.Type in
    match x with
    | Unit          -> P.unit
    | Int           -> P.int
    | Int32         -> P.int32
    | Int64         -> P.int64
    | Bool          -> P.bool
    | String        -> P.string
    | Bytes         -> P.bytes
    | Lwt           -> P.lwt
    | List a        -> P.list (untype a)
    | Option a      -> P.option (untype a)
    | Array a       -> P.array (untype a)
    | Abstract a    -> P.abstract a
    | Apply (a, b)  -> P.apply (untype a) (untype b)
    | Pair (a, b)   -> P.(untype a ** untype b)
    | Either (a, b) -> P.(untype a || untype b)
    | Arrow (a, b)  -> P.(untype a @-> untype b)
    | Result (a, b) -> P.result (untype a) (untype b)

  type v = V : 'a t -> v

  let rec typ: Parsetree.typ -> v = fun x ->
    let module P = Parsetree.Type in
    match x with
    | Unit   -> V Unit
    | Int    -> V Int
    | Int32  -> V Int32
    | Int64  -> V Int64
    | Bool   -> V Bool
    | String -> V String
    | Bytes  -> V Bytes
    | Lwt    -> V Lwt
    | List a ->
      let V a = typ a in
      V (List a)
    | Option a ->
      let V a = typ a in
      V (Option a)
    | Array a ->
      let V a = typ a in
      V (Array a)
    | Abstract (P.A a) -> V (Abstract a)
    | Apply (a, b) ->
      let V b = typ b in
      let V a = typ a in
      V (Apply (a, b))
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
    | Result (a, b) ->
      let V a = typ a in
      let V b = typ b in
      V (Result (a, b))

  let pp_bytes ppf x = Fmt.string ppf (Bytes.unsafe_to_string x)

  let rec pp_val: type a. a t -> a Fmt.t = fun t ppf x ->
    match t with
    | Unit           -> Fmt.string ppf "()"
    | Int            -> Fmt.int ppf x
    | Int32          -> Fmt.int32 ppf x
    | Int64          -> Fmt.int64 ppf x
    | Bool           -> Fmt.bool ppf x
    | String         -> Fmt.string ppf x
    | Bytes          -> pp_bytes ppf x
    | Lwt            -> Fmt.string ppf "<promise>"
    | List a         -> Fmt.Dump.list (pp_val a) ppf x
    | Option a       -> Fmt.Dump.option (pp_val a) ppf x
    | Array a        -> Fmt.Dump.array (pp_val a) ppf x
    | Abstract _     -> Fmt.pf ppf "<abstract>"
    | Apply (t, Lwt) -> Fmt.pf ppf "#promise(%a)" pp t
    | Apply (v, t)   -> Fmt.pf ppf "#%a(%a)" pp t pp v
    | Arrow _        -> Fmt.pf ppf "<function>"
    | Pair (a, b)    -> Fmt.Dump.pair (pp_val a) (pp_val b) ppf x
    | Result (a, b)  -> Fmt.Dump.result ppf ~ok:(pp_val a) ~error:(pp_val b) x
    | Either (a, b)  -> match x with
      | L x -> Fmt.pf ppf "L %a" (pp_val a) x
      | R x -> Fmt.pf ppf "R %a" (pp_val b) x

  let rec eq_val: type a. a t -> a Parsetree.eq = fun t a b ->
    match t with
    | Unit -> true
    | Int -> (=) a b
    | Int32 -> Int32.equal a b
    | Int64 -> Int64.equal a b
    | Bool -> (=) a b
    | String -> String.equal a b
    | Bytes  -> Bytes.equal a b
    | List t ->
      (try List.for_all2 (eq_val t) a b
       with Invalid_argument _ -> false)
    | Option t ->
      (match a, b with
       | None, None -> true
       | Some a, Some b -> (eq_val t) a b
       | Some _, None | None, Some _ -> false)
    | Array t ->
      (try List.for_all2 (eq_val t) (Array.to_list a) (Array.to_list b)
       with Invalid_argument _ -> false)
    | Pair (ta, tb) -> eq_val ta (fst a) (fst b) && eq_val tb (snd a) (snd b)
    | Result (tok, terr) ->
      (match a, b with
       | Ok va, Ok vb -> eq_val tok va vb
       | Error va, Error vb -> eq_val terr va vb
       | Ok _, Error _ | Error _, Ok _ -> false)
    | Either (tl, tr) ->
      (match a, b with
       | L va, L vb -> eq_val tl va vb
       | R va, R vb -> eq_val tr va vb
       | L _, R _ | R _, L _ -> false)
    | Lwt        -> invalid_arg "compare: type constructor"
    | Abstract _ -> invalid_arg "compare: abstract value"
    | Arrow _    -> invalid_arg "compare: functional value"
    | Apply _    -> invalid_arg "compare: unconstructed type"

  let rec eq: type a. a t Parsetree.eq
    = fun a b -> match a, b with
    | Unit,   Unit   -> true
    | Int,    Int    -> true
    | Int32,  Int32  -> true
    | Int64,  Int64  -> true
    | Bool,   Bool   -> true
    | String, String -> true
    | Lwt,    Lwt    -> true
    | List ta,   List tb   -> eq ta tb
    | Option ta, Option tb -> eq ta tb
    | Array ta,  Array tb  -> eq ta tb
    | Apply (ta, tb), Apply (tx, ty) ->
      eq ta tx && eq tb ty
    | Pair (ta, tb),   Pair (tx, ty) ->
      eq ta tx && eq tb ty
    | Result (ta, tb), Result (tx, ty) ->
      eq ta tx && eq tb ty
    | Either (ta, tb), Either (tx, ty) ->
      eq ta tx && eq tb ty
    | Abstract ta, Abstract tb ->
      (match Eq.Witness.eq ta.Eq.wit tb.Eq.wit with
       | Some Eq.Refl -> true
       | None -> false)
    | _, _ -> false

end

module Env = struct

  type 'e t =
    | [] : unit t
    | (::) : 'a Type.t * 'e t -> ('e * 'a) t

  let rec equal: type a b. a t -> b t -> (a, b) Eq.refl option = fun a b ->
    match a, b with
    | [], []               -> Some Eq.Refl
    | (at :: a), (bt :: b) -> Eq.(equal a b >*= fun Refl -> Type.equal at bt)
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

  let rec to_int : type e a. (e, a) t -> int = function
    | Z -> 0
    | S n -> 1 + (to_int n)

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
      let V (x', r', tr) = typ { Parsetree.id = n-1 } r in
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
    match Type.equal v.t t with
    | Some Eq.Refl -> v.v
    | None -> error x t

  let untype: type a. a Type.t -> a -> Parsetree.value = fun t v ->
    let pp = Type.pp_val t in
    let eq = Type.eq_val t in
    Parsetree.V {v; t; pp; eq}

  let untype_lwt:
    type a. a Lwt.t Type.t -> (a, Type.lwt) Type.app -> Parsetree.value =
    fun t (App v) ->
      let pp = Type.pp_val t in
      let eq = Type.eq_val t in
      Parsetree.V {v = Type.Lwt.prj v; t; pp; eq}

end

module Prim = struct

  type ('r, 'e) t = { p: Parsetree.primitive; f: (env:'e -> 'r) }

  type v = V: ('a, 'b) t * 'b Env.t * 'a Type.t -> v

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

  let typ: Parsetree.primitive -> Parsetree.typ list -> v =
    fun p g ->
      let rec chop n l = match n, l with
        | 0, l -> l
        | n, _ :: r -> chop (n -- 1) r
        | _, _ -> assert false in
      let miss = List.length g in
      let Env.V g' = Env.typ List.(rev_append p.args g) in
      let Type.V typ = Type.typ p.ret in
      V ({ p; f = (fun ~env ->
          let ua = env_to_list g' env in
          let uc = p.exp (chop miss ua) in
          Value.cast uc typ
        )}, g', typ)

  let untype: ('e, 'a) t -> Parsetree.primitive = fun p -> p.p

end

module Expr = struct

  type ('l, 'r, 'v) binop =
    | Add : (int, int, int) binop
    | Mul : (int, int, int) binop
    | Sub : (int, int, int) binop
    | Div : (int, int, int) binop
    | Pair : ('a, 'b, 'a * 'b) binop
    | Eq  : ('a, 'a, bool) binop
    | Cons : ('a, 'a list, 'a list) binop

  let pp_binop : type l r v. (l, r, v) binop Fmt.t = fun ppf -> function
    | Add  -> Fmt.string ppf "+"
    | Mul  -> Fmt.string ppf "*"
    | Sub  -> Fmt.string ppf "-"
    | Div  -> Fmt.string ppf "/"
    | Eq   -> Fmt.string ppf "="
    | Pair -> Fmt.string ppf ","
    | Cons -> Fmt.string ppf "::"

  let binop_to_string x = Fmt.strf "%a" pp_binop x

  type ('u, 'v) unop =
    | Fst: ('a * 'b, 'a) unop
    | Snd: ('a * 'b, 'b) unop
    | L  : 'b Type.t -> ('a, ('a, 'b) Type.either) unop
    | R  : 'a Type.t -> ('b, ('a, 'b) Type.either) unop
    | Oky: 'b Type.t -> ('a, ('a, 'b) result) unop
    | Err: 'a Type.t -> ('b, ('a, 'b) result) unop

  let pp_unop : type u v. (u, v) unop Fmt.t = fun ppf -> function
    | Fst    -> Fmt.string ppf "fst"
    | Snd    -> Fmt.string ppf "snd"
    | L ty   -> Fmt.pf ppf "L:%a" Type.pp ty
    | R ty   -> Fmt.pf ppf "R:%a" Type.pp ty
    | Oky ty -> Fmt.pf ppf "Ok:%a" Type.pp ty
    | Err ty -> Fmt.pf ppf "Error:%a" Type.pp ty

  type ('u, 'v) nnop =
    | Arr: 'a Type.t -> ('a, 'a array) nnop

  type 'a value = {
    t  : 'a Type.t;
    v  : 'a;
    pp : 'a Fmt.t;
    eq : 'a Parsetree.eq;
  }

  type 'a lwt = ('a, Type.lwt) Type.app

  type ('e, 'p, 'r) rec_ = {
    r   : 'r Type.t;                           (* type of the return function *)
    p   : (string * 'p Type.t);             (* name and type of the parameter *)
    body: ('e * 'p, ('p, 'r) Type.either) t;                 (* function body *)
  }

  and ('e, 'a) t =
    | Val : 'a value -> ('e, 'a) t
    | Prm : ('r, 'e) Prim.t -> ('e, 'r) t
    | Uno : ('a, 'res) unop * ('e, 'a) t -> ('e, 'res) t
    | Opt : 'a Type.t * ('e, 'a) t option -> ('e, 'a option) t
    | Ret : ('e, 'a) t -> ('e, 'a lwt) t
    | Bnd : ('e, 'a lwt) t * ('e, 'a -> 'b lwt) t -> ('e, 'b lwt) t
    | Bin : ('a, 'b, 'res) binop * ('e, 'a) t * ('e, 'b) t -> ('e, 'res) t
    | Nar : ('a, 'res) nnop * ('e, 'a) t list -> ('e, 'res) t
    | Var : ('e, 'a) Var.t -> ('e, 'a) t
    | Lam : 'a Type.t * string * ('e * 'a, 'b) t -> ('e, 'a -> 'b) t
    | Rec : ('e, 'p, 'r) rec_ -> ('e, 'p -> 'r) t
    | App : ('e, 'a -> 'b) t * ('e, 'a) t -> ('e, 'b) t
    | Let : 'a Type.t * string * ('e, 'a) t * ('e * 'a, 'b) t -> ('e, 'b) t
    | Swt : { s : ('e, ('l, 'r) Type.either) t
            ; a : ('e * 'l, 'a) t
            ; b : ('e * 'r, 'a) t } -> ('e, 'a) t
    | If  : ('e, bool) t * ('e, 'a) t * ('e, 'a) t -> ('e, 'a) t

  let rec pp : type e a. (e, a) t Fmt.t = fun ppf -> function
    | Val v ->
      v.pp ppf v.v
    | Prm p ->
      Fmt.pf ppf "%%%s" p.p.Parsetree.name
    | Uno (o, e) ->
      Fmt.pf ppf "@[<1>(@[%a@]@ @[%a@])@]" pp_unop o pp e
    | Bin (o, a, b) ->
      Type.pp_infix ~infix:(binop_to_string o) pp pp ppf (a, b)
    | Nar (Arr _, lst) ->
      Fmt.(Dump.array pp) ppf (Array.of_list lst)
    | Opt (ty, Some expr) ->
      Fmt.pf ppf "@[<1>(@[Some:%a option@]@ @[%a@])@]"
        Type.pp ty pp expr
    | Ret e -> Fmt.pf ppf "@[(return (%a))@]" pp e
    | Bnd (f, x) -> Fmt.pf ppf "@[(%a >>= %a)@]" pp x pp f
    | Opt (ty, None) ->
      Fmt.pf ppf "@[None:%a option@]" Type.pp ty
    | Var v ->
      Fmt.pf ppf "$%d" (Var.to_int v)
    | Lam (ty, arg, expr) ->
      Fmt.pf ppf "@[<2>(fun @[(%s:@ %a)@]@ -> @[<2>%a@])@]"
        arg Type.pp ty pp expr
    | Rec r ->
      Fmt.pf ppf "@[<2>(rec @[(%s:@ %a)@]@ -> @[<2>%a@])@]"
        (fst r.p) Type.pp (snd r.p) pp r.body
    | App (a, b) ->
      Fmt.pf ppf "@[(@[%a@]@ @[%a@])@]"
        pp a pp b
    | Let (ty, name, a, b) ->
      Fmt.pf ppf "@[<v>@[<v2>let @[%s: %a@] =@ @[%a@]@]@ in@ @[<2>%a@]@]"
        name Type.pp ty pp a pp b
    | Swt { s; a; b; } ->
       Fmt.pf ppf "@[<2>match %a with@\n| @[<2>@[L $1@] ->@ \
                   @[%a@]@]@\n| @[<2>@[R $1@] ->@ @[%a@]@]@]"
         pp s pp a pp b
    | If (t, a, b) ->
       Fmt.pf ppf "@[<v>@[<v2if %a then@,(%a)@]@,@[<v2>else@,(%a)@]@]"
         pp t pp a pp b

  let rec get: type e a. (e, a) Var.t -> e -> a = fun v e ->
    match v, e with
    | Var.Z, (_, x)   -> x
    | Var.S n, (r, _) -> get n r

  let return (x: 'a): 'a lwt =
    Type.App (Type.Lwt.inj (Lwt.return x))

  let bind (Type.App x: 'a lwt) (f:'a -> 'b lwt): 'b lwt =
    let f = Lwt.bind (Type.Lwt.prj x) (fun x ->
        let Type.App f = f x in
        Type.Lwt.prj f
      ) in
    Type.App (Type.Lwt.inj f)

  let rec eval: type e a. (e, a) t -> e -> a = fun x e ->
    match x with
    | Val v        -> v.v
    | Prm { f; _ } -> f ~env:e
    | Uno (L _, x) -> L (eval x e)
    | Uno (R _, x) -> R (eval x e)
    | Uno (Oky _, x) -> Ok (eval x e)
    | Uno (Err _, x) -> Error (eval x e)
    | Uno (Fst, x) -> fst (eval x e)
    | Uno (Snd, x) -> snd (eval x e)
    | Nar (Arr _, lst) -> List.map (fun x -> eval x e) lst |> Array.of_list
    | Opt (_, Some x) -> Some (eval x e)
    | Opt (_, None)   -> None
    | Ret x -> return (eval x e)
    | Bnd (x, f) -> bind (eval x e) (eval f e)
    | Var x -> get x e
    | Lam (_, _, r) -> (fun v -> eval r (e, v))
    | App (f, a) -> (eval f e) (eval a e)
    | Bin (Add, l, r) -> (eval l e) + (eval r e)
    | Bin (Sub, l, r) -> (eval l e) - (eval r e)
    | Bin (Mul, l, r) -> (eval l e) * (eval r e)
    | Bin (Div, l, r) -> (eval l e) / (eval r e)
    | Bin (Pair, l, r) -> (eval l e, eval r e)
    | Bin (Eq, l, r) -> (eval l e) = (eval r e)
    | Bin (Cons, x, r) -> (eval x e) :: (eval r e)
    | Let (_, _, a, f) ->
      eval f (e, (eval a e))
    | Swt { s; a; b; } ->
      (match eval s e with
       | L l -> eval a (e, l)
       | R r -> eval b (e, r))
    | Rec r ->
      let rec loop a = match eval r.body (e, a) with
        | L a -> loop a
        | R r -> r
      in
      eval (Var Var.o) (e, loop)
    | If (t, a, b) ->
      (match eval t e with
       | true -> eval a e
       | false -> eval b e)

  (* combinators *)

  let value t v = Val {t; v; pp = Type.pp_val t; eq = Type.eq_val t}

  let unit   _ = value Unit ()
  let int    x = value Int x
  let int32  x = value Int32 x
  let int64  x = value Int64 x
  let bool   x = value Bool x
  let string x = value String x

  let list t = value (List t)
  let array t = value (Array t)
  let option t = value (Option t)

  let lambda (n, t) e = Lam (t, n, e)
  let apply f a = App (f, a)
  let ($) = apply

  let fix p r body = Rec { r; p; body }

  let var x = Var x
  let if_ b t e = If (b, t, e)

  let ( = ) a b = Bin (Eq, a, b)
  let ( + ) a b = Bin (Add, a, b)
  let ( * ) a b = Bin (Mul, a, b)
  let ( - ) a b = Bin (Sub, a, b)

  let left e r = Uno (L r, e)
  let right l e = Uno (R l, e)

  let fst e = Uno (Fst, e)
  let snd e = Uno (Snd, e)
  let pair a b = Bin (Pair, a, b)

  let let_rec l r f =
    let context = var Var.o in
    let return x = right (snd_ l) x in
    let continue x = left x r in
    fix l r (f ~context ~return ~continue)

  let rec untype: type a e. (e, a) t -> Parsetree.expr = fun e ->
    let module P = Parsetree in
    match e with
    | Val {t=Type.List t;v=[];_} -> P.list ~typ:(Type.untype t) []
    | Val {t=Type.Array t;v=[||];_} -> P.array ~typ:(Type.untype t) [||]
    | Val {v;t;pp;eq}  -> P.value v t pp eq
    | Prm x            -> P.prim (Prim.untype x)
    | Uno (Fst, x)     -> P.fst (untype x)
    | Uno (Snd, x)     -> P.snd (untype x)
    | Uno (L r, x)     -> P.left (Type.untype r) (untype x)
    | Uno (R l, x)     -> P.right (Type.untype l) (untype x)
    | Uno (Oky e, x)   -> P.ok (Type.untype e) (untype x)
    | Uno (Err o, x)   -> P.error (Type.untype o) (untype x)
    | Nar (Arr t, [])  -> P.array ~typ:(Type.untype t) [||]
    | Nar (Arr _, l)   -> P.array (List.map untype l |> Array.of_list)
    | Opt (t, None)    -> P.none (Type.untype t)
    | Opt (_, Some x)  -> P.some (untype x)
    | Ret e            -> P.return (untype e)
    | Bnd (x, f)       -> P.bind (untype x) (untype f)
    | Bin (Add, x, y)  -> P.(untype x + untype y)
    | Bin (Mul, x, y)  -> P.(untype x * untype y)
    | Bin (Sub, x, y)  -> P.(untype x - untype y)
    | Bin (Div, x, y)  -> P.(untype x / untype y)
    | Bin (Eq , x, y)  -> P.(untype x = untype y)
    | Bin (Pair, x, y) -> P.pair (untype x) (untype y)
    | Bin (Cons, _, _) ->
      let rec go
        : type a e. Parsetree.expr list -> (e, a list) t -> Parsetree.expr list
        = fun acc -> function
        | Bin (Cons, x, r) -> go (untype x :: acc) r
        | Val {v=[];_} -> List.rev acc
        | expr -> List.rev (untype expr :: acc) in
      P.list (go [] e)
    | Var x            -> P.var (Var.to_int x)
    | Lam (t, n, e)    -> P.lambda [n, Type.untype t] (untype e)
    | App (a, b)       -> P.apply (untype a) (untype b)
    | Let (t, n, e, b) -> P.let_var (Type.untype t) n (untype e) (untype b)
    | If (a, b, c)     -> P.if_ (untype a) (untype b) (untype c)
    | Rec re           ->
      let r = Type.untype re.r in
      let p = fst_ re.p, Type.untype (snd_ re.p) in
      let b = untype re.body in
      P.fix p r b
    | Swt { s; a; b; } ->
      let s = untype s in
      let a = untype a in
      let b = untype b in
      P.match_ s a b

  type v = V: (unit, 'a) t * 'a Type.t -> v

  type a_expr = Expr: ('e, 'a) t * 'e Env.t * 'a Type.t -> a_expr

  type kind =
    | EnvMismatch of { g  : Env.v
                     ; g' : Env.v }
    | TypMismatch of { a  : Type.v
                     ; b  : Type.v }
    | ExpectedPair    of a_expr
    | ExpectedLambda  of a_expr
    | ExpectedEither  of a_expr
    | UnboundVariable of int

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
    | UnboundVariable n  -> Fmt.pf ppf "unbound variable $%d" n

  let pp_error ppf (e, ts, ks) =
    Fmt.pf ppf "error while evaluating:@ %a@ %a@ %a"
      Parsetree.pp e
      Fmt.(Dump.list Parsetree.Type.pp) ts
      Fmt.(Dump.list pp_kind) ks

  let uncurry args =
    let name =
      let n = ref 0 in
      fun () -> incr n; Fmt.strf "x%d" !n
    in
    List.fold_right (fun t -> function
        | Expr (a, Env.(ta :: e), tr) ->
          let Type.V t' = Type.typ t in
          (match Type.equal ta t' with
           | Some Eq.Refl ->
             Expr (Lam (t', name (), a), e, Arrow (t', tr))
           | None ->
             Fmt.invalid_arg "Type mismatch: %a <> %a." Type.pp ta Type.pp t')
        | _ -> invalid_arg "Impossible to uncurry, environment mismatch")
      args

  let typ e =
    let rec aux: Parsetree.expr -> Parsetree.typ list -> a_expr = fun e g ->
      let Env.V g' = Env.typ g in
      match e with
      | Val (Parsetree.V {v;t;pp;eq}) -> Expr (Val {v;t;pp;eq}, g', t)
      | Ret e ->
        let Expr (x, y, t) = aux e g in
        Expr (Ret x, y, Type.lwt t)
      | Bnd (x, f) ->
        (match aux x g, aux f g with
         | Expr (x', gx, Type.Apply (tx, Type.Lwt)),
           Expr (f', gf, Type.Arrow (a, Type.Apply (b, Type.Lwt))) ->
           (match Env.equal gx gf, Type.equal a tx with
            | Some Eq.Refl, Some Eq.Refl -> Expr (Bnd (x', f'), gf, Type.lwt b)
            | _, None ->
              Log.err (fun l -> l "Bnd");
              error e g [ TypMismatch { a = Type.V a; b = Type.V tx } ]
            | _ ->
              Log.err (fun l -> l "Bnd");
              error e g [ EnvMismatch { g = Env.V gx; g' = Env.V gf } ]
           )
         | wexp, _ ->
           Log.err (fun l -> l "Bnd");
           error e g [ ExpectedLambda wexp ])
      | Lst (t, l) -> typ_list t l g
      | Arr (t, a) ->
        let typ_array t a g =
          let Env.V g0 = Env.typ g in

          let Type.V t = match t, a with
            | Some t, _ -> Type.typ t
            | None, a when Array.length a > 0 ->
              let Expr (_, _, t) = aux (Array.get a 0) g in Type.V t
            | _  -> failwith "cannot type empty array" in

          let ta = Type.array t in

          let rec aux_a = function
            | [] -> Expr (Nar (Arr t, []), g0, ta)
            | a :: b ->
              match aux a g, aux_a b with
              | Expr (e1, g1, t1), Expr (Nar (Arr t, e2), g2, t2) ->
                (match
                   Type.equal t t1, Type.equal ta t2,
                   Env.equal g0 g1, Env.equal g0 g2
                 with
                 | Some Eq.Refl, Some Eq.Refl, Some Eq.Refl, Some Eq.Refl ->
                   Expr (Nar (Arr t, e1 :: e2), g0, ta)
                 | _, _, _, _ ->
                   error e g [ TypMismatch {a=Type.V t; b=Type.V t1}
                             ; TypMismatch {a=Type.V ta; b=Type.V t2}
                             ; EnvMismatch {g=Env.V g0; g'=Env.V g1}
                             ; EnvMismatch {g=Env.V g0; g'=Env.V g2} ])
              | _, _ -> assert false in
          aux_a (Array.to_list a) in
        typ_array t a g
      | Prm p ->
        let Prim.V (x, y, z) = Prim.typ p g in
        uncurry p.args (Expr (Prm x, y, z))
      | Uno (L tr, x) ->
        let Type.V tr' = Type.typ tr in
        let Expr (x', g'', tx') = aux x g in
        (match Env.equal g' g'' with
         | Some Eq.Refl -> Expr (Uno (L tr', x'), g', Either (tx', tr'))
         | _ -> error e g [ EnvMismatch { g = Env.V g'; g' = Env.V g'' } ])
      | Uno (Ok tr, x) ->
        let Type.V tr' = Type.typ tr in
        let Expr (x', g'', tx') = aux x g in
        (match Env.equal g' g'' with
         | Some Eq.Refl -> Expr (Uno (Oky tr', x'), g', Result (tx', tr'))
         | _ -> error e g [ EnvMismatch { g = Env.V g'; g' = Env.V g'' } ])
      | Uno (R tl, x) ->
        let Type.V tl' = Type.typ tl in
        let Expr (x', g'', tx') = aux x g in
        (match Env.equal g' g'' with
         | Some Eq.Refl -> Expr (Uno (R tl', x'), g', Either (tl', tx'))
         | _ -> error e g [ EnvMismatch { g = Env.V g'; g' = Env.V g'' } ])
      | Uno (Error tl, x) ->
        let Type.V tl' = Type.typ tl in
        let Expr (x', g'', tx') = aux x g in
        (match Env.equal g' g'' with
         | Some Eq.Refl -> Expr (Uno (Err tl', x'), g', Result (tl', tx'))
         | _ -> error e g [ EnvMismatch { g = Env.V g'; g' = Env.V g'' } ])
      | Uno (Fst, x) ->
        (match aux x g with
         | Expr (x', g'', Type.Pair (ta', _)) ->
           (match Env.equal g' g'' with
            | Some Eq.Refl -> Expr (Uno (Fst, x'), g', ta')
            | _ -> error e g [ EnvMismatch { g = Env.V g'; g' = Env.V g'' } ])
         | wexpr -> error e g [ ExpectedPair wexpr ])
      | Uno (Snd, x) ->
        (match aux x g with
         | Expr (x', g'', Type.Pair (_, tb')) ->
           (match Env.equal g' g'' with
            | Some Eq.Refl -> Expr (Uno (Snd, x'), g', tb')
            | _ -> error e g [ EnvMismatch { g = Env.V g'; g' = Env.V g'' } ])
         | wexpr -> error e g [ ExpectedPair wexpr ])
      | Opt (tr, None) ->
        let tr = match tr with Some t -> t | None -> failwith "cannot type None" in
        let Type.V tr' = Type.typ tr in
        Expr (Opt (tr', None), g', Option tr')
      | Opt (tr, Some x) ->
        let Expr (x', g'', tr') = aux x g in
        (match tr with
         | None   -> ()
         | Some t ->
           let Type.V t = Type.typ t in
           match Type.equal t tr' with
           | Some Eq.Refl -> ()
           | _ -> error e g [ TypMismatch {a=Type.V t; b=Type.V tr'} ]);
        (match Env.equal g' g'' with
         | Some Eq.Refl -> Expr (Opt (tr', Some x'), g', Option tr')
         | _ -> error e g [ EnvMismatch { g = Env.V g'; g' = Env.V g'' } ])
      | Bin (`Pair, a, b) ->
        let Expr (a', g'', ta') = aux a g in
        let Expr (b', g''', tb') = aux b g in
        (match Env.equal g' g'', Env.equal g'' g''' with
         | Some (Eq.Refl as g1), Some (Eq.Refl as g2) ->
           (match Eq.trans g1 g2 with
            | Eq.Refl -> Expr (Bin (Pair, a', b'), g', Pair (ta', tb')))
         | _, _ ->
           Log.err (fun l -> l "Bin `Pair");
           error e g [ EnvMismatch { g = Env.V g'; g' = Env.V g'' }
                     ; EnvMismatch { g = Env.V g''; g' = Env.V g''' } ])
      | Var x ->
        (try
           let Var.V (x', g'', t') = Var.typ x g in
           Expr (Var x', g'', t')
         with Invalid_argument _ -> error e g [ UnboundVariable 0 ])
      | Lam (t, n, e) ->
        let Type.V t' = Type.typ t in
        (match aux e (t :: g) with
         | Expr (e', Env.(t'' :: g'), tr) ->
           (match Type.equal t' t'' with
            | Some Eq.Refl -> Expr (Lam (t', n, e'), g', Type.Arrow (t', tr))
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
           (match Env.equal g' g'', Type.equal t' t'' with
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
           (match Env.equal g' g'' with
            | Some Eq.Refl -> Expr (Bin (binop, l', r'), g', Int)
            | None ->
              Log.err (fun l -> l "Bin");
              error e g [ EnvMismatch { g = Env.V g'; g' = Env.V g'' } ])
         | Expr (_, _, ta), Expr (_, _, tb) ->
           Log.err (fun l -> l "Bin");
           error e g [ TypMismatch { a = Type.V Int; b = Type.V ta }
                     ; TypMismatch { a = Type.V Int; b = Type.V tb } ])
      | Let (t, n, a, b) ->
        let Type.V t' = Type.typ t in
        (match aux a g, aux b (t :: g) with
         | Expr (a', g'', t''), Expr (f', g''', r') ->
           (match
              Type.equal t' t'', Env.equal g' g'', Env.equal (t'' :: g'') g'''
            with
            | Some Eq.Refl, Some Eq.Refl, Some Eq.Refl ->
              Expr (Let (t', n, a', f'), g', r')
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
           (match Type.equal u' v', Env.equal g' g'', Env.equal g'' g''' with
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
        (match Type.equal ta' tb', Env.equal g' g'' with
         | Some Eq.Refl, Some Eq.Refl -> Expr (Bin (Eq, a', b'), g', Bool)
         | _, _ -> error e g [ TypMismatch {a=Type.V ta'; b=Type.V tb'}
                             ; EnvMismatch {g=Env.V g'; g'=Env.V g''} ])
      | Swt { a; b; s; } ->
        (match aux s g with
         | Expr (s', g'', Type.Either (l', r')) ->
           let l = Type.untype l' in
           let r = Type.untype r' in
           (match aux a (l :: g), aux b (r :: g) with
            | Expr (a', Env.(l'' :: ag''), ar),
              Expr (b', Env.(r'' :: bg''), br) ->
              (match Type.equal l' l'', Type.equal r' r'',
                     Env.equal ag'' g'', Env.equal bg'' g'',
                     Type.equal ar br with
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
      | Rec {r=rtyp; p=(n, ptyp); e }  ->
        let Type.V r = Type.typ rtyp in
        let Type.V p = Type.typ ptyp in
        let te = Type.Either (p, r) in
        let tb = Type.Arrow (p, r) in
        (match aux e (ptyp :: g) with
         | Expr (e', Env.(te' :: g''), tr) ->
           (match Type.equal tr te, Type.equal te' p, Env.equal g' g'' with
            | Some Eq.Refl, Some Eq.Refl, Some Eq.Refl ->
              Expr (fix (n, p) r e', g', tb)
            | Some Eq.Refl, None, _ ->
              error e g [ TypMismatch { a = Type.V te'; b = Type.V p }]
            | None, Some Eq.Refl, _ ->
              error e g [ TypMismatch { a = Type.V tr; b = Type.V te }]
            | Some Eq.Refl, Some Eq.Refl, None ->
              error e g [ EnvMismatch { g = Env.V g'; g' = Env.V g'' } ]
            | None, None, _ ->
              error e g [ TypMismatch { a = Type.V te'; b = Type.V p };
                          TypMismatch { a = Type.V tr; b = Type.V te }])
         | Expr (_, g', _) ->
           error e g [ EnvMismatch { g = Env.(V (p :: g')); g' = Env.V g' }])
    and typ_list t l g =
      let Env.V g0 = Env.typ g in

      let Type.V t = match t, l with
        | Some t, _  -> Type.typ t
        | None, h::_ -> let Expr (_, _, t) = aux h g in Type.V t
        | _          -> failwith "cannot type the empty list" in

      let tl = Type.list t in

      let rec aux_l = function
        | []   -> Expr (list t [], g0, tl)
        | a::b ->
          match aux a g, aux_l b with
          | Expr (e1, g1 , t1), Expr (e2, g2, t2) ->
            (match
               Type.equal t t1, Type.equal tl t2,
               Env.equal g0 g1, Env.equal g0 g2
             with
             | Some Eq.Refl, Some Eq.Refl, Some Eq.Refl, Some Eq.Refl ->
               Expr (Bin (Cons, e1, e2), g0, tl)
             | _, _, _, _ ->
               (* TODO(dinosaure): more explicit error. *)
               error e g [ TypMismatch {a=Type.V t; b=Type.V t1}
                         ; TypMismatch {a=Type.V tl; b=Type.V t2}
                         ; EnvMismatch {g=Env.V g0; g'=Env.V g1}
                         ; EnvMismatch {g=Env.V g0; g'=Env.V g2} ]) in
      aux_l l in
    match aux e [] with
    | Expr (m, Env.[], t) -> Ok (V (m ,t))
    | Expr (_, g', _) -> Error (e, [], [EnvMismatch {g=Env.V []; g'=Env.V g'}])
    | exception Break e -> Error e
end

type expr = Expr.v
type 'a typ = 'a Type.t
type value = V: 'a * 'a Type.t -> value

let pp_value ppf (V (v, t)) = Type.pp_val t ppf v

open Expr

let err_type_mismatch m t t': (_, error) result =
  Error (m, [], [ TypMismatch { a = Type.V t; b = Type.V t' } ])
