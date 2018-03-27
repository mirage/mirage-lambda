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

let pp_infix ~infix pp_a pp_b ppf (a, b) =
  Fmt.pf ppf "@[<1>(@[%a@]@ %s @[%a@])@]" pp_a a infix pp_b b

module Type = struct

  type abstract = A: 'a Eq.witness -> abstract

  type t =
    | Unit
    | Int
    | Bool
    | String
    | Lwt
    | Apply of t * t
    | Abstract of t list * abstract
    | Arrow of t * t
    | Pair of t * t
    | Either of t * t

  let pp_abstract ppf (A a) = Fmt.string ppf a.name

  let rec pp ppf = function
    | Unit            -> Fmt.string ppf "unit"
    | Int             -> Fmt.string ppf "int"
    | Bool            -> Fmt.string ppf "bool"
    | String          -> Fmt.string ppf "string"
    | Lwt             -> Fmt.pf ppf "Lwt.t"
    | Apply (a, b)    -> Fmt.pf ppf "@[(%a %a)@]" pp a pp b
    | Abstract (a, b) -> Fmt.pf ppf "@[%a%a@]" pp_params a pp_abstract b
    | Arrow (a, b)    -> Fmt.pf ppf "%a" (pp_infix ~infix:"->" pp pp) (a, b)
    | Pair (a, b)     -> Fmt.pf ppf "%a" (pp_infix ~infix:"*" pp pp) (a, b)
    | Either (a, b)   -> Fmt.pf ppf "%a" (pp_infix ~infix:"|" pp pp) (a, b)

  and pp_params ppf = function
    | []  -> ()
    | [x] -> Fmt.pf ppf "%a " pp x
    | l   -> Fmt.pf ppf "(%a)" Fmt.(list ~sep:(unit ", ") pp) l

  let unit = Unit
  let int = Int
  let bool = Bool
  let string = String
  let lwt = Lwt

  let arrow a b = Arrow (a, b)
  let either a b = Either (a, b)
  let abstract a ps = Abstract (ps, A a)
  let apply a b = Apply (a, b)

  let ( ** ) a b = Pair (a, b)
  let ( @->) = arrow
  let ( || ) = either

end

type typ = Type.t

type value = V: { v: 'a; t: 'a T.t; pp: 'a Fmt.t } -> value

let pp_value ppf (V t) = t.pp ppf t.v

type arithmetic = [ `Add | `Sub | `Mul | `Div ]

type primitive =
  { name : string
  ; typ  : typ list * typ
  ; exp  : value list -> value }
and binop =
  [ arithmetic | `Pair | `Eq ]
and unop =
  | Fst | Snd | L of typ | R of typ
and expr =
  | Val of value
  | Prm of primitive
  | Var of var
  | Abs of typ * string * expr
  | App of expr * expr
  | Bin of binop * expr * expr
  | Uno of unop * expr
  | Let of typ * expr * expr
  | Swt of { a : expr
           ; b : expr
           ; s : expr }
  | Rec of typ * expr * expr
  | If  of expr * expr * expr
and var = {
  id : int;
}

let dump_var ppf v = Fmt.pf ppf "$%d" v.id

let pp ppf t =
  let rec aux ctx ppf t =
    let pp = aux ctx in
    match t with
    | Var n ->
      let name = List.nth ctx n.id in
      Fmt.pf ppf "%s$%d" name n.id
    | Val c -> pp_value ppf c
    | Prm { name; typ = (a, r); _ } ->
      let params = List.fold_right (fun x a -> Type.Arrow (x, a)) a r in
      Fmt.pf ppf "(@[<1>#%s@ @[%a@]@])"
        name Type.pp params
    | Abs (t, name, e) ->
      let pp = aux (name :: ctx) in
      Fmt.pf ppf "@[<2>(fun @[(%s:@ %a)@]@ ->@ @[<2>%a@])@]"
        name Type.pp t pp e
    | App (f, a) ->
      Fmt.pf ppf "@[@[%a@]@ @[%a@]@]" pp f pp a
    | Bin (`Add, a, b) ->
      Fmt.pf ppf "%a" (pp_infix ~infix:"+" pp pp) (a, b)
    | Bin (`Sub, a, b) ->
      Fmt.pf ppf "%a" (pp_infix ~infix:"-" pp pp) (a, b)
    | Bin (`Mul, a, b) ->
      Fmt.pf ppf "%a" (pp_infix ~infix:"*" pp pp) (a, b)
    | Bin (`Div, a, b) ->
      Fmt.pf ppf "%a" (pp_infix ~infix:"/" pp pp) (a, b)
    | Bin (`Eq, a, b) ->
      Fmt.pf ppf "%a" (pp_infix ~infix:"=" pp pp) (a, b)
    | Bin (`Pair, a, b) ->
      Fmt.pf ppf "%a" (Fmt.Dump.pair pp pp) (a, b)
    | Uno (Fst, a) ->
      Fmt.pf ppf "@[<2>(fst@ @[%a@])@]" pp a
    | Uno (Snd, a) ->
      Fmt.pf ppf "@[<2>(snd@ @[%a@])@]" pp a
    | Uno (L _, a) ->
      Fmt.pf ppf "@[<2>(L@ @[%a@])@]" pp a
    | Uno (R _, a) ->
      Fmt.pf ppf "@[<2>(R@ @[%a@])@]" pp a
    | Let (t, v, f) ->
      Fmt.pf ppf "@[<2>let @[$1 : %a@] =@ @[%a@] in@ @[%a@]@]"
        (Fmt.hvbox Type.pp) t (Fmt.hvbox pp) v (Fmt.hvbox pp) f
    | Swt { a; b; s; } ->
      Fmt.pf ppf "@[<2>@[match %a with@\n| @[<2>@[L $1@] ->@ \
                  @[%a@]@]@\n| @[<2>@[R $1@] ->@ @[%a@]@]@]@]"
        pp s pp a pp b
    | Rec (_, z, s) ->
      Fmt.pf ppf "@[<2>fix@\n@[init:%a@]@\n@[%a@]@]" pp z pp s
    | If (t, a, b) ->
      Fmt.pf ppf "@[if %a@\nthen @[%a@]@\nelse @[%a@]@]"
        pp t pp a pp b
  in
  aux [] ppf t

let value v t pp = Val (V {v; t; pp})

let unit = value () Unit (fun ppf () -> Fmt.pf ppf "()")
let int n = value n Int Fmt.int
let string s = value s String Fmt.string
let pair a b = Bin (`Pair, a, b)
let lambda t name f = Abs (t, name, f)
let apply f a = App (f, a)
let fix ~typ ~init s = Rec (typ, init, s)
let var id = Var { id }
let match_ s a b = Swt {a; b; s}
let bool b = value b Bool Fmt.bool
let true_ = bool true
let false_ = bool false
let let_ t x y = Let (t, x, y)

let if_ t a b = If (t, a, b)
let left rtyp x = Uno (L rtyp, x)
let right ltyp x = Uno (R ltyp, x)
let fst x = Uno (Fst, x)
let snd x = Uno (Snd, x)

let primitive name inputs output exp = Prm {name; typ = (inputs, output); exp}

let equal a b =
  let rec aux k a b = match a, b with
    | Var a    , Var b     -> k (a.id = b.id)
    | Val (V a), Val (V b) ->
      (match T.eq a.t b.t with
       | Some Eq.Refl -> k (a.v = b.v)
       | None          -> k false)
    | Prm a, Prm b -> k (a.name = b.name && a.typ = b.typ)
    | Abs (a, b, c), Abs (d, e, f) ->
      aux (fun x -> k (x && a=d && b=e)) c f
    | App (a, b), App (c, d) ->
      aux (fun x -> aux (fun y -> k @@ x && y) a c) b d
    | Bin (a, b, c), Bin (d, e, f) ->
      aux (fun x -> aux (fun y -> k @@ x && y && a=d) b e) c f
    | Uno (a, b), Uno (c, d) ->
      aux (fun x -> x && k (a=c)) b d
    | Let (a, b, c), Let (d, e, f) ->
      aux (fun x -> aux (fun y -> k @@ x && y && a=d) b e) c f
    | Swt a, Swt b ->
      aux (fun x -> x && aux (fun y ->
          aux (fun z -> k @@ x && y && z) a.a b.a
        ) a.b b.b) a.s b.s
    | Rec (a, b, c), Rec (d, e, f) ->
      aux (fun x -> aux (fun y -> k @@ x && y && a=d) b e) c f
    | If (a, b, c), If (d, e, f) ->
      aux (fun x -> aux (fun y ->
          aux (fun z -> k @@ x && y && z) a d
        ) b e) c f
    | Var _, _ | Val _, _ | Prm _, _ | Abs _, _ | App _, _
    | Bin _, _ | Uno _, _ | Let _, _ | Swt _, _ | Rec _, _
    | If _, _ -> false
  in
  aux (fun x -> x) a b

let ( = ) a b = Bin (`Eq, a, b)
let ( + ) a b = Bin (`Add, a, b)
let ( * ) a b = Bin (`Mul, a, b)
let ( - ) a b = Bin (`Sub, a, b)
