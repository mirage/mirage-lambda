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

  type abstract = A: { name: string; wit: 'a Eq.Witness.t } -> abstract

  let name (A a) = a.name

  type t =
    | Int
    | Bool
    | String
    | Abstract of abstract
    | Lambda of t * t
    | Pair of t * t
    | Either of t * t

  let rec pp ppf = function
    | Int           -> Fmt.string ppf "int"
    | Bool          -> Fmt.string ppf "bool"
    | String        -> Fmt.string ppf "string"
    | Abstract a    -> Fmt.string ppf (name a)
    | Lambda (a, b) -> Fmt.pf ppf "%a" (pp_infix ~infix:"->" pp pp) (a, b)
    | Pair (a, b)   -> Fmt.pf ppf "%a" (pp_infix ~infix:"*" pp pp) (a, b)
    | Either (a, b) -> Fmt.pf ppf "%a" (pp_infix ~infix:"|" pp pp) (a, b)

  let int = Int
  let bool = Bool
  let string = String
  let lambda a b = Lambda (a, b)
  let either a b = Either (a, b)
  let ( ** ) a b = Pair (a, b)
  let ( @->) = lambda
  let ( || ) = either

  let abstract name =
    let wit = Eq.Witness.v () in
    Abstract (A {name; wit})

end

type typ = Type.t

module Param = struct

  type abstract =
    | Abs: { name: string; v: 'a; wit : 'a Eq.Witness.t } -> abstract

  type t =
    | Int of int
    | Bool of bool
    | String of string
    | Abstract of abstract
    | Pair of t * t
    | Either of Type.t * either
    | Lambda of { typ: Type.t * Type.t
                ; exp: (t -> t) }

  and either = L of t | R of t

  let pp_abstract ppf (Abs a) = Fmt.pf ppf "<abstract:%s>" a.name

  let rec pp ppf = function
    | Int n           -> Fmt.pf ppf "%d" n
    | Bool b          -> Fmt.pf ppf "%b" b
    | String s        -> Fmt.pf ppf "%s" s
    | Abstract a      -> pp_abstract ppf a
    | Pair (a, b)     -> Fmt.pf ppf "%a" (Fmt.Dump.pair pp pp) (a, b)
    | Either (_, L x) -> Fmt.pf ppf "@[<1>(L@ %a)@]" pp x
    | Either (_, R x) -> Fmt.pf ppf "@[<1>(R@ %a)@]" pp x
    | Lambda { typ = (ta, tb); _ } ->
      Fmt.pf ppf "@[<hov>#lambda:%a@]" Type.pp (Lambda (ta, tb))

  let int i = Int i
  let bool b = Bool b
  let string s = String s
  let pair a b = Pair (a, b)
  let either t e = Either (t, e)
  let lambda i o exp = Lambda { typ = (i, o); exp }

  let ( ** ) = pair

end

type param = Param.t

type arithmetic = [ `Add | `Sub | `Mul | `Div ]

type primitive =
  { name : string
  ; typ  : Type.t list * Type.t
  ; exp  : param list -> param }
and binop =
  [ arithmetic | `Pair | `Eq ]
and unop =
  | Fst | Snd | L of Type.t | R of Type.t
and expr =
  | Con of Param.t
  | Prm of primitive
  | Var of var
  | Abs of Type.t * string * expr
  | App of expr * expr
  | Bin of binop * expr * expr
  | Uno of unop * expr
  | Let of Type.t * expr * expr
  | Swt of { a : expr
           ; b : expr
           ; s : expr }
  | Rec of Type.t * expr * expr
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
    | Con c -> Fmt.pf ppf "%a" Param.pp c
    | Prm { name; typ = (a, r); _ } ->
      Fmt.pf ppf "(@[<1>#%s@ @[%a@]@])"
        name
        Type.pp (List.fold_right (fun x a -> Type.Lambda (x, a)) a r)
    | Abs (t, name, e) ->
      let pp = aux (name :: ctx) in
      Fmt.pf ppf "@[<2>(fun @[(%s:@ %a)@]@ ->@ @[<2>%a@])@]" name Type.pp t pp e
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

let int n = Con (Int n)
let string s = Con (String s)
let pair a b = Bin (`Pair, a, b)
let lambda t name f = Abs (t, name, f)
let apply f a = App (f, a)
let fix ~typ ~init s = Rec (typ, init, s)
let var id = Var { id }
let match_ s a b = Swt {a; b; s}
let bool b = Con (Bool b)
let true_ = bool true
let false_ = bool false
let let_ t x y = Let (t, x, y)

let ( = ) a b = Bin (`Eq, a, b)
let ( + ) a b = Bin (`Add, a, b)
let ( * ) a b = Bin (`Mul, a, b)
let ( - ) a b = Bin (`Sub, a, b)

let if_ t a b = If (t, a, b)
let left rtyp x = Uno (L rtyp, x)
let right ltyp x = Uno (R ltyp, x)
let fst x = Uno (Fst, x)
let snd x = Uno (Snd, x)

let primitive name inputs output exp = Prm {name; typ = (inputs, output); exp}
