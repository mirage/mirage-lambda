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

(** Parsetree *)

module Type: sig

  type abstract = private A: 'a Eq.witness -> abstract

  type t = private
    | Unit
    | Int
    | Int32
    | Int64
    | Bool
    | String
    | Lwt
    | List of t
    | Array of t
    | Option of t
    | Apply of t * t
    | Arrow of t * t
    | Pair of t * t
    | Either of t * t
    | Result of t * t
    | Abstract of abstract

  val unit: t
  val int: t
  val int32: t
  val int64: t
  val bool: t
  val string: t
  val lwt: t

  val list: t -> t
  val array: t -> t
  val option: t -> t
  val either: t -> t -> t
  val result: t -> t -> t

  val abstract: 'a Eq.witness -> t
  val apply: t -> t -> t

  val ( ** ): t -> t -> t
  val ( @->): t -> t -> t
  val ( || ): t -> t -> t

  val pp: t Fmt.t
end

type value = V: { v: 'a; t: 'a T.t; pp: 'a Fmt.t } -> value

type typ = Type.t

type var = {id: int}

type arithmetic = [ `Add | `Sub | `Mul | `Div ]

type expr = private
  | Val of value
  | Prm of primitive
  | Lst of typ option * expr list
  | Arr of typ option * expr array
  | Opt of typ option * expr option
  | Var of var
  | Lam of typ * string * expr
  | Rec of { r: typ; p: string * typ; e: expr }
  | App of expr * expr
  | Bin of binop * expr * expr
  | Uno of unop * expr
  | Let of typ * string * expr * expr
  | Swt of { a : expr
           ; b : expr
           ; s : expr }
  | If  of expr * expr * expr

and primitive =
  { name : string
  ; typ  : typ list * typ
  ; exp  : value list -> value }

and binop =  [ arithmetic | `Pair | `Eq ]
and unop =  Fst | Snd | L of typ | R of typ | Ok of typ | Error of typ

val pp: expr Fmt.t
val pp_value: value Fmt.t

val dump_var: var Fmt.t

val unit: expr

val int: int -> expr
val int32: int32 -> expr
val int64: int64 -> expr
val list: ?typ:typ -> expr list -> expr
val array: ?typ:typ -> expr array -> expr
val none: typ -> expr
val some: expr -> expr
val ok: typ -> expr -> expr
val error: typ -> expr -> expr

val string: string -> expr

val true_: expr
val false_: expr

val value: 'a -> 'a T.t -> 'a Fmt.t -> expr

val lambda: (string * typ) list -> expr -> expr

val pair: expr -> expr -> expr

val fst: expr -> expr
val snd: expr -> expr

val left: typ -> expr -> expr
val right: typ -> expr -> expr

val let_var: typ -> string -> expr -> expr -> expr
val let_fun: typ -> string -> (string * typ) list -> expr -> expr -> expr
val let_rec: typ -> string -> string * typ -> expr -> expr -> expr

val if_: expr -> expr -> expr -> expr
val match_: expr -> expr -> expr -> expr
val apply: expr -> expr -> expr
val fix: (string * typ) -> typ -> expr -> expr

val var: int -> expr
val primitive: string -> typ list -> typ -> (value list -> value) -> expr

val ( = ): expr -> expr -> expr
val ( + ): expr -> expr -> expr
val ( - ): expr -> expr -> expr
val ( * ): expr -> expr -> expr

val equal: expr -> expr -> bool
