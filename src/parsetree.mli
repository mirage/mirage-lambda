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
    (** Unit type. *)
    | Int
    (** Native integer. *)
    | Int32
    (** 32-bits integer. *)
    | Int64
    (** 64-bits integer. *)
    | Bool
    (** Bool type. *)
    | String
    (** String type. *)
    | Bytes
    (** Bytes type. *)
    | Lwt
    (** Lwt constructor. *)
    | List of t
    (** List type (unary type constructor). *)
    | Array of t
    (** Array type (unary type constructor). *)
    | Option of t
    (** Option type (unary type constructor). *)
    | Apply of t * t
    (** Application of unary constructor with type. *)
    | Arrow of t * t
    (** Function type constructor. *)
    | Pair of t * t
    (** Pair type constructor. *)
    | Either of t * t
    (** Either type constructor. *)
    | Result of t * t
    (** Result type constructor. *)
    | Abstract of abstract
    (** Abstract type. *)

  (** {2 Type constructor.} *)

  val unit: t
  val int: t
  val int32: t
  val int64: t
  val bool: t
  val string: t
  val bytes: t
  val lwt: t

  val list: t -> t
  val array: t -> t
  val option: t -> t
  val either: t -> t -> t
  val result: t -> t -> t

  val abstract: 'a Eq.witness -> t
  val unsafe_abstract: abstract -> t
  val apply: t -> t -> t

  (** {2 Infix operators.} *)

  val ( ** ): t -> t -> t
  val ( @->): t -> t -> t
  val ( || ): t -> t -> t

  (** {2 Pretty-printer.} *)

  val pp: t Fmt.t
  val dump: t Fmt.t
end

type 'a eq = 'a -> 'a -> bool

(** OCaml value (with type and pretty-printer). *)
type value = V: { v: 'a; t: 'a T.t; pp: 'a Fmt.t; eq: 'a eq; } -> value

(** Type of {!expr}. *)
type typ = Type.t

(** De-bruijn variable. *)
type var = {id: int}

(** Arithmetic operations. *)
type arithmetic = [ `Add | `Sub | `Mul | `Div ]

type expr = private
  | Val of value
  (** OCaml value. *)
  | Prm of primitive
  (** Primitive. *)
  | Lst of typ option * expr list
  (** List term. *)
  | Arr of typ option * expr array
  (** Array term. *)
  | Opt of typ option * expr option
  (** Option term. *)
  | Ret of expr
  (** Monadic return *)
  | Bnd of expr * expr
  (** Monadic bind *)
  | Var of var
  (** Variable. *)
  | Lam of typ * string * expr
  (** Lambda expression. *)
  | Rec of { r: typ; p: string * typ; e: expr }
  (** Recursive expression. *)
  | App of expr * expr
  (** Application term. *)
  | Bin of binop * expr * expr
  (** Binary operation. *)
  | Uno of unop * expr
  (** Unary operation. *)
  | Let of typ * string * expr * expr
  (** Let expression. *)
  | Swt of { a : expr
           ; b : expr
           ; s : expr }
  (** Switch on either value term. *)
  | If  of expr * expr * expr
  (** Conditional term. *)

and primitive =
  { name : string
  ; args : typ list
  ; ret  : typ
  ; exp  : value list -> value }
(** A user-defined primitive. *)

and binop =  [ arithmetic | `Pair | `Eq ]
(** Binary operations. *)

(** Unary operations. *)
and unop =  Fst | Snd | L of typ | R of typ | Ok of typ | Error of typ

(** {2 Pretty-printers.} *)

val pp: expr Fmt.t
val to_string: expr -> string
val equal: expr eq
val dump: expr Fmt.t

val pp_value: value Fmt.t

val dump_var: var Fmt.t

(** {2 Constructors.} *)

val unit: expr
val prim: primitive -> expr

val int: int -> expr
val int32: int32 -> expr
val int64: int64 -> expr
val list: ?typ:typ -> expr list -> expr
val array: ?typ:typ -> expr array -> expr
val none: typ -> expr
val some: expr -> expr
val ok: typ -> expr -> expr
val error: typ -> expr -> expr
val return: expr -> expr
val bind: expr -> expr -> expr

val string: string -> expr

val true_: expr
val false_: expr

val value: 'a -> 'a T.t -> 'a Fmt.t -> 'a eq -> expr
val of_value: value -> expr

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

(** {2 Infix operators.} *)

val ( = ): expr -> expr -> expr
val ( + ): expr -> expr -> expr
val ( - ): expr -> expr -> expr
val ( * ): expr -> expr -> expr
val ( / ): expr -> expr -> expr
