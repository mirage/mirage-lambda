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

module Type: sig

  module Lwt: Higher.Newtype1 with type 'a s = 'a Lwt.t
  type lwt = Lwt.t

  type ('a, 'b) app = App of ('a, 'b) Higher.app
  type 'a t = 'a T.t

  type ('a, 'b) either = ('a, 'b) T.either =
    | L of 'a
    | R of 'b

  val eq: 'a t -> 'b t -> ('a, 'b) Eq.refl option
  (** [eq a b] proves than [a] and [b] have the same type if we return [Some
     Eq.Refl]. *)

  val untype: 'a t -> Parsetree.typ
  (** [untype t] un-types [t] to a {!Parsetree.typ} value. *)

  (** {2 Constructors.} *)

  val unit: unit t
  val int: int t
  val int32: int32 t
  val int64: int64 t
  val bool: bool t
  val string: string t

  val list: 'a t -> 'a list t
  val option: 'a t -> 'a option t
  val array: 'a t -> 'a array t
  val abstract: string -> 'a t

  val lwt: 'a t -> ('a, lwt) app t
  val apply: 'a t -> 'b t -> ('a, 'b) app t

  val either: 'a t -> 'b t -> ('a, 'b) either t
  val result: 'a t -> 'b t -> ('a, 'b) result t

  (** {2 Infix operators.} *)

  val ( @->): 'a t -> 'b t -> ('a -> 'b) t
  val ( ** ): 'a t -> 'b t -> ('a * 'b) t
  val ( || ): 'a t -> 'b t -> ('a, 'b) either t

  (** {2 Pretty-printer.} *)

  val pp_val: 'a t -> 'a Fmt.t

  (** {2 Witness type value.} *)

  type v = V : 'a t -> v

  val typ: Parsetree.typ -> v
  (** [typ ut] returns a witness of type [ut]. *)
end

module Value: sig

  val cast: Parsetree.value -> 'a Type.t -> 'a
  (** [cast value typ] casts unsafe [value] with [typ]. If [value] does have the
     type [typ], we raise [Invalid_argument]. *)

  val untype: 'a Type.t -> 'a -> Parsetree.value
  (** [untype ty value] wraps [value] to {!Parsetree.value} and, by this way,
     untypes it (however, we keep a witness of type). *)

  val untype_lwt: 'a Lwt.t Type.t -> ('a, Type.lwt) Type.app -> Parsetree.value
  (** [untype_lwt ty value] wraps an LWT [value] to {!Parsetree.value} and, by this way,
      untypes it (however, we keep a witness of type). *)
end

module Var: sig

  type ('a, 'b) t

  val o : ('a * 'b, 'b) t
  (** Zero De-bruijn indice. *)

  val x : unit
  (** Unit value. *)

  val ( $ ) : ('a, 'b) t -> unit -> ('a * 'c, 'b) t
  (** Infix operator to construct De-bruijn indice in a church-style (e.g.
     [o$x$x$x = 3]). *)
end

module Expr: sig

  type ('a, 'e) t
  (** Type of a typed expression. *)

  (** {2 Constructors.} *)

  val unit: ('a, unit) t
  val int: int -> ('a, int) t
  val bool: bool -> ('a, bool) t
  val string: string -> ('a, string) t
  val pair: ('a, 'b) t -> ('a, 'c) t -> ('a, 'b * 'c) t

  val fst: ('a, 'b * 'c) t -> ('a, 'b) t
  val snd: ('a, 'b * 'c) t -> ('a, 'c) t

  val left : ('a, 'b) t -> ('a, ('b, 'c) Type.either) t
  val right: ('a, 'b) t -> ('a, ('c, 'b) Type.either) t

  val var: ('a, 'b) Var.t -> ('a, 'b) t

  val if_: ('a, bool) t -> ('a, 'b) t -> ('a, 'b) t -> ('a, 'b) t

  val fix: 'a Type.t -> 'b Type.t ->
    ('c * 'a, ('a, 'b) Type.either) t ->  ('c, 'a -> 'b) t

  val let_rec: 'a Type.t -> 'b Type.t ->
    (context : ('e *'a, 'a) t ->
     return  :(('f, 'b) t -> ('f, ('a, 'b) Type.either) t) ->
     continue:(('g, 'a) t -> ('g, ('a, 'b) Type.either) t) ->
     ('e * 'a, ('a, 'b) Type.either) t
    ) ->  ('e, 'a -> 'b) t

  val lambda: 'a Type.t -> ('b * 'a, 'c) t -> ('b, 'a -> 'c) t
  val apply: ('a, 'b -> 'c) t -> ('a, 'b) t -> ('a, 'c) t
  val eval: ('e, 'a) t -> 'e -> 'a

  (** {2 Infix operators.} *)

  val ( = ): ('a, 'b) t -> ('a, 'b) t -> ('a, bool) t
  val ( + ): ('a, int) t -> ('a, int) t -> ('a, int) t
  val ( - ): ('a, int) t -> ('a, int) t -> ('a, int) t
  val ( * ): ('a, int) t -> ('a, int) t -> ('a, int) t
  val ( $ ): ('a, 'b -> 'c) t -> ('a, 'b) t -> ('a, 'c) t
end

type 'a typ = 'a Type.t

type expr = E: (unit, 'a) Expr.t * 'a Type.t -> expr
(** Expression with type witness. *)

type v    = V: 'a * 'a Type.t -> v
(** Value with type witness. *)

type error
(** Type of errors. *)

val pp_error: error Fmt.t
(** Pretty-printer of {!error}. *)

val typ: Parsetree.expr -> (expr, error) result
(** [typ unsafe_expr] tries to type [unsafe_expr] and returns an {!expr}. *)

val err_type_mismatch:
  Parsetree.expr -> 'a Type.t -> 'b Type.t -> ('c, error) result
