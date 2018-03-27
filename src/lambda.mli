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

module Untyped: sig

  module Type: sig

    type abstract

    type t = private
      | Int
      | Bool
      | String
      | Abstract of abstract
      | Lambda of t * t
      | Pair of t * t
      | Either of t * t

    val int: t
    val bool: t
    val string: t
    val abstract: string -> t

    val ( ** ): t -> t -> t
    val ( @->): t -> t -> t
    val ( || ): t -> t -> t

  end

  type typ = Type.t

  type expr

  val pp: expr Fmt.t

  val int: int -> expr
  val string: string -> expr
  val true_: expr
  val false_: expr

  val lambda: typ -> string -> expr -> expr

  val pair: expr -> expr -> expr

  val fst: expr -> expr
  val snd: expr -> expr

  val left: typ -> expr -> expr
  val right: typ -> expr -> expr

  val if_: expr -> expr -> expr -> expr
  val match_: expr -> expr -> expr -> expr
  val let_: typ -> expr -> expr -> expr
  val apply: expr -> expr -> expr
  val fix: typ:typ -> init:expr -> expr -> expr

  val var: int -> expr

  val ( = ): expr -> expr -> expr
  val ( + ): expr -> expr -> expr
  val ( - ): expr -> expr -> expr
  val ( * ): expr -> expr -> expr

end

module Type: sig
  type 'a t
  type ('a, 'b) either

  val int: int t
  val bool: bool t
  val string: string t
  val abstract: string -> 'a t

  val ( @->): 'a t -> 'b t -> ('a -> 'b) t
  val ( ** ): 'a t -> 'b t -> ('a * 'b) t
  val ( || ): 'a t -> 'b t -> ('a, 'b) either t

  val pp_val: 'a t -> 'a Fmt.t
end

module Var: sig
  type ('a, 'b) t
  val o : ('a * 'b, 'b) t
  val x : unit
  val ( $ ) : ('a, 'b) t -> unit -> ('a * 'c, 'b) t
end

module Expr: sig

  type ('a, 'e) t

  val int: int -> ('a, int) t
  val bool: bool -> ('a, bool) t
  val string: string -> ('a, string) t
  val pair: ('a, 'b) t -> ('a, 'c) t -> ('a, 'b * 'c) t

  val fst: ('a, 'b * 'c) t -> ('a, 'b) t
  val snd: ('a, 'b * 'c) t -> ('a, 'c) t

  val left : ('a, 'b) t -> ('a, ('b, 'c) Type.either) t
  val right: ('a, 'b) t -> ('a, ('c, 'b) Type.either) t

  val var: ('a, 'b) Var.t -> ('a, 'b) t

  val ( = ): ('a, 'b) t -> ('a, 'b) t -> ('a, bool) t
  val ( + ): ('a, int) t -> ('a, int) t -> ('a, int) t
  val ( - ): ('a, int) t -> ('a, int) t -> ('a, int) t
  val ( * ): ('a, int) t -> ('a, int) t -> ('a, int) t

  val if_: ('a, bool) t -> ('a, 'b) t -> ('a, 'b) t -> ('a, 'b) t
  val fix: init:('a, 'b) t -> ('a * 'b, ('b, 'c) Type.either) t -> ('a, 'c) t
  val lambda: 'a Type.t -> ('b * 'a, 'c) t -> ('b, 'a -> 'c) t
  val apply: ('a, 'b -> 'c) t -> ('a, 'b) t -> ('a, 'c) t
  val eval: ('e, 'a) t -> 'e -> 'a
end

type 'a typ = 'a Type.t
type expr
type value

type error
val pp_error: error Fmt.t

module Primitive: sig

  type ('a, 'res) args =
    | []   : ('res, 'res) args
    | (::) : 'a typ * ('k, 'res) args -> ('a -> 'k, 'res) args

end

val ( $ ): ('a -> 'b, 'c) result -> 'a -> ('b, 'c) result

type primitive = string * Untyped.expr

val primitive: string -> ('a, 'b) Primitive.args -> 'b typ -> 'a -> primitive

val parse:
  ?file:string ->
  ?primitives:primitive list ->
  string -> (Untyped.expr, [`Msg of string]) result

val typ: Untyped.expr -> (expr, error) result

val eval: expr -> value

val cast: value -> 'a typ -> 'a option

val type_and_eval: Untyped.expr -> 'a typ -> ('a, error) result
(** typ+eval+cast *)
