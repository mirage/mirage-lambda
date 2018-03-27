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

module Parsetree = Parsetree

module Type = Typedtree.Type
module Var = Typedtree.Var
module Expr = Typedtree.Expr

type 'a typ = 'a Typedtree.typ
type expr = Typedtree.expr
type value = Typedtree.v

type error
val pp_error: error Fmt.t

module Args: sig

  type ('a, 'res) t =
    | []   : ('res, 'res) t
    | (::) : 'a typ * ('k, 'res) t -> ('a -> 'k, 'res) t

end

val ( $ ): ('a -> 'b, 'c) result -> 'a -> ('b, 'c) result

type primitive = string * Parsetree.expr

val primitive: string -> ('a, 'b) Args.t -> 'b typ -> 'a -> primitive

val parse:
  ?file:string ->
  ?primitives:primitive list ->
  string -> (Parsetree.expr, [`Msg of string]) result

val typ: Parsetree.expr -> (expr, error) result

val eval: expr -> value

val cast: value -> 'a typ -> 'a option

val type_and_eval: Parsetree.expr -> 'a typ -> ('a, error) result
(** typ+eval+cast *)

(** {1 Lwt} *)

module L: sig
  val primitive:
    string -> ('a, 'b Lwt.t) Args.t -> ('b, Type.lwt) Type.app typ ->
    'a -> primitive

  val cast: value -> ('a, Type.lwt) Type.app typ -> 'a Lwt.t option

  val type_and_eval:
    Parsetree.expr -> ('a, Type.lwt) Type.app typ -> ('a Lwt.t, error) result
end
