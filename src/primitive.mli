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

open Typedtree

module Args: sig

  type ('a, 'res) t =
    | []   : ('res, 'res) t
    | (::) : 'a typ * ('k, 'res) t -> ('a -> 'k, 'res) t

end

type ('f, 'a) t
val v: string -> ('a, 'b) Args.t -> 'b typ -> 'a -> ('a, 'b) t
val untype_args: Parsetree.typ list -> ('a, 'b) Args.t -> Parsetree.typ list
val apply: ('a, 'b) Args.t -> 'a -> Parsetree.value list -> 'b * ('b, 'b) Args.t
val untype: ('a, 'b) t -> Parsetree.expr

val continue: Parsetree.typ -> Parsetree.typ -> string * Parsetree.expr
val return: Parsetree.typ -> Parsetree.typ -> string * Parsetree.expr

module L: sig

  (** Lwt *)

  type ('f, 'a) t

  val v: string ->
    ('a, 'b Lwt.t) Args.t -> ('b, Type.lwt) Type.app typ -> 'a -> ('a, 'b) t
  val untype_args: Parsetree.typ list -> ('a, 'b) Args.t -> Parsetree.typ list
  val apply: ('a, 'b) Args.t -> 'a -> Parsetree.value list -> 'b * ('b, 'b) Args.t
  val untype: ('a, 'b) t -> Parsetree.expr

end
