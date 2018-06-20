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

  (** Arguments of a primitive. *)
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
val stop: Parsetree.typ -> Parsetree.typ -> string * Parsetree.expr

module L: sig

  (** Lwt primitive. *)

  type ('f, 'a) t

  val v: string ->
    ('a, 'b Lwt.t) Args.t -> ('b, Type.lwt) Type.app typ -> 'a -> ('a, 'b) t
  (** [v args ty f] makes an Lwt primitive which take [args] and return a Lwt
     [value : ty]. *)

  val untype_args: Parsetree.typ list -> ('a, 'b) Args.t -> Parsetree.typ list
  (** [untype_args acc types] puts successively untype element of [types] to
     [acc] and reverse [acc] at the end of process. *)

  val apply: ('a, 'b) Args.t -> 'a -> Parsetree.value list -> 'b * ('b, 'b) Args.t
  (** [apply types_of_args f args] applies successively [f] with values [args]
     which follow types [types_of_args]. If one of [args] does not have expected
     type of [types_of_args], we raise [Invalid_argument]. If length of [args]
     is not equal to length of [types_of_args], we raise [Failure]. *)

  val untype: ('a, 'b) t -> Parsetree.expr
  (** [untype p] transforms a typed primitive [t] to an un-typed primitives. *)

end
