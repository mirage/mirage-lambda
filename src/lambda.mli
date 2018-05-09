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

(** Type of errors. *)
type error

(** Pretty-printer of {!error}. *)
val pp_error: error Fmt.t

module Args: sig

  (** Arguments of a primitive. *)
  type ('a, 'res) t =
    | []   : ('res, 'res) t
    | (::) : 'a typ * ('k, 'res) t -> ('a -> 'k, 'res) t

end

val ( $ ): ('a -> 'b, 'c) result -> 'a -> ('b, 'c) result

type primitive = string * Parsetree.expr

val primitive: string -> ('a, 'b) Args.t -> 'b typ -> 'a -> primitive
(** [primitives name types_of_args type_of_res f] records a new primitive with
   [name]. *)

val parse:
  ?file:string ->
  ?primitives:primitive list ->
  string -> (Parsetree.expr, [`Msg of string]) result
(** [parse ?file ?primitives input] tries to parse [input] and binds primitives
   with their associated names (see {!primitive}) in resulted {!Parsetree.expr}
   expression. [?file] helps to produce a better error message. *)

val typ: Parsetree.expr -> (expr, error) result
(** [typ unsafe_expr] tries to type [unsafe_expr]. *)

val eval: expr -> value
(** [eval safe_expr] safely evals [safe_expr] and returns resulting computation.
   *)

val cast: value -> 'a typ -> 'a option
(** [cast v typ] unwraps value [v] and proves type of it is equivalent to [ty].
   *)

val type_and_eval: Parsetree.expr -> 'a typ -> ('a, error) result
(** [type_and_eval unsafe_expr ty] tries to type [unsafe_expr] to [ty], evals it
   safely and returns resulting computation. *)

(** {1 Lwt} *)

module L: sig
  val primitive:
    string -> ('a, 'b Lwt.t) Args.t -> ('b, Type.lwt) Type.app typ ->
    'a -> primitive
  (** [primitives name types_of_args type_of_res f] records a new LWT primitive.
     *)

  val cast: value -> ('a, Type.lwt) Type.app typ -> 'a Lwt.t option
  (** [cast v typ] unwraps LWT value [v] and proves type of it is equivalent to
     [ty]. *)

  val type_and_eval:
    Parsetree.expr -> ('a, Type.lwt) Type.app typ -> ('a Lwt.t, error) result
    (** [type_and_eval unsafe_lwt_expr ty] tries to type [unsafe_lwt_expr] to
       [ty], evals it under LWT context safely and returns resulting
       computation. *)
end
