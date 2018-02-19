(*
 * Copyright (c) 2018 Thomas Gazagnaire <thomas@gazagnaire.org>
 * Copyright (c) 2018 Seagate
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

(** Safe manipulation of static pointers in OCaml. Inspired from
    Haskel's distributed closure library. *)

module Type = Type
(** Runtime types. *)

val dump: 'a Type.t -> 'a Fmt.t
(** [dump t v] pretty-prints the static pointer [v], using the type
    witness [t]. That representation can be parsed back by any other
    instance of the same program, for instance on a different/remote
    machine. *)

val dump_string: 'a Type.t -> 'a -> string
(** [dump_string t v] is [Fmt.to_to_string (dump t) v]. *)

val parse: 'a Type.t -> string -> ('a, [`Msg of string]) result
(** [parse t s] is [Ok v] iff [pp t v] generates s. *)
