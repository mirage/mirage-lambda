(*
 * Copyright (c) 2018 Thomas Gazagnaire <thomas@gazagnaire.org>
 * and Romain Calascibetta <romain.calascibetta@gmail.com>
 * and Frédéric Bour <frederic.bour(_)lakaban.net>
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

type ('l, 'r) either =
  | L of 'l
  | R of 'r

type (_, _) refl = Refl : ('a, 'a) refl

let trans
  : type m n p. (m, n) refl -> (n, p) refl -> (m, p) refl
  = fun Refl Refl -> Refl

let arrow
  : type a b c d. (a, b) refl -> (c, d) refl option ->
    (a -> c, b -> d) refl option
  = fun Refl -> function
    | Some Refl -> Some Refl
    | None -> None

let pair
  : type a b c d. (a, b) refl -> (c, d) refl option ->
    (a * c, b * d) refl option
  = fun Refl -> function
    | Some Refl -> Some Refl
    | None -> None

let either
  : type a b c d. (a, b) refl -> (c, d) refl option ->
    ((a, c) either, (b, d) either) refl option
  = fun Refl -> function
    | Some Refl -> Some Refl
    | None -> None

let ( >>= )

  : type a b c d.
    (a, b) refl option
    -> ((a, b) refl
        -> (c, d) refl option)
    -> (a -> c, b -> d) refl option

  = fun x f -> match x with
    | Some (Refl as x) -> arrow x (f x)
    | None -> None

let ( >&= )

  : type a b c d.
    (a, b) refl option
    -> ((a, b) refl
        -> (c, d) refl option)
    -> (a * c, b * d) refl option

  = fun x f -> match x with
    | Some (Refl as x) -> pair x (f x)
    | None -> None

let ( >*= )

  : type a b c d.
    (a, b) refl option
    -> ((a, b) refl
        -> (c, d) refl option)
    -> (a * c, b * d) refl option

  = fun x f -> match x with
    | Some (Refl as x) -> pair x (f x)
    | None -> None

let ( >?= )

  : type a b c d.
    (a, b) refl option
    -> ((a, b) refl
        -> (c, d) refl option)
    -> ((a, c) either, (b, d) either) refl option

  = fun x f -> match x with
    | Some (Refl as x) -> either x (f x)
    | None -> None

module Witness : sig
  type 'a t
  val v : unit -> 'a t
  val eq: 'a t -> 'b t -> ('a, 'b) refl option
end = struct

  type _ equality = ..

  module type Inst = sig
    type t
    type _ equality += Eq : t equality
  end

  type 'a t = (module Inst with type t = 'a)

  let v: type a. unit -> a t = fun () ->
    let module Inst = struct
      type t = a
      type _ equality += Eq : t equality
    end
    in
    (module Inst)

  let eq: type a b. a t -> b t -> (a, b) refl option =
    fun (module A) (module B) ->
      match A.Eq with
      | B.Eq -> Some Refl
      | _    -> None

end

type 'a witness = { name: string; wit : 'a Witness.t }

let witness name =
  let wit = Witness.v () in
  {name; wit}
