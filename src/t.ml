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

type ('l, 'r) either = ('l, 'r) Eq.either =
  | L of 'l
  | R of 'r

type ('a, 'b) app = ('a, 'b) Higher.app

type lwt = Higher.Lwt.t

type _ t =
  | Unit    : unit t
  | Int     : int t
  | Bool    : bool t
  | String  : string t
  | Lwt     : lwt t
  | Apply   : 'a t * 'b t -> ('a, 'b) app t
  | Abstract: 'b params * 'a Eq.witness-> 'a t
  | Arrow   : 'a t * 'b t -> ('a -> 'b) t
  | Pair    : 'a t * 'b t -> ('a * 'b) t
  | Either  : 'a t * 'b t -> ('a, 'b) either t

and 'a params =
  | Empty: unit params
  | Cons: 'a t * 'b params -> ('a * 'b) params

let rec eq: type a b. a t -> b t -> (a, b) Eq.refl option = fun a b ->
  match a, b with
  | Unit  , Unit   -> Some Eq.Refl
  | Int   , Int    -> Some Eq.Refl
  | Bool  , Bool   -> Some Eq.Refl
  | String, String -> Some Eq.Refl
  | Lwt   , Lwt    -> Some Eq.Refl
  | Apply (a, a') , Apply (b, b')  ->
    (match eq a b, eq a' b' with
     | Some Eq.Refl, Some Eq.Refl -> Some Eq.Refl
     | _ -> None)
  | Abstract (a, a'), Abstract (b, b') ->
    (match eq_params a b with
     | Some Eq.Refl -> Eq.Witness.eq a'.wit b'.wit
     | None -> None)
  | Arrow (a, a') , Arrow (b, b')  -> Eq.(eq a b >>= fun Refl -> eq a' b')
  | Either (a, a'), Either (b, b') -> Eq.(eq a b >?= fun Refl -> eq a' b')
  | Pair  (a, a') , Pair  (b, b')  -> Eq.(eq a b >&= fun Refl -> eq a' b')
  | Int , _ -> None | Bool, _ -> None | String, _ -> None
  | Abstract _, _ -> None | Arrow _, _ -> None | Either _, _ -> None
  | Pair _, _ -> None | Unit, _ -> None | Lwt, _ -> None
  | Apply _, _ -> None

and eq_params:
  type a b. a params -> b params -> (a, b) Eq.refl option = fun a b ->
  match a, b with
  | Empty      , Empty       -> Some Eq.Refl
  | Cons (a, b), Cons (c, d) ->
    (match eq a c, eq_params d b with
     | Some Eq.Refl, Some Eq.Refl -> Some Eq.Refl
     | _ -> None)
  | Empty, _ -> None | Cons _, _ -> None
