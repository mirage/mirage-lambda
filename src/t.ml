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

module Lwt = Higher.Newtype1(Lwt)
let lwt: Lwt.t Eq.witness = Eq.witness "Lwt.t"

type ('l, 'r) either = ('l, 'r) Eq.either =
  | L of 'l
  | R of 'r

type ('a, 'b) app = App of ('a, 'b) Higher.app

type lwt = Lwt.t

type 'a abstract = { eq: 'a Eq.witness; pp: 'a Fmt.t option }

type _ t =
  | Unit    : unit t
  | Int     : int t
  | Int32   : int32 t
  | Int64   : int64 t
  | Bool    : bool t
  | String  : string t
  | Bytes   : bytes t
  | Lwt     : lwt t
  | List    : 'a t -> 'a list t
  | Array   : 'a t -> 'a array t
  | Option  : 'a t -> 'a option t
  | Abstract: 'a abstract -> 'a t
  | Apply   : 'a t * 'b t -> ('a, 'b) app t
  | Arrow   : 'a t * 'b t -> ('a -> 'b) t
  | Pair    : 'a t * 'b t -> ('a * 'b) t
  | Either  : 'a t * 'b t -> ('a, 'b) either t
  | Result  : 'a t * 'b t -> ('a, 'b) result t

let rec equal: type a b. a t -> b t -> (a, b) Eq.refl option = fun a b ->
  match a, b with
  | Unit  , Unit   -> Some Eq.Refl
  | Int   , Int    -> Some Eq.Refl
  | Int32 , Int32  -> Some Eq.Refl
  | Int64 , Int64  -> Some Eq.Refl
  | Bool  , Bool   -> Some Eq.Refl
  | String, String -> Some Eq.Refl
  | Bytes , Bytes  -> Some Eq.Refl
  | Lwt   , Lwt    -> Some Eq.Refl
  | List a  , List b   ->
    (match equal a b with Some Eq.Refl -> Some Eq.Refl | _ -> None)
  | Array a , Array b  ->
    (match equal a b with Some Eq.Refl -> Some Eq.Refl | _ -> None)
  | Option a, Option b ->
    (match equal a b with Some Eq.Refl -> Some Eq.Refl | _ -> None)
  | Abstract a, Abstract b -> Eq.Witness.eq a.eq.wit b.eq.wit
  | Apply (a, a') , Apply (b, b')  ->
    (match equal a b, equal a' b' with
     | Some Eq.Refl, Some Eq.Refl -> Some Eq.Refl
     | _ -> None)
  | Arrow (a, a') , Arrow (b, b')  -> Eq.(equal a b >>= fun Refl -> equal a' b')
  | Either (a, a'), Either (b, b') -> Eq.(equal a b >?= fun Refl -> equal a' b')
  | Result (a, a'), Result (b, b') ->
    (match equal a b, equal a' b' with
     | Some Eq.Refl, Some Eq.Refl -> Some Eq.Refl
     | _ -> None)
  | Pair  (a, a') , Pair  (b, b')  -> Eq.(equal a b >&= fun Refl -> equal a' b')
  | Int , _ -> None | Bool, _ -> None | String, _ -> None
  | Abstract _, _ -> None | Arrow _, _ -> None | Either _, _ -> None
  | Pair _, _ -> None | Unit, _ -> None | Lwt, _ -> None
  | Apply _, _ -> None | Int32, _ -> None | Int64, _ -> None
  | List _, _ -> None | Array _, _ -> None | Option _, _ -> None
  | Result _, _ -> None
  | Bytes, _ -> None
