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

let src = Logs.Src.create "lambda"
module Log = (val Logs.src_log src : Logs.LOG)

(* Parser *)

let pp_position file ppf lexbuf =
  let p = Lexing.lexeme_start_p lexbuf in
  Fmt.pf ppf
    "File \"%s\", line %d, character %d"
    file p.Lexing.pos_lnum
    (p.Lexing.pos_cnum - p.Lexing.pos_bol)

let parse ?(file="<none>") ?(primitives=[]) str =
  let lexbuf = Lexing.from_string str in
  let err msg =
    let msg = Fmt.strf "%a: %s\n.%!" (pp_position file) lexbuf msg in
    Log.err (fun l -> l "%s" msg);
    Error (`Msg msg)
  in
  match Parser.main Lexer.(token @@ v ()) lexbuf primitives with
  | Ok _ as x                 -> x
  | Error e                   -> err e
  | exception Lexer.Error msg -> err msg
  | exception Parser.Error    -> err "syntax error"

let parse_exn ?file ?primitives str =
  match parse ?file ?primitives str with
  | Ok y           -> y
  | Error (`Msg e) -> invalid_arg e

(* Typer *)

module Parsetree = Parsetree
module Fuzzer = Fuzzer
module Protobuf = Protobuf

module Type = Typedtree.Type
module Var = Typedtree.Var
module Expr = Typedtree.Expr

let typ = Expr.typ

let typ_exn e = match typ e with
  | Ok t    -> t
  | Error e -> Fmt.kstrf invalid_arg "%a" Expr.pp_error e

let untype (Expr.V (e, _)) = Expr.untype e

type 'a typ = 'a Type.t
type expr = Typedtree.expr
type value = Typedtree.value

let pp_value = Typedtree.pp_value
let string_of_value = Fmt.to_to_string pp_value

type error = Expr.error
let pp_error = Expr.pp_error

let ( $ ) f x = match f with Ok f -> Ok (f x) | Error e -> Error e

let eval e =
  let open Expr in
  let Typedtree.Expr.V (m, t) = e in
  Typedtree.V (eval m (), t)

let cast: type a. value -> a typ -> a option = fun v t' ->
  let Typedtree.V (v, t) = v in
  match Type.equal t t' with
  | Some Eq.Refl -> Some v
  | None         -> None

let type_and_eval:
  type a. Parsetree.expr -> a typ -> (a, error) result = fun m t' ->
  match Typedtree.Expr.typ m with
  | Error _ as e -> e
  | Ok e         ->
    let Typedtree.V (v, t) = eval e in
    match Type.equal t t' with
    | Some Eq.Refl -> Ok v
    | None   -> Typedtree.err_type_mismatch m t t'

(*
let untype: expr -> Parsetree.expr = fun e ->
  let Typedtree.E (m, t) = e in
  Typed
*)

type primitive = string * Parsetree.expr

module Args = Primitive.Args

let primitive name args out f =
  name, Primitive.(v name args out f |> untype)

module L = struct

  let primitive name args out f =
    name, Primitive.L.(v name args out f |> untype)

  let cast
    : type a. value -> (a, Type.lwt) Type.app typ -> a Lwt.t option
    = fun v t' ->
      let Typedtree.V (v, t) = v in
      match Type.equal t t' with
      | Some Eq.Refl -> let Type.App v = v in Some (Type.Lwt.prj v)
      | None         -> None

  let type_and_eval
    : type a. Parsetree.expr -> (a, Type.lwt) Type.app typ ->
      (a Lwt.t, error) result
    = fun m t' ->
    match Typedtree.Expr.typ m with
    | Error _ as e -> e
    | Ok e         ->
      let Typedtree.V (v, t) = eval e in
      match Type.equal t t' with
      | Some Eq.Refl -> let Type.App v = v in Ok (Type.Lwt.prj v)
      | None         -> Typedtree.err_type_mismatch m t t'

end
