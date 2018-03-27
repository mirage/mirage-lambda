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
  | Error (`Msg e)            -> err e
  | exception Lexer.Error msg -> err msg
  | exception Parser.Error    -> err "syntax error"

(* Typer *)

module Untyped = Untyped
module Type = Typed.Type
module Var = Typed.Var
module Expr = Typed.Expr

type 'a typ = 'a Typed.Type.t
type expr = Typed.Expr.e
type value = Typed.Expr.v

type error = Typed.Expr.error
let pp_error = Typed.Expr.pp_error

let ( $ ) f x = match f with Ok f -> Ok (f x) | Error e -> Error e

let eval e =
  let open Typed.Expr in
  let E (m, t) = e in
  V (eval m (), t)

let cast: type a. value -> a typ -> a option = fun v t' ->
  let Typed.Expr.V (v, t) = v in
  match Typed.Type.eq t t' with
  | Some Eq.Refl -> Some v
  | None         -> None

let typ = Typed.Expr.typ

let type_and_eval:
  type a. Untyped.expr -> a typ -> (a, error) result = fun m t' ->
  match Typed.Expr.typ m with
  | Error _ as e -> e
  | Ok e         ->
    let open Typed in
    let Expr.V (v, t) = eval e in
    match Type.eq t t' with
    | Some Eq.Refl -> Ok v
    | None   -> Error (m, [], [ TypMismatch { a = Type.V t; b = Type.V t' } ])

module Primitive = struct

  type ('a, 'res) args =
    | []   : ('res, 'res) args
    | (::) : 'a typ * ('k, 'res) args -> ('a -> 'k, 'res) args

  type ('f, 'a) t = {
    name  : string;
    args  : ('f, 'a) args;
    output: 'a typ;
    body  : 'f;
  }

  let v name args output body = { name; args; output; body }

  let untype_args args =
    let rec aux:
      type a b. Untyped.typ list -> (a, b) args -> Untyped.typ list
      = fun acc -> function
        | []   -> List.rev acc
        | h::t -> aux (Typed.Type.untype h :: acc) t
    in
    aux args

  let rec apply:
    type a b. (a, b) args -> a -> Untyped.param list -> b * (b, b) args
    = fun args f params ->
      match args, params with
      | []  , []   -> f, []
      | a::b, h::t -> apply b (f @@ Typed.Param.cast h a) t
      | _          -> failwith "invalid arity"

  let untype v =
    let inputs = untype_args [] v.args in
    Untyped.primitive v.name inputs (Typed.Type.untype v.output) (fun l ->
        let r, _ = apply v.args v.body l in
        Typed.Param.untype v.output r
      )
end

type primitive = string * Untyped.expr

let primitive name args out f =
  name, Primitive.(v name args out f |> untype)
