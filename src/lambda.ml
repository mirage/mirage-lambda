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

(* Typer *)

module Parsetree = Parsetree

module Type = Typedtree.Type
module Var = Typedtree.Var
module Expr = Typedtree.Expr

let typ = Typedtree.typ

type 'a typ = 'a Type.t
type expr = Typedtree.expr
type value = Typedtree.v

type error = Typedtree.error
let pp_error = Typedtree.pp_error

let ( $ ) f x = match f with Ok f -> Ok (f x) | Error e -> Error e

let eval e =
  let open Expr in
  let Typedtree.E (m, t) = e in
  Typedtree.V (eval m (), t)

let cast: type a. value -> a typ -> a option = fun v t' ->
  let Typedtree.V (v, t) = v in
  match Type.eq t t' with
  | Some Eq.Refl -> Some v
  | None         -> None

let type_and_eval:
  type a. Parsetree.expr -> a typ -> (a, error) result = fun m t' ->
  match Typedtree.typ m with
  | Error _ as e -> e
  | Ok e         ->
    let Typedtree.V (v, t) = eval e in
    match Type.eq t t' with
    | Some Eq.Refl -> Ok v
    | None   -> Typedtree.err_type_mismatch m t t'

module Args = struct

  type ('a, 'res) t =
    | []   : ('res, 'res) t
    | (::) : 'a typ * ('k, 'res) t -> ('a -> 'k, 'res) t

end

module Primitive = struct

  type ('f, 'a) t = {
    name  : string;
    args  : ('f, 'a) Args.t;
    output: 'a typ;
    body  : 'f;
  }

  let v name args output body = { name; args; output; body }

  let untype_args args =
    let open Args in
    let rec aux:
      type a b. Parsetree.typ list -> (a, b) Args.t -> Parsetree.typ list
      = fun acc -> function
        | []   -> List.rev acc
        | h::t -> aux (Type.untype h :: acc) t
    in
    aux args

  let rec apply:
    type a b. (a, b) Args.t -> a -> Parsetree.value list -> b * (b, b) Args.t
    = fun args f params ->
      let open Args in
      match args, params with
      | []  , []   -> f, []
      | a::b, h::t -> apply b (f @@ Typedtree.Value.cast h a) t
      | _          -> failwith "invalid arity"

  let untype v =
    let inputs = untype_args [] v.args in
    Parsetree.primitive v.name inputs (Type.untype v.output) (fun l ->
        let r, _ = apply v.args v.body l in
        Typedtree.Value.untype v.output r
      )
end

module Primitive_lwt = struct

  (* XXX(samoht): there's probably a better way then to duplicate the
     module, but I didn't find it. *)

  type ('f, 'a) t = {
    name  : string;
    args  : ('f, 'a Lwt.t) Args.t;
    output: ('a, Type.lwt) Type.app typ;
    body  : 'f;
  }

  let v name args output body = { name; args; output; body }

  let untype_args args =
    let open Args in
    let rec aux:
      type a b. Parsetree.typ list -> (a, b) Args.t -> Parsetree.typ list
      = fun acc -> function
        | []   -> List.rev acc
        | h::t -> aux (Type.untype h :: acc) t
    in
    aux args

  let rec apply:
    type a b. (a, b) Args.t -> a -> Parsetree.value list -> b * (b, b) Args.t
    = fun args f params ->
      let open Args in
      match args, params with
      | []  , []   -> f, []
      | a::b, h::t -> apply b (f @@ Typedtree.Value.cast h a) t
      | _          -> failwith "invalid arity"

  let untype v =
    let inputs = untype_args [] v.args in
    Parsetree.primitive v.name inputs (Type.untype v.output) (fun l ->
        let r, _ = apply v.args v.body l in
        Typedtree.Value.untype v.output (Higher.Lwt.inj r)
      )
end

type primitive = string * Parsetree.expr

let primitive name args out f =
  name, Primitive.(v name args out f |> untype)

module L = struct

  let primitive name args out f =
    name, Primitive_lwt.(v name args out f |> untype)

  let cast
    : type a. value -> (a, Type.lwt) Type.app typ -> a Lwt.t option
    = fun v t' ->
      let Typedtree.V (v, t) = v in
      match Type.eq t t' with
      | Some Eq.Refl -> Some (Higher.Lwt.prj v)
      | None         -> None

  let type_and_eval
    : type a. Parsetree.expr -> (a, Type.lwt) Type.app typ ->
      (a Lwt.t, error) result
    = fun m t' ->
    match Typedtree.typ m with
    | Error _ as e -> e
    | Ok e         ->
      let Typedtree.V (v, t) = eval e in
      match Type.eq t t' with
      | Some Eq.Refl -> Ok (Higher.Lwt.prj v)
      | None   -> Typedtree.err_type_mismatch m t t'

end
