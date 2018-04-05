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

module Args = struct

  type ('a, 'res) t =
    | []   : ('res, 'res) t
    | (::) : 'a typ * ('k, 'res) t -> ('a -> 'k, 'res) t

end

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
    | a::b, h::t -> apply b (f @@ Value.cast h a) t
    | _          -> failwith "invalid arity"

let untype v =
  let inputs = untype_args [] v.args in
  Parsetree.primitive v.name inputs (Type.untype v.output) (fun l ->
      let r, _ = apply v.args v.body l in
      Value.untype v.output r
    )

let continue l r =
  let Type.V l = Type.typ l in
  let Type.V r = Type.typ r in
  let ty = Type.either l r in
  let f x = T.L x in
  let n = "continue" in
  n, (v n Args.[l] ty f |> untype)

let return l r =
  let Type.V l = Type.typ l in
  let Type.V r = Type.typ r in
  let ty = Type.either l r in
  let f x = T.R x in
  let n = "return" in
  n, (v n Args.[r] ty f |> untype)

module L = struct

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
      | a::b, h::t -> apply b (f @@ Value.cast h a) t
      | _          -> failwith "invalid arity"

  let untype v =
    let inputs = untype_args [] v.args in
    Parsetree.primitive v.name inputs (Type.untype v.output) (fun l ->
        let r, _ = apply v.args v.body l in
        Value.untype v.output (Higher.Lwt.inj r)
      )
end
