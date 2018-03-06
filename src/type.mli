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

(** Runtime representation of types. *)

type 'a t
(** The type for runtime representation of values of type ['a]. *)

(** {1:primitives Primitives} *)

val unit: unit t
(** [unit] is a representation of the unit type. *)

val bool: bool t
(** [bool] is a representation of the boolean type. *)

val char: char t
(** [char] is a representation of the character type. *)

val int: int t
(** [int] is a representation of the integer type. *)

val int32: int32 t
(** [int32] is a representation of the 32-bit integers type. *)

val int64: int64 t
(** [int64] is a representation of the 64-bit integer type. *)

val float: float t
(** [float] is a representation of the float type. *)

val string: string t
(** [string] is a representation of the string type. *)

val list: 'a t -> 'a list t
(** [list t] is a representation of list of values of type [t]. *)

val array: 'a t -> 'a array t
(** [array t] is a representation of array of values of type [t]. *)

val option: 'a t -> 'a option t
(** [option t] is a representation of value of type [t option]. *)

val pair: 'a t -> 'b t -> ('a * 'b) t
(** [pair x y] is a representation of values of type [x * y]. *)

val triple: 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t
(** [triple x y z] is a representation of values of type [x * y *
    z]. *)

val (@->): 'a t -> 'b t -> ('a -> 'b) t
(** [a @-> b] is a representation of functions of type [a -> b]. *)

val (!!): 'a t lazy_t -> 'a t
(** [!!f] is useful to define cyclic types:
    {[
      type t = Nil | Cons of (int * t)

      let rec t = lazy (variant [case0 Nil; case1 (pair int !!t)])
      let t = !!t
    ]}
    *)

(** {1:records Records} *)

type 'a field
(** The type for the fields of a record of type ['a]. *)

val field: 'b t ->  ('a -> 'b) -> 'a field
(** [field t g] is the representation of a field of type [t]
    with getter [g].

    For instance:

    {[
      type t = { foo: string option }

      let foo = field (option string) (fun t -> t.x)]}
*)

val record: 'a field list -> 'a t
(** [record f] is a representation of a record type with fields
    [f].

    {[
      type t = { foo: string option; bar: int }

      let t = record [
          field (option string) (fun x -> x.foo);
          field int             (fun x -> x.bar);
        ]
    ]}
*)

(** {1:variants Variants} *)

type 'a case
(** The type for the cases of a variant of type ['a]. *)

val case0: 'a -> 'a case
(** [case0 v] is a representation of a variant case with no argument:

    {[
      type t = Foo

      let foo = case0 Foo
    ]}
*)

val case1: 'b t -> ('b -> 'a) -> 'a case
(** [case1 t c] is a representation of a variant case with 1
    argument of type [t] and a pattern [c] an function with one argument
    of type [t]. e.g.

    {[
      type t = Foo of string

      let foo = case1 string (fun s -> Foo s)
    ]}
*)

val case2: 'b t -> 'c t -> ('b * 'c -> 'a) -> 'a case
val case3: 'b t -> 'c t -> 'd t -> ('b * 'c * 'd -> 'a) -> 'a case

val variant: 'a case list -> 'a t
(** [variant t] is a representation of a variant type containing
    the cases [c]:

    Putting all together:

    {[
      type t = Foo | Bar of string

      let t = variant [
          case0         Foo;
          case1 string (fun x -> Bar x);
        ]
    ]}
*)

val enum: 'a list -> 'a t
(** [enum c] is [variant (List.map case0 c)]. *)

val pp: 'a t Fmt.t
(** [pp] is the pretty-printer for runtime types. *)

(** {1 Type Equality} *)

type (_, _) eq = Refl: ('a, 'a) eq

val eq: 'a t -> 'b t -> ('a, 'b) eq option
