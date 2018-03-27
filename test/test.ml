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

let reporter ?(prefix="") () =
  let report src level ~over k msgf =
    let k _ = over (); k () in
    let ppf = match level with Logs.App -> Fmt.stdout | _ -> Fmt.stderr in
    let with_stamp h _tags k fmt =
      Fmt.kpf k ppf ("%s %a %a @[" ^^ fmt ^^ "@]@.")
        prefix
        Fmt.(styled `Magenta string) (Logs.Src.name src)
        Logs_fmt.pp_header (level, h)
    in
    msgf @@ fun ?header ?tags fmt ->
    with_stamp header tags k fmt
  in
  { Logs.report = report }

let () =
  Logs.set_level (Some Logs.Debug);
  Logs.set_reporter (reporter ());
  Printexc.record_backtrace true

open Lambda

let uexpr = Alcotest.testable Untyped.pp (=)
let error = Alcotest.testable pp_error (=)
let ok x = Alcotest.result x error

let test_if () =
  let x = Untyped.(if_ true_ (int 42) (int 21)) in
  Alcotest.(check @@ ok int) "if" (Ok 42) (type_and_eval x Type.int);
  Alcotest.(check @@ neg @@ ok string) "failure" (Ok "")
    (type_and_eval x Type.string)

let test_match () =
  let x =
    let open Untyped in
    match_ (left Type.int (string "Hello World!"))
      (var 0)
      (string "<int>")
  in
  Alcotest.(check @@ ok string) "match"
    (Ok "Hello World!")
    (type_and_eval x Type.string)


let test_lambda () =
  let x =
    let open Untyped in
    apply (
      apply (
        lambda Type.int "x" (
          lambda Type.int "y" (var 1))
      ) (int 42)
    ) (int 21)
  in
  Alcotest.(check @@ ok int) "lambda" (type_and_eval x Type.int) (Ok 42)

(*
let fix_example: (unit * int, _) expr =
  let open Expr in
  fix ~init:(var Var.o)
    (if_ (int 123 = var Var.o)
       (left (var Var.o + var Var.o))
       (right (var Var.o)))
*)

let test_fact () =

  let code =
    let init x = ((fun _ -> 1), x) in
    let rec fact v =
      if 0 = (snd v)
      then (fst v, 0)
      else fact ((fun x -> (fst v) x * (snd v)), (snd v) - 1)
    in
    fun x -> (fst ((fun x -> fact (init x)) x)) 0
  in
  Alcotest.(check int) "code" 120 (code 5);

  let safe =
    let open Expr in
    let main =
      fix ~init:(pair (lambda Type.int (int 1)) (var Var.o))
        (if_
           (snd (var Var.o) = int 0)
           (right (pair (fst (var Var.o)) (int 0)))
           (left  (pair
                     (lambda Type.int
                        (apply (fst (var Var.(o$x))) (var Var.o)
                         * snd (var Var.(o$x))))
                     (snd (var Var.o) - int 1))))
    in
    lambda Type.int
      (apply (fst (apply (lambda Type.int main) (var Var.o))) (int 0))
  in
  Alcotest.(check @@ int) "safe" 120 (Expr.eval safe () 5);

  let unsafe =
    let open Untyped in
    let main =
      let typ = Type.((int @-> int) ** int) in
      fix ~typ ~init:(pair (lambda Type.int "x" (int 1)) (var 0))
        (if_
           (snd (var 0) = int 0)
           (right typ (pair (fst (var 0)) (int 0)))
           (left typ
              (pair
                 (lambda Type.int "y"
                    ((apply (fst (var 1)) (var 0)) * (snd (var 1))))
                 (snd (var 0) - int 1))))
    in
    lambda Type.int "x"
      (apply (fst (apply (lambda Type.int "y" main) (var 0))) (int 0))
  in
  Alcotest.(check @@ ok int) "unsafe" (Ok 120)
    (type_and_eval unsafe Type.(int @-> int) $ 5)

let test_prim () =
  let _, padd = primitive "%add" [Type.int; Type.int] Type.int (+) in
  Alcotest.(check @@ ok int) "padd" (Ok 42)
    (type_and_eval padd Type.(int @-> int @-> int) $ 21 $ 21);

  let _, padebool =
    primitive "%add-bool" [Type.int; Type.bool] Type.int (fun a -> function
        | true  -> a + 1
        | false -> a
      ) in
  Alcotest.(check @@ ok int) "padebool" (Ok 1)
    (type_and_eval padebool Type.(int @-> bool @-> int) $ 0 $ true);

  let env_and_prim =
    let open Untyped in
    (lambda Type.string "x" (apply (apply padd (int 21)) (int 21)))
  in

  Alcotest.(check @@ ok int) "env_and_prim safe" (Ok 42)
    (type_and_eval env_and_prim Type.(string @-> int) $ "Hello World!");
  Alcotest.(check @@ ok int) "env_and_prim unsafe" (Ok 42)
    (type_and_eval
       Untyped.(lambda Type.int "x" env_and_prim)
       Type.(int @-> string @-> int)
     $ 0 $ "Hello World!")

let parse_exn ?primitives s = match parse ?primitives s with
  | Ok e -> e
  | Error (`Msg e) -> Alcotest.fail e

let test_parse_expr () =
  let check s (a, t) r =
    let e = parse_exn s in
    Alcotest.(check @@ ok a) ("parse: " ^ s) (Ok r) (type_and_eval e t);
    let s' = Fmt.to_to_string Untyped.pp e in
    Logs.debug (fun l -> l "roundtrip: %s => %s" s s');
    let e' = parse_exn s' in
    Alcotest.(check @@ uexpr) ("roundtrip: " ^ s') e e'
  in
  let int = (Alcotest.int, Type.int) in
  let bool = (Alcotest.bool, Type.bool) in
  check "1 + 1 + 1" int 3;
  check "1 + 1 * 3" int 4;
  check "1 + 1 = 2" bool true;
  check "(1 = 2)" bool false;
  check "(fun (x:int) -> x + 1) 1" int 2;
  check "(fun (x:int) -> fun (x:bool) -> x) 1 false" bool false;
  check {|
    (fun (f:int -> int) -> (fun (k:int) -> f$1 k$0))
      (fun (x:int) -> x$0 + 1)
      2
  |} int 3

let test_ping () =
  let app t f x =
    let f = parse_exn f in
    let x = parse_exn x in
    match type_and_eval Untyped.(apply f x) t with
    | Ok x    -> Fmt.to_to_string (Type.pp_val t) x
    | Error e -> Fmt.failwith "%a" pp_error e
  in
  Alcotest.(check string) "ping" "20"
    (app Type.int "(fun (x:int) -> x * 2)" "10")

let test_primitives () =
  let primitives = [
    primitive "string_of_int" [Type.int] Type.string string_of_int
  ] in
  Alcotest.(check @@ ok string) "safe" (Ok "10")
    (type_and_eval (parse_exn ~primitives "string_of_int 10") Type.string)

module Lwt: sig
  type +'a t
  (*    val (>>=): 'a t -> ('a -> 'b) t -> 'b t *)
  val return: 'a -> 'a t
end = struct
  type 'a t = 'a
  (*    let (>>=) x f = f x *)
  let return x = x
end

module Block: sig
  type t
  val pp: t Fmt.t
  type error
  val connect: string -> t
  val read: t -> int64 -> string list -> (unit, error) result Lwt.t
end = struct
  type error = [ `Foo ]
  type t = C of string
  let pp ppf (C t) = Fmt.pf ppf "(C %S)" t
  let connect n = C n

  let read (C n) off pages =
    Logs.debug (fun l ->
        l "READ[%s] off=%Ld pages=%a" n off Fmt.(Dump.list string) pages);
    if Random.int 3 = 0 then Lwt.return (Error `Foo) else Lwt.return (Ok ())
end

let test_block () =
  let t = Type.abstract "Block.t" in
  let primitives = [
    primitive "Block.connect" [Type.string] t Block.connect;
    primitive "Block.to_string" [t] Type.string (Fmt.to_to_string Block.pp);
  ] in
  let t_t = Alcotest.testable Block.pp (=) in
  Alcotest.(check @@ ok t_t) "Block.connect"
    (Ok (Block.connect "foo"))
    (type_and_eval (parse_exn ~primitives "Block.connect \"foo\"") t);
  Alcotest.(check @@ ok string) "compose"
    (Ok "(C \"foo\")")
    (type_and_eval
       (parse_exn ~primitives "Block.to_string (Block.connect \"foo\")")
       Type.string);

  let _ = Block.read in
  ()

let () =
  Alcotest.run "compute" [
    "basic", [
      "if"    , `Quick, test_if;
      "match" , `Quick, test_match;
      "lambda", `Quick, test_lambda;
    ];
    "fonctions", [
      "fact"     ,  `Quick, test_fact;
      "primitives", `Quick, test_prim;
    ];
    "parsing", [
      "expr", `Quick, test_parse_expr;
      "ping", `Quick, test_ping;
    ];
    "primitives", [
      "simple"  , `Quick, test_primitives;
      "abstract", `Quick, test_block;
    ];
  ]
