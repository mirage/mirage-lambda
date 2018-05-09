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

let pexpr = Alcotest.testable Parsetree.pp Parsetree.equal
let error = Alcotest.testable pp_error (=)
let ok x = Alcotest.result x error

let parse_exn ?primitives e =
  match Lambda.parse ?primitives e with
  | Ok y           -> y
  | Error (`Msg e) -> Alcotest.failf "parsing: %s" e

let _typ_exn e =
  match Lambda.typ e with
  | Ok y    -> y
  | Error e -> Alcotest.failf "typing: %a" Lambda.pp_error e

let test_if () =
  let x = Parsetree.(if_ true_ (int 42) (int 21)) in
  Alcotest.(check @@ ok int) "if" (Ok 42) (type_and_eval x Type.int);
  Alcotest.(check @@ neg @@ ok string) "failure" (Ok "")
    (type_and_eval x Type.string)

let test_match () =
  let x =
    let open Parsetree in
    match_ (left Type.int (string "Hello World!"))
      (var 0)
      (string "<int>")
  in
  Alcotest.(check @@ ok string) "match"
    (Ok "Hello World!")
    (type_and_eval x Type.string)


let test_lambda () =
  let x =
    let open Parsetree in
    apply (
      apply (
        lambda [ ("x", Type.int); ("y", Type.int) ] (var 1)
      ) (int 42)
    ) (int 21)
  in
  Alcotest.(check @@ ok int) "lambda" (type_and_eval x Type.int) (Ok 42);
  let y = parse_exn {|
      let f (x: int, y: int): int = x + y in
      f 42 21
  |} in
  Alcotest.(check @@ ok int) "lambda" (type_and_eval y Type.int) (Ok 63)

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
      let_rec Type.("x", int ** int) Type.int (fun ~context ~return ~continue ->
          let acc = fst context in
          let n = snd context in
          (if_ (n = int 0)
             (return acc)
             (continue (pair (acc * n) (n - int 1))))
        ) in
    lambda ("x", Type.int) (main $ (pair (int 1) (var Var.o)))
  in
  Alcotest.(check @@ int) "safe" 120 (Expr.eval safe () 5);
  (* check untyping -> pretty-printing -> parsing -> typing *)
  let u  = Lambda.Expr.untype safe in
  let _ = typ_exn u in
  let s  = Fmt.to_to_string Lambda.Parsetree.pp u in
  let u' = parse_exn s in
  let _  = typ_exn u' in
  Alcotest.(check pexpr) "full" u u';

  let unsafe =
    let open Parsetree in
    let init = pair (lambda ["_", Type.int] (int 1)) (var 0) in
    let ptyp = Type.((int @-> int) ** int) in
    let main =
      fix
        ("v", ptyp) Type.int
        (if_
           (snd (var 0) = int 0)
           (right ptyp (apply (fst (var 0)) (int 0)))
           (left Type.int
              (pair
                 (lambda ["y", Type.int]
                    ((apply (fst (var 1)) (var 0)) * (snd (var 1))))
                 (snd (var 0) - int 1))))
    in
    lambda ["x",  Type.int] (apply main init)
  in
  Alcotest.(check @@ ok int) "unsafe" (Ok 120)
    (type_and_eval unsafe Type.(int @-> int) $ 5);

  let str = parse_exn {|
    rec fact (v: int * int): int =
      let acc: int = fst v in
      let n  : int = snd v in
      if n = 1 then
         return acc
      else
         continue (acc * n, n - 1)
    in
    fun (x: int) -> fact (1, x)
  |} in
  Alcotest.(check @@ ok int) "parse" (Ok 120)
    (type_and_eval str Type.(int @-> int) $ 5)

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
    let open Parsetree in
    (lambda ["x", Type.string] (apply (apply padd (int 21)) (int 21)))
  in

  Alcotest.(check @@ ok int) "env_and_prim safe" (Ok 42)
    (type_and_eval env_and_prim Type.(string @-> int) $ "Hello World!");
  Alcotest.(check @@ ok int) "env_and_prim unsafe" (Ok 42)
    (type_and_eval
       Parsetree.(lambda ["x", Type.int] env_and_prim)
       Type.(int @-> string @-> int)
     $ 0 $ "Hello World!")

let parse_exn ?primitives s = match parse ?primitives s with
  | Ok e -> e
  | Error (`Msg e) -> Alcotest.fail e

let test_parse_expr () =
  let check s (a, t) r =
    let e = parse_exn s in
    Alcotest.(check @@ ok a) ("parse: " ^ s) (Ok r) (type_and_eval e t);
    let te = typ_exn e in
    let ete = untype te in
    Alcotest.(check pexpr) "parse untyped" e ete;
    let s' = Fmt.to_to_string Parsetree.pp e in
    Logs.debug (fun l -> l "roundtrip: %s => %s" s s');
    let e' = parse_exn s' in
    Alcotest.(check @@ pexpr) ("roundtrip: " ^ s') e e';
    let te' = typ_exn e' in
    let ete' = untype te' in
    Alcotest.(check pexpr) "parse untyped (2)" e' ete'
  in
  let int = (Alcotest.int, Type.int) in
  let bool = (Alcotest.bool, Type.bool) in
  let string = (Alcotest.string, Type.string) in
  let list l = (Alcotest.list (fst l), Type.list (snd l)) in
  let array l = (Alcotest.array (fst l), Type.array (snd l)) in
  let option a = (Alcotest.option (fst a), Type.option (snd a)) in
  let result a b =
    (Alcotest.result (fst a) (fst b), Type.result (snd a) (snd b))
  in
  let either a b =
    let pp ppf = function
      | Type.L x -> Fmt.pf ppf "L %a" (Alcotest.pp (fst a)) x
      |      R x -> Fmt.pf ppf "R %a" (Alcotest.pp (fst b)) x
    in
    let eq x y = match x, y with
      | Type.L x, Type.L y -> Alcotest.equal (fst a) x y
      |      R x,      R y -> Alcotest.equal (fst b) x y
      | _ -> false
    in
    Alcotest.testable pp eq, Type.either (snd a) (snd b)
  in
  check "1 + 1 + 1" int 3;
  check "1 + 1 * 3" int 4;
  check "1 + 1 = 2" bool true;
  check "(1 = 2)" bool false;
  check "[1;2]" (list int) [1; 2];
  check "([]: string list)" (list string) [];
  check "[|1;2|]" (array int) [|1; 2|];
  check "([||]: bool array)" (array bool) [||];
  check "(None: int option)" (option int) None;
  check "Ok 1 bool" (result int bool) (Ok 1);
  check "Error int true" (result int bool) (Error true);
  check "Some \"foo\"" (option string) (Some "foo");
  check "L (None: int option) string" (either (option int) string) (L None);
  check "(fun (x:int) -> x + 1) 1" int 2;
  check "(fun (x:int, y:bool) -> y) 1 false" bool false;
  check {|
    (fun (f: int -> int, k:int) -> f k)
      (fun (x:int) -> x + 1)
      2 |} int 3

let test_ping () =
  let app t f x =
    let f = parse_exn f in
    let x = parse_exn x in
    match type_and_eval Parsetree.(apply f x) t with
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

module Block: sig
  type t
  val pp: t Fmt.t
  type error
  val pp_error: error Fmt.t
  val connect: string -> t
  val read: t -> int64 -> string list -> (unit, error) result Lwt.t
end = struct
  type error = [ `Foo ]
  let pp_error ppf `Foo = Fmt.string ppf "Foo"
  type t = C of string
  let pp ppf (C t) = Fmt.pf ppf "(C %S)" t
  let connect n = C n

  let read (C n) off pages =
    Logs.debug (fun l ->
        l "READ[%s] off=%Ld pages=%a" n off Fmt.(Dump.list string) pages);
    if off = 0L then Lwt.return (Error `Foo) else Lwt.return (Ok ())

end

let error_t = Alcotest.testable Block.pp_error (=)

let lwt_t a =
  Alcotest.testable
    (fun ppf x -> Alcotest.pp a ppf (Lwt_main.run x))
    (fun x y -> Alcotest.equal a (Lwt_main.run x) (Lwt_main.run y))

let test_block () =
  let t = Type.abstract "Block.t" in
  let error = Type.abstract "Block.error" in
  let primitives = [
    primitive "Block.connect" [Type.string] t Block.connect;
    primitive "Block.to_string" [t] Type.string (Fmt.to_to_string Block.pp);
    L.primitive "Block.read"
      Type.[t; int64; list string] Type.(lwt (result unit error))
      Block.read
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
  Alcotest.(check @@ ok (lwt_t (result unit error_t))) "read"
    (Ok (Lwt.return (Ok ())))
    (L.type_and_eval
       (parse_exn ~primitives "Block.read (Block.connect \"foo\") 1L [\"x\"]")
       Type.(lwt (result unit error)));

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
