(*---------------------------------------------------------------------------
   Copyright (c) 2016 Thomas Gazagnaire. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

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


open Dist_closures.Type

type r = { foo: int; bar: string list; z: z option }

and z = { x: int; r: r list }

let rec r = lazy (
  record [
    field int           (fun t -> t.foo);
    field (list string) (fun t -> t.bar);
    field (option !!z)  (fun t -> t.z);
  ]
)

and z = lazy (
  record [
    field int        (fun t -> t.x);
    field (list !!r) (fun t -> t.r);
  ]
)

let r = Lazy.force r
let z = Lazy.force z

let r1 = { foo = 3; bar = ["aaa";"b"]; z = None }
let r2 = { foo = 3; bar = ["aaa";"c"]; z = Some { x = 2; r = [r1; r1] } }

type v =
  | Foo
  | Bar of int
  | Yo of x * v option

and x = {
  r: r;
  i: (int * v) list;
}

let rec v = lazy (
  variant [
    case0 Foo;
    case1 int (fun x -> Bar x);
    case1 (pair !!x (option !!v)) (fun (x, y) -> Yo (x, y));
  ]
)

and x = lazy (
  record [
    field r (fun x -> x.r);
    field (list (pair int !!v)) (fun x -> x.i);
  ]
)

let lazy v = v
let lazy x = x

let v1 = Foo
let v2 = Bar 0
let v3 =
  Yo ({ r = r2; i = [ (1, v1); (2, v2); (3, v2); (4, Bar 3); (5, Bar 6)] },
      Some v2)

type e = Fooe | Bars | Toto | Tata
let e = enum [Fooe; Bars; Toto; Tata]

type y = [`E of e]

let y: y t = variant [case1 e (fun e -> `E e)]

let e1 = Fooe
let e2 = Bars
let e3 = Toto
let e4 = Tata

let y1 = `E e1
let y2 = `E e2

type dyn = Dyn: 'a t * 'a -> dyn

let test check =
  check @@ Dyn (r, r1);
  check @@ Dyn (r, r2);
  check @@ Dyn (v, v1);
  check @@ Dyn (v, v2);
  check @@ Dyn (v, v3);
  check @@ Dyn (e, e1);
  check @@ Dyn (e, e2);
  check @@ Dyn (e, e3);
  check @@ Dyn (y, y1);
  check @@ Dyn (y, y2)

let basic () =
  let check (Dyn (t, x)) =
    Logs.debug (fun l -> l "checking type %a" pp t);
    let s = Dist_closures.dump_string t x in
    match Dist_closures.parse t s with
    | Ok _ -> ()
    | Error (`Msg s) -> Alcotest.fail s
  in
  test check

let safety () =
  let check (Dyn (t, _)) =
    Logs.debug (fun l -> l "checking type safety for %a" pp t);
    let s =
      String.init (Random.int 128) (fun _ -> char_of_int @@ Random.int 256)
    in
    Logs.debug (fun l -> l "random string: %S" s);
    match Dist_closures.parse t s with
    | Ok _    -> Alcotest.failf "no no no"
    | Error _ -> ()
  in
  test check

let pipe () =
  let ic, oc = Unix.pipe ~cloexec:false () in
  Unix.in_channel_of_descr ic, Unix.out_channel_of_descr oc

let fork () =
  let check (Dyn (t, x)) =
    Logs.debug (fun l -> l "checking type %a" pp t);
    let s = Dist_closures.dump_string t x in
    let ic, oc = pipe () in
    match Unix.fork () with
    | 0 -> (* child *)
      close_in ic;
      output_string oc s;
      close_out oc;
      exit 0
    | _ ->
      close_out oc;
      let s = really_input_string ic (String.length s) in
      close_in ic;
      (match Dist_closures.parse t s with
       | Ok _ -> ()
       | Error (`Msg s) -> Alcotest.fail s)
  in
  test check

let () =
  Alcotest.run "ptr" [
    "basic", [
      "same process", `Quick, basic;
      "fork"        , `Quick, fork;
      "safety"      , `Quick, safety;
    ]
  ]

(*---------------------------------------------------------------------------
   Copyright (c) 2016 Thomas Gazagnaire

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
