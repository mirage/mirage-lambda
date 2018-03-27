(* Copyright (c) 2013 Leo White and Jeremy Yallop

   Permission is hereby granted, free of charge, to any person
   obtaining a copy of this software and associated documentation
   files (the "Software"), to deal in the Software without
   restriction, including without limitation the rights to use, copy,
   modify, merge, publish, distribute, sublicense, and/or sell copies
   of the Software, and to permit persons to whom the Software is
   furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be
   included in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
   EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
   NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
   BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
   ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
   CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
   SOFTWARE. *)

type ('p, 'f) app = App

module type S = sig
  type 'a s
  type t
  external inj : 'a s -> ('a, t) app  = "%identity"
  external prj : ('a, t) app -> 'a s = "%identity"
end

module Make (T : sig type 'a t end): S with type 'a s = 'a T.t = struct
  type t
  type 'a s = 'a T.t
  external inj : 'a -> 'b = "%identity"
  external prj : 'a -> 'b = "%identity"
end

module Lwt =Make(Lwt)
let lwt: Lwt.t Eq.witness = Eq.witness "Lwt.t"
