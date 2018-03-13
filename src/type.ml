(*---------------------------------------------------------------------------
   Copyright (c) 2016-2018 Thomas Gazagnaire
   Copyright (c) 2018 Seagate

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

let src = Logs.Src.create "dist-closure.type" ~doc:"Serializable runtime types"
module Log = (val Logs.src_log src : Logs.LOG)

type (_, _) eq = Refl: ('a, 'a) eq

type _ t =
  | Prim   : 'a prim -> 'a t
  | Unlazy : 'a t lazy_t -> 'a t (* backpointer, for cycles *)
  | Arrow  : ('a t * 'b t) -> ('a -> 'b) t
  | List   : 'a t -> 'a list t
  | Array  : 'a t -> 'a array t
  | Tuple  : 'a tuple -> 'a t
  | Option : 'a t -> 'a option t
  | Record : 'a record -> 'a t
  | Variant: 'a variant -> 'a t

and 'a prim =
  | Unit   : unit prim
  | Bool   : bool prim
  | Char   : char prim
  | Int    : int prim
  | Int32  : int32 prim
  | Int64  : int64 prim
  | Float  : float prim
  | String : string prim

and 'a tuple =
  | Single : 'a t -> 'a tuple
  | Pair   : 'a t * 'b t -> ('a * 'b) tuple
  | Triple : 'a t * 'b t * 'c t -> ('a * 'b * 'c) tuple

and 'a record = {
  fields: 'a field list;
}

and 'a field =
  | Field: { ftype: 'b t;
             fget: 'a -> 'b ; (* link the types 'a and 'b *)
           } -> 'a field


and 'a variant = {
  cases: 'a case list;
}

and 'a case =
  | C0: 'a case0 -> 'a case
  | C1: ('a, 'b) case1 -> 'a case

and 'a case0 = {
  cval0: 'a;
}

and ('a, 'b) case1 = {
  ctyp1: 'b tuple;
  cmak1: 'b -> 'a;
}

module Pp = struct

  let idx l = List.mapi (fun i x -> i, x) l

  let rec t: type a. a t Fmt.t = fun ppf -> function
    | Prim p       -> prim ppf p
    | Unlazy _     -> Fmt.pf ppf "..."
    | Arrow (a, b) -> Fmt.pf ppf "%a -> %a" t a t b
    | List a       -> Fmt.pf ppf "%a list" t a
    | Array a      -> Fmt.pf ppf "%a array" t a
    | Tuple a      -> tuple ppf a
    | Option a     -> Fmt.pf ppf "%a option" t a
    | Record a     -> record ppf a
    | Variant a    -> variant ppf a

  and prim: type a. a prim Fmt.t = fun ppf -> function
    | Unit   -> Fmt.string ppf "unit"
    | Bool   -> Fmt.string ppf "bool"
    | Char   -> Fmt.string ppf "char"
    | Int    -> Fmt.string ppf "int"
    | Int32  -> Fmt.string ppf "int32"
    | Int64  -> Fmt.string ppf "int64"
    | Float  -> Fmt.string ppf "float"
    | String -> Fmt.string ppf "string"

  and tuple: type a. a tuple Fmt.t = fun ppf -> function
    | Single a         -> t ppf a
    | Pair (a, b)      -> Fmt.pf ppf "(%a * %a)" t a t b
    | Triple (a, b, c) -> Fmt.pf ppf "(%a * %a * %a)" t a t b t c

  and record: type a. a record Fmt.t = fun ppf r ->
    Fmt.pf ppf "{ %a }" Fmt.(list ~sep:(unit "; ") field) (idx r.fields)

  and field: type a. (int * a field) Fmt.t = fun ppf (i, Field x) ->
    Fmt.pf ppf "%d: %a" i t x.ftype

  and variant: type a. a variant Fmt.t = fun ppf v ->
    let case0, case1 =
      List.partition (function C0 _ -> true | _ -> false) v.cases
    in
    Fmt.pf ppf "[ %a ]" Fmt.(list ~sep:(unit " | ") pp_case)
      (idx case0 @ idx case1)

  and pp_case: type a. (int * a case) Fmt.t = fun ppf (i, c) ->
    match c with
    | C0 _ -> Fmt.int ppf i
    | C1 c -> Fmt.pf ppf "%d of %a" i tuple c.ctyp1

end

let pp = Pp.t

type elt = Elt: 'a t * 'b t -> elt

module Tbl = Hashtbl.Make(struct
    type t = elt
    let equal (Elt (a,b)) (Elt (c,d)) =
      Obj.(repr a == repr c && repr b == repr d)
    let hash = Hashtbl.hash
  end)

type tbl = bool Tbl.t

module Refl = struct

  let unsafe_ok: type a b. unit -> (a, b) eq option =
    fun () -> Obj.magic @@ Some Refl

  let prim: type a b. a prim -> b prim -> (a, b) eq option = fun a b ->
    match a, b with
    | Unit  , Unit   -> Some Refl
    | Bool  , Bool   -> Some Refl
    | Char  , Char   -> Some Refl
    | Int   , Int    -> Some Refl
    | Int32 , Int32  -> Some Refl
    | Int64 , Int64  -> Some Refl
    | Float , Float  -> Some Refl
    | String, String -> Some Refl
    | _ -> None

  let rec t: type a b. tbl -> a t -> b t -> (a, b) eq option =
    fun tbl a b ->
      Log.debug (fun l -> l "eq %a %a" pp a pp b);
      if Tbl.mem tbl (Elt (a, b)) then unsafe_ok ()
      else (
        Tbl.replace tbl (Elt (a, b)) true;
        match a, b with
        | Unlazy a, b      -> t tbl (Lazy.force a) b
        | a, Unlazy b      -> t tbl a (Lazy.force b)
        | Prim a , Prim b  -> prim a b
        | Arrow a, Arrow b -> arrow tbl a b
        | List a , List b  ->
          (match t tbl a b with Some Refl -> Some Refl | None -> None)
        | Array a, Array b ->
          (match t tbl a b with Some Refl -> Some Refl | None -> None)
        | Tuple a , Tuple b  -> tuple tbl a b
        | Option a, Option b ->
          (match t tbl a b with Some Refl -> Some Refl | None -> None)
        | Record a , Record b  -> fields tbl a.fields b.fields
        | Variant a, Variant b -> cases tbl a.cases b.cases
        | _ -> None
      )

  and arrow: type a b c d. tbl ->
    a t * b t -> c t * d t -> (a -> b, c -> d) eq option
    = fun tbl (a, b) (c, d) ->
      match t tbl a c with
      | None      -> None
      | Some Refl -> match t tbl b d with
        | None      -> None
        | Some Refl -> Some Refl

  and fields:
    type a b. tbl -> a field list -> b field list -> (a, b) eq option
    = fun tbl a b -> match a, b with
      | []    , []     -> unsafe_ok ()
      | [], _ | _, []  -> None
      | ah::at, bh::bt ->
        match field tbl ah bh with
        | false -> None
        | true  -> fields tbl at bt

  and field: type a b. tbl -> a field -> b field -> bool =
    fun tbl a b -> match a, b with
      | Field ta, Field tb ->
        match t tbl ta.ftype tb.ftype with
        | None      -> false
        | Some Refl -> true

  and cases: type a b. tbl -> a case list -> b case list -> (a, b) eq option =
    fun tbl a b ->
      let aux a b = match a, b with
        | C0 _, C0 _ -> true
        | C1 a, C1 b ->
          (match tuple tbl a.ctyp1 b.ctyp1 with
           | None      -> false
           | Some Refl -> true)
        | _ -> false
      in
      if List.length a = List.length b && List.for_all2 aux a b then
        unsafe_ok ()
      else
        None

  and tuple:
    type a b. tbl -> a tuple -> b tuple -> (a, b) eq option
    = fun tbl a b ->
      match a, b with
      | Single a, Single b -> t tbl a b
      | Pair (a0, a1), Pair (b0, b1) ->
        (match t tbl a0 b0, t tbl a1 b1 with
         | Some Refl, Some Refl -> Some Refl
         | _ -> None)
      | Triple (a0, a1, a2), Triple (b0, b1, b2) ->
        (match t tbl a0 b0, t tbl a1 b1, t tbl a2 b2 with
         | Some Refl, Some Refl, Some Refl -> Some Refl
         | _ -> None)
      | _ -> None

end

let eq x y = Refl.t Tbl.(create 7) x y


let unit = Prim Unit
let bool = Prim Bool
let char = Prim Char
let int = Prim Int
let int32 = Prim Int32
let int64 = Prim Int64
let float = Prim Float
let string = Prim String

let list l = List l
let array a = Array a
let pair a b = Tuple (Pair (a, b))
let triple a b c = Tuple (Triple (a, b, c))
let option a = Option a
let (@->) a b = Arrow (a, b)
let (!!) x = Unlazy x

let rec match_arrow: type a b. (a -> b) t -> (a t * b t) = function
  | Arrow (a, b)     -> (a, b)
  | Unlazy a         -> match_arrow (Lazy.force a)
  | Tuple (Single a) -> match_arrow a
  | Record _         -> assert false
  | Variant _        -> assert false
  | _ -> .

(* records *)

let field ftype fget = Field { ftype; fget }
let record fields = Record { fields }

(* variants *)

let variant cases =
  let case0, case1 =
    List.partition (function C0 _ -> true | _ -> false) cases
  in
  Variant { cases = case0 @ case1 }

let case0 cval0 = C0 { cval0 }
let case1 t cmak1 = C1 { ctyp1 = Single t; cmak1 }
let case2 t1 t2 cmak1 = C1 { ctyp1 = Pair (t1, t2); cmak1 }
let case3 t1 t2 t3 cmak1 = C1 { ctyp1 = Triple (t1, t2, t3); cmak1 }

let enum c = variant (List.map case0 c)
