let unsafe_type_gen : Parsetree.Type.t Crowbar.gen =
  let open Parsetree in

  Crowbar.fix @@ fun unsafe_type_gen ->
  Crowbar.choose
    [ Crowbar.const Type.unit
    ; Crowbar.const Type.int
    ; Crowbar.const Type.int32
    ; Crowbar.const Type.int64
    ; Crowbar.const Type.bool
    ; Crowbar.const Type.string
    ; Crowbar.map [ unsafe_type_gen ] (fun t -> Type.list t)
    ; Crowbar.map [ unsafe_type_gen ] (fun t -> Type.array t)
    ; Crowbar.map [ unsafe_type_gen ] (fun t -> Type.option t)
    ; Crowbar.map [ unsafe_type_gen; unsafe_type_gen ] (fun ta tb -> Type.either ta tb)
    ; Crowbar.map [ unsafe_type_gen; unsafe_type_gen ] (fun ta tb -> Type.result ta tb)
    ; Crowbar.map [ unsafe_type_gen; ] (fun t -> Type.apply t Type.lwt)
    ; Crowbar.map [ Crowbar.bytes ] (fun name -> Type.abstract (Eq.witness name))
        (* XXX(dinosaure): lol je suis vraiment pas sûr. *)
    ; Crowbar.map [ unsafe_type_gen; unsafe_type_gen ] (fun ta tb -> Type.(ta ** tb))
    ; Crowbar.map [ unsafe_type_gen; unsafe_type_gen ] (fun ta tb -> Type.(ta @-> tb)) ]

let type_gen = Crowbar.map [ unsafe_type_gen ] Typedtree.Type.typ

let pp_unit ppf () = Fmt.string ppf "()"
let pp_either ppa ppb ppf = function
  | T.L a -> Fmt.pf ppf "(L %a)" ppa a
  | T.R b -> Fmt.pf ppf "(R %a)" ppb b
let eq_int : int -> int -> bool = (=)
let eq_bool : bool -> bool -> bool = (=)

let value t pp eq v = Parsetree.V { v; t; pp; eq; }

let (<.>) f g = fun x -> f (g x)

let pp : type a. a T.t -> Parsetree.value -> a Fmt.t = fun witness -> function
  | Parsetree.V { pp; t; v; _ } -> match T.equal t witness with
    | Some Eq.Refl -> pp
    | None -> Fmt.invalid_arg "Type %a does not match with value %a." Typedtree.Type.pp witness pp v

let eq: type a. a T.t -> Parsetree.value -> a Parsetree.eq = fun witness -> function
  | Parsetree.V { pp; eq; t; v; } -> match T.equal t witness with
    | Some Eq.Refl -> eq
    | None -> Fmt.invalid_arg "Type %a does not match with value %a." Typedtree.Type.pp witness pp v

let to_list witness l =
  let rec map : type a. a T.t -> Parsetree.value list -> a list -> a list = fun witness l a -> match l with
    | [] -> List.rev a
    | Parsetree.V { v; t; _ } :: r -> match T.equal t witness with
      | Some Eq.Refl -> map witness r (v :: a)
      | None -> map witness r a in
  map witness l []

let pp_bytes ppf s = Fmt.string ppf (Bytes.unsafe_to_string s)

let rec value_gen : type a. a T.t -> Parsetree.value Crowbar.gen = fun ty ->
  match ty with
  | T.Unit     -> Crowbar.const (value ty pp_unit (fun () () -> true) ())
  | T.Int      -> Crowbar.(map [ int ] (value ty Fmt.int eq_int))
  | T.Int32    -> Crowbar.(map [ int32 ] (value ty Fmt.int32 Int32.equal))
  | T.Int64    -> Crowbar.(map [ int64 ] (value ty Fmt.int64 Int64.equal))
  | T.Bool     -> Crowbar.(map [ bool ] (value ty Fmt.bool eq_bool))
  | T.String   -> Crowbar.(map [ bytes ] (value ty Fmt.string String.equal))
  | T.Bytes    ->
    Crowbar.(map [ bytes ] (fun s ->
        value ty pp_bytes Bytes.equal (Bytes.unsafe_of_string s)
      ))
  | T.List ta  ->
    let cmp = Typedtree.Type.eq_val ty in
    Crowbar.(map [ list (value_gen ta); (value_gen ta) ]) (fun l x -> value ty Fmt.(list (pp ta x)) cmp (to_list ta l))
  | T.Array ta ->
    let cmp = Typedtree.Type.eq_val ty in
    Crowbar.(map [ list (value_gen ta); (value_gen ta) ]) (fun l x -> value ty Fmt.(array (pp ta x)) cmp (Array.of_list @@ to_list ta l))
  | T.Option ta ->
    Crowbar.(map [ bool; (value_gen ta) ])
      (fun o x ->
         let pp = Fmt.(option (pp ta x)) in
         let cmp = Typedtree.Type.eq_val ty in

         match o with
         | true -> value ty pp cmp (Some (Typedtree.Value.cast x ta))
         | false -> value ty pp cmp None)
  | T.Pair (ta, tb) ->
    Crowbar.(map [ (value_gen ta); (value_gen tb) ])
      (fun va vb ->
         let ppa = pp ta va in
         let ppb = pp tb vb in
         let cmp = Typedtree.Type.eq_val ty in

         value ty Fmt.(pair ppa ppb) cmp (Typedtree.Value.cast va ta, Typedtree.Value.cast vb tb))
  | T.Either (ta, tb) ->
    Crowbar.(map [ bool; (value_gen ta); (value_gen tb) ])
      (fun c va vb ->
         let ppa = pp ta va in
         let ppb = pp tb vb in
         let pp = pp_either ppa ppb in
         let cmp = Typedtree.Type.eq_val ty in

         match c with
         | true -> value ty pp cmp (T.L (Typedtree.Value.cast va ta))
         | false -> value ty pp cmp (T.R (Typedtree.Value.cast vb tb)))
  | T.Result (ta, tb) ->
    Crowbar.(map [ bool; (value_gen ta); (value_gen tb) ])
      (fun c va vb ->
         let ppa = pp ta va in
         let ppb = pp tb vb in
         let pp = Fmt.result ~ok:ppa ~error:ppb in
         let cmp = Typedtree.Type.eq_val ty in

         match c with
         | true -> value ty pp cmp (Ok (Typedtree.Value.cast va ta))
         | false -> value ty pp cmp (Error (Typedtree.Value.cast vb tb)))
  | T.Lwt        -> Crowbar.bad_test ()
  | T.Apply _    -> Crowbar.bad_test ()
  | T.Abstract _ -> Crowbar.bad_test ()
  | T.Arrow _    -> Crowbar.bad_test ()

let pair_gen : 'a Crowbar.gen -> 'b Crowbar.gen ->  ('a * 'b) Crowbar.gen = fun a b -> Crowbar.map [a; b] (fun a b -> (a, b))

let unsafe_expr_gen : Parsetree.expr Crowbar.gen =
  Crowbar.fix @@ fun unsafe_expr_gen ->
  Crowbar.choose
    [ Crowbar.dynamic_bind type_gen (fun (Typedtree.Type.V ty) -> Crowbar.map [ (value_gen ty) ] Parsetree.of_value)
    ; Crowbar.map [ Crowbar.int ] Parsetree.var
    ; Crowbar.(map [ unsafe_type_gen; list unsafe_expr_gen ]) (fun typ expr -> Parsetree.list ~typ expr)
    ; Crowbar.(map [ unsafe_type_gen; list unsafe_expr_gen ]) (fun typ expr -> Parsetree.array ~typ (Array.of_list expr))
    ; Crowbar.(map [ unsafe_type_gen ] (fun ty -> Parsetree.none ty))
    ; Crowbar.(map [ unsafe_expr_gen ]) Parsetree.some
    ; Crowbar.(map [ unsafe_type_gen; unsafe_expr_gen ]) Parsetree.ok
    ; Crowbar.(map [ unsafe_type_gen; unsafe_expr_gen ]) Parsetree.error
    ; Crowbar.(map [ list (pair_gen bytes unsafe_type_gen); unsafe_expr_gen ]) Parsetree.lambda
    ; Crowbar.(map [ unsafe_expr_gen; unsafe_expr_gen ]) Parsetree.pair
    ; Crowbar.(map [ unsafe_expr_gen ]) Parsetree.fst
    ; Crowbar.(map [ unsafe_expr_gen ]) Parsetree.snd
    ; Crowbar.(map [ unsafe_type_gen; unsafe_expr_gen ]) Parsetree.left
    ; Crowbar.(map [ unsafe_type_gen; unsafe_expr_gen ]) Parsetree.right
    ; Crowbar.(map [ unsafe_type_gen; bytes; unsafe_expr_gen; unsafe_expr_gen ]) Parsetree.let_var
    ; Crowbar.(map [ unsafe_type_gen; bytes; list (pair_gen bytes unsafe_type_gen); unsafe_expr_gen; unsafe_expr_gen ]) Parsetree.let_fun
    ; Crowbar.(map [ unsafe_type_gen; bytes; pair_gen bytes unsafe_type_gen; unsafe_expr_gen; unsafe_expr_gen ]) Parsetree.let_rec
    ; Crowbar.(map [ unsafe_expr_gen; unsafe_expr_gen; unsafe_expr_gen ]) Parsetree.if_
    ; Crowbar.(map [ unsafe_expr_gen; unsafe_expr_gen; unsafe_expr_gen ]) Parsetree.match_
    ; Crowbar.(map [ unsafe_expr_gen; unsafe_expr_gen ]) Parsetree.apply
    ; Crowbar.(map [ pair_gen bytes unsafe_type_gen; unsafe_type_gen; unsafe_expr_gen ]) Parsetree.fix
    ; Crowbar.(map [ unsafe_expr_gen; unsafe_expr_gen ]) Parsetree.( = )
    ; Crowbar.(map [ unsafe_expr_gen; unsafe_expr_gen ]) Parsetree.( + )
    ; Crowbar.(map [ unsafe_expr_gen; unsafe_expr_gen ]) Parsetree.( - )
    ; Crowbar.(map [ unsafe_expr_gen; unsafe_expr_gen ]) Parsetree.( * ) ]
