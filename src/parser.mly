%{

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

  exception Internal of string

  let err fmt = Fmt.kstrf (fun s -> raise (Internal s)) fmt

  let err_unbound v prims ctx =
    err "The variable %s is not bound in %a"
        v Fmt.(Dump.list string) (List.map fst prims @ ctx)

  let index v l =
    let rec aux i = function
      | []   -> None
      | h::t -> if String.equal h v then Some i else aux (i+1) t
    in
    aux 0 l

  let resolve_name v prims ctx =
    match index v ctx with
    | Some i -> Parsetree.var i
    | None   ->
       if List.mem_assoc v prims then List.assoc v prims
       else err_unbound v prims ctx

  let add_ctx a ctx = List.rev_map fst a @ ctx

  let add_prim (_, l) r prims =
    let stop = Primitive.stop l r in
    let continue = Primitive.continue l r in
    stop :: continue :: prims

  open Parsetree
%}

%token UNIT
%token <int> INT
%token <int32> INT32
%token <int64> INT64
%token <string> VAR STRING
%token <bool> BOOL

%token NONE SOME OK ERROR
%token RETURN BIND
%token PLUS MINUS
%token TIMES
%token EQ IN COLON SEMI
%token EOF
%token LPAR RPAR LSQU RSQU
%token FUN REC IF ELSE LET THEN
%token COMMA
%token L R
%token APP
%token DOLLAR
%token FST SND GET

%token S_INT S_INT32 S_INT64 S_BOOL S_STRING S_LIST S_ARRAY S_OPTION S_RESULT S_LWT
%token ARROW BAR

%right    ARROW
%nonassoc below_SEMI
%nonassoc SEMI
%nonassoc LET
%nonassoc ELSE
%nonassoc COMMA
%left     EQ
%left     PLUS MINUS
%left     TIMES
%left     BIND

%left     S_OPTION S_LIST S_ARRAY S_LWT
%nonassoc LSQU VAR INT INT32 INT64 STRING LPAR BOOL REC R L IF FUN FST SND
          OK ERROR SOME RETURN NONE GET UNIT
          S_STRING S_INT S_INT32 S_INT64 S_BOOL
%nonassoc APP

%left BAR

%start main
%type <(string * Parsetree.expr) list -> (string * Parsetree.Type.abstract) list -> (Parsetree.expr, string) result> main
%type <(string * Parsetree.expr) list -> (string * Parsetree.Type.abstract) list -> string list -> Parsetree.expr> expr

%start request
%type <(string * Parsetree.expr) list -> (string * Parsetree.Type.abstract) list -> (Parsetree.expr * Parsetree.Type.t, string) result> request

%%

main:
  | e=expr_seq EOF { fun prims gams ->
                     try Ok (e prims gams [])
                     with Internal s -> Error s }

request:
  | e=expr_seq COLON t=typ EOF
                         { fun prims gams ->
                           try Ok (e prims gams [], t gams)
                           with Internal s -> Error s }

expr_seq:
  | e=expr %prec below_SEMI { e }
  | e=expr SEMI             { e }
  | e=expr SEMI b=expr_seq
    { fun prims gams ctx ->
        let v = "_" and t = Type.unit in
        let e = e prims gams ctx in
        let b = b prims gams ctx in
        let_var t v e b }

base_expr:
  | LPAR e=expr RPAR        { e }
  | i=INT                   { fun _ _ _ -> int i }
  | i=INT32                 { fun _ _ _ -> int32 i }
  | i=INT64                 { fun _ _ _ -> int64 i }
  | s=STRING                { fun _ _ _ -> string s }
  | b=BOOL                  { fun _ _ _ -> if b then true_ else false_ }
  | v=VAR                   { fun prims _ ctx -> resolve_name v prims ctx }
  | v=VAR DOLLAR i=INT      {
      fun prims _ ctx ->
        let id = index v ctx in
        if Pervasives.(id = Some i) then resolve_name v prims ctx
        else err "%s$%d should have id %a" v i Fmt.(option int) id }


expr:
  | e=base_expr                  { e }
  | GET i=base_expr e=base_expr  { fun p g c -> get (i p g c) (e p g c) }
  | R t=base_typ e=base_expr     { fun p g c -> right (t g) (e p g c) }
  | L e=base_expr t=base_typ     { fun p g c -> left (t g) (e p g c) }
  | OK e=base_expr t=base_typ    { fun p g c -> ok (t g) (e p g c) }
  | ERROR t=base_typ e=base_expr { fun p g c -> error (t g) (e p g c) }
  | RETURN e=base_expr           { fun p g c -> return (e p g c) }
  | x=expr BIND f=expr           { fun p g c -> bind (x p g c) (f p g c) }
  | SOME e=base_expr             { fun p g c -> some (e p g c) }
  | NONE t=base_typ              { fun _ g _ -> none (t g) }
  | FST e=base_expr              { fun p g c -> fst (e p g c) }
  | SND e=base_expr              { fun p g c -> snd (e p g c) }
  | LSQU BAR BAR RSQU t=base_typ { fun _ g _ -> array ~typ:(t g) [||] }
  | LSQU BAR l=list BAR RSQU
    { fun p g c ->
      let l = List.map (fun e -> e p g c) l in
      array (Array.of_list l) }
  | LSQU RSQU t=base_typ         { fun _ g _ -> list ~typ:(t g) [] }
  | LSQU l=list RSQU             { fun p g c -> list (List.map (fun e -> e p g c) l) }
  | a=expr COMMA b=expr          { fun p g c -> pair (a p g c) (b p g c) }
  | IF i=expr THEN t=expr ELSE e=expr
    { fun prims gams ctx ->
        let i = i prims gams ctx in
        let t = t prims gams ctx in
        let e = e prims gams ctx in
        if_ i t e }
  | LET v=VAR COLON t=typ EQ e=expr IN b=expr_seq
    { fun prims gams ctx ->
        let e = e prims gams ctx in
        let b = b prims gams (v::ctx) in
        let_var (t gams) v e b }
  | LET n=VAR LPAR a=args RPAR COLON t=typ EQ e=expr IN b=expr_seq
    { fun prims gams ctx ->
        let a = a gams in
        let e = e prims gams (add_ctx a ctx) in
        let b = b prims gams (n :: ctx) in
        let_fun (t gams) n a e b
    }
  | REC n=VAR LPAR a=arg RPAR COLON t=typ EQ e=expr IN b=expr_seq
    { fun prims gams ctx ->
        let a = a gams in
        let t = t gams in
        let e = e (add_prim a t prims) gams (add_ctx [a] ctx) in
        let b = b prims gams (n :: ctx) in
        let_rec t n a e b }
  | REC LPAR a=arg RPAR COLON t=base_typ ARROW e=expr
    { fun prims gams ctx ->
        let a = a gams in
        let e = e prims gams (add_ctx [a] ctx) in
        fix a (t gams) e }
  | FUN LPAR a=args RPAR ARROW e=expr
    { fun prims gams ctx ->
        let a = a gams in
        let e = e prims gams (add_ctx a ctx) in
        lambda a e }
  | a=expr f=binop b=expr   {
      fun prims gams ctx ->
      let a = a prims gams ctx in
      let b = b prims gams ctx in
      f a b }
  | f=expr x=expr %prec APP {
      fun prims gams ctx ->
      let f = f prims gams ctx in
      let x = x prims gams ctx in
      apply f x }

%inline binop:
  | PLUS  { ( + ) }
  | MINUS { ( - ) }
  | TIMES { ( * ) }
  | EQ    { ( = ) }

base_typ:
  | LPAR a=typ RPAR  { a }
  | UNIT             { fun _ -> Type.unit }
  | S_INT            { fun _ -> Type.int }
  | S_INT32          { fun _ -> Type.int32 }
  | S_INT64          { fun _ -> Type.int64 }
  | S_BOOL           { fun _ -> Type.bool }
  | S_STRING         { fun _ -> Type.string }
  | S_LWT            { fun _ -> Type.lwt }
  | v=VAR            { fun gams ->
                       try let Type.A ({ name; _}, _ ) = List.assoc v gams in
                           if String.equal name v
                           then Type.unsafe_abstract (List.assoc v gams)
                           else err "Mismatch abstract type: %s <> %s (expected)" v name
                       with _ ->
                         err "Invalid abstract type: %s (%a)"
                             v Fmt.(list (pair string Type.pp_abstract)) gams }

typ:
  | ty=base_typ       { ty }
  | a=typ S_LIST      { fun g -> Type.list (a g) }
  | a=typ S_ARRAY     { fun g -> Type.array (a g) }
  | a=typ S_OPTION    { fun g -> Type.option (a g) }
  | a=typ ARROW b=typ { fun g -> Type.((a g) @-> (b g)) }
  | a=typ TIMES b=typ { fun g -> Type.((a g) ** (b g)) }
  | a=typ BAR b=typ   { fun g -> Type.((a g) || (b g)) }
  | LPAR a=typ COMMA b=typ RPAR S_RESULT { fun g -> Type.result (a g) (b g) }
  | a=typ b=typ %prec APP { fun g -> Type.apply (a g) (b g) }

list:
  | x=separated_nonempty_list(SEMI, expr) { x }

args:
  | l=separated_nonempty_list(COMMA, arg) { fun g -> List.map (fun a -> a g) l }

arg:
  | v=VAR COLON t=typ { fun g -> (v, t g) }
