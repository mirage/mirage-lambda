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
%token FST SND

%token S_INT S_INT32 S_INT64 S_BOOL S_STRING S_LIST S_ARRAY S_OPTION S_RESULT
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

%left     S_OPTION S_LIST S_ARRAY
%nonassoc LSQU VAR INT INT32 INT64 STRING LPAR BOOL REC R L IF FUN FST SND
          OK ERROR SOME RETURN
%nonassoc APP

%left BAR

%start main
%type <(string * Parsetree.expr) list -> (Parsetree.expr, string) result> main
%type <(string * Parsetree.expr) list -> string list -> Parsetree.expr> expr

%%

main:
  | e=expr_seq EOF { fun prims ->
                     try Ok (e prims [])
                     with Internal s -> Error s }

expr_seq:
  | e=expr %prec below_SEMI { e }
  | e=expr SEMI             { e }
  | e=expr SEMI b=expr_seq
    { fun prims ctx ->
        let v = "_" and t = Type.unit in
        let e = e prims ctx in
        let b = b prims ctx in
        let_var t v e b }

expr:
  | LPAR e=expr RPAR        { e }
  | i=INT                   { fun _ _ -> int i }
  | i=INT32                 { fun _ _ -> int32 i }
  | i=INT64                 { fun _ _ -> int64 i }
  | s=STRING                { fun _ _ -> string s }
  | b=BOOL                  { fun _ _ -> if b then true_ else false_ }
  | R t=typ e=expr          { fun p c -> right t (e p c) }
  | L e=expr t=typ          { fun p c -> left t (e p c) }
  | OK e=expr t=typ         { fun p c -> ok t (e p c) }
  | ERROR t=typ e=expr      { fun p c -> error t (e p c) }
  | RETURN e=expr           { fun p c -> return (e p c) }
  | x=expr BIND f=expr      { fun p c -> bind (x p c) (f p c) }
  | SOME e=expr             { fun p c -> some (e p c) }
  | LPAR NONE COLON t=typ S_OPTION RPAR
    { fun _ _ -> none t }
  | FST e=expr              { fun p c -> fst (e p c) }
  | SND e=expr              { fun p c -> snd (e p c) }
  | LPAR LSQU BAR BAR RSQU COLON t=typ S_ARRAY RPAR
                            { fun _ _ -> array ~typ:t [||] }
  | LSQU BAR l=list BAR RSQU
    { fun p c ->
      let l = List.map (fun e -> e p c) l in
      array (Array.of_list l) }
  | LPAR LSQU RSQU COLON t=typ S_LIST RPAR
                            { fun _ _ -> list ~typ:t [] }
  | LSQU l=list RSQU        { fun p c -> list (List.map (fun e -> e p c) l)}
  | a=expr COMMA b=expr     { fun p c -> pair (a p c) (b p c) }
  | IF i=expr THEN t=expr ELSE e=expr
    { fun prims ctx ->
        let i = i prims ctx in
        let t = t prims ctx in
        let e = e prims ctx in
        if_ i t e }
  | LET v=VAR COLON t=typ EQ e=expr IN b=expr_seq
    { fun prims ctx ->
        let e = e prims ctx in
        let b = b prims (v::ctx) in
        let_var t v e b }
  | LET n=VAR LPAR a=args RPAR COLON t=typ EQ e=expr IN b=expr_seq
    { fun prims ctx ->
        let e = e prims (add_ctx a ctx) in
        let b = b prims (n :: ctx) in
        let_fun t n a e b
    }
  | REC n=VAR LPAR a=arg RPAR COLON t=typ EQ e=expr IN b=expr_seq
    { fun prims ctx ->
        let e = e (add_prim a t prims) (add_ctx [a] ctx) in
        let b = b prims (n :: ctx) in
        let_rec t n a e b
    }
  | REC LPAR a=arg RPAR COLON t=typ ARROW e=expr
    { fun prims ctx ->
        let e = e prims (add_ctx [a] ctx) in
        fix a t e }
  | FUN LPAR a=args RPAR ARROW e=expr
    { fun prims ctx ->
        let e = e prims (add_ctx a ctx) in
        lambda a e }
  | v=VAR                   { resolve_name v }
  | v=VAR DOLLAR i=INT      {
      fun prims ctx ->
        let id = index v ctx in
        if Pervasives.(id = Some i) then resolve_name v prims ctx
        else err "%s$%d should have id %a" v i Fmt.(option int) id }
  | a=expr f=binop b=expr   {
      fun prims ctx ->
      let a = a prims ctx in
      let b = b prims ctx in
      f a b }
  | f=expr x=expr %prec APP {
      fun prims ctx ->
      let f = f prims ctx in
      let x = x prims ctx in
      apply f x }

%inline binop:
  | PLUS  { ( + ) }
  | MINUS { ( - ) }
  | TIMES { ( * ) }
  | EQ    { ( = ) }

typ:
  | UNIT              { Type.unit }
  | S_INT             { Type.int }
  | S_INT32           { Type.int32 }
  | S_INT64           { Type.int64 }
  | S_BOOL            { Type.bool }
  | S_STRING          { Type.string }
  | a=typ S_LIST      { Type.list a }
  | a=typ S_ARRAY     { Type.array a }
  | a=typ S_OPTION    { Type.option a }
  | a=typ ARROW b=typ { Type.(a @-> b) }
  | a=typ TIMES b=typ { Type.(a ** b) }
  | a=typ BAR b=typ   { Type.(a || b) }
  | LPAR a=typ RPAR   { a }
  | LPAR a=typ COMMA b=typ RPAR S_RESULT { Type.result a b }

list:
  | x=separated_nonempty_list(SEMI, expr) { x }

args:
  | x=separated_nonempty_list(COMMA, arg) {x}

arg:
  | v=VAR COLON t=typ { (v, t) }
