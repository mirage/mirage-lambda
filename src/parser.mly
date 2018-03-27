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

  let error fmt = Fmt.kstrf (fun s -> raise (Internal s)) fmt

  let err_unbound v prims ctx =
    error "The variable %s is not bound in %a"
          v Fmt.(Dump.list string) (List.map fst prims @ ctx)

  let index v l =
    let rec aux i = function
      | []   -> None
      | h::t -> if String.equal h v then Some i else aux (i+1) t
    in
    aux 0 l

  let resolve_name v prims ctx =
    match index v ctx with
    | Some i -> Untyped.var i
    | None   ->
       if List.mem_assoc v prims then List.assoc v prims
       else err_unbound v prims ctx

  open Untyped
%}

%token <int> INT
%token <string> VAR STRING
%token <bool> BOOL

%token PLUS MINUS
%token TIMES
%token EQ
%token EOF
%token LPAR RPAR
%token FUN
%token APP
%token COLON DOLLAR

%token S_INT S_BOOL S_STRING
%token ARROW BAR

%left ARROW

%nonassoc VAR INT STRING LPAR FUN BOOL
%nonassoc APP

%left EQ
%left PLUS MINUS
%left TIMES
%left BAR

%start main
%type <(string * Untyped.expr) list -> (Untyped.expr, [`Msg of string]) result> main
%type <(string * Untyped.expr) list -> string list -> Untyped.expr> expr

%%

main:
  | e=expr EOF { fun prims ->
                 try Ok (e prims [])
                 with Internal s -> Error (`Msg s) }

expr:
  | LPAR e=expr RPAR        { e }
  | i=INT                   { fun _ _   -> int i }
  | s=STRING                { fun _ _   -> string s }
  | b=BOOL                  { fun _ _   -> if b then true_ else false_ }
  | v=VAR                   { resolve_name v }
  | v=VAR DOLLAR i=INT      {
      fun prims ctx ->
        let id = index v ctx in
        if Pervasives.(id = Some i) then resolve_name v prims ctx
        else error "%s$%d should have id %a" v i Fmt.(option int) id }
  | a=expr f=binop b=expr   {
      fun prims ctx ->
      let a = a prims ctx in
      let b = b prims ctx in
      f a b }
  | FUN LPAR n=VAR COLON t=typ RPAR ARROW e=expr
    { fun prims ctx ->
      let ctx = n :: ctx in
      Logs.debug (fun l -> l "LAMBDA %s %a" n Fmt.(Dump.list string) ctx) ;
      lambda t n (e prims ctx) }
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
  | S_INT             { Type.int }
  | S_BOOL            { Type.bool }
  | S_STRING          { Type.string }
  | a=typ ARROW b=typ { Type.lambda a b }
  | a=typ TIMES b=typ { Type.(a ** b) }
  | a=typ BAR b=typ   { Type.(a || b) }
  | LPAR a=typ RPAR   { a }
