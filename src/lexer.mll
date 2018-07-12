{
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

open Parser

type t = {
  debug       : bool;
  buffer      : Buffer.t
}

let v ?(debug=false) () = { debug; buffer = Buffer.create 8196 }
let p t fmt = Fmt.kstrf (fun s -> if t.debug then print_endline s) fmt

let eof t =
  p t "EOF";
  EOF

let get_string t =
  let str = Buffer.contents t.buffer in
  Buffer.reset t.buffer;
  p t "STRING %S" str;
  STRING str

let add_char t c =
  p t "adding %c" c;
  Buffer.add_char t.buffer c

let add_newline t l =
  Lexing.new_line l;
  let str = Lexing.lexeme l in
  for i = 0 to String.length str - 1 do
    add_char t str.[i]
  done

let check_newlines l =
  let s = Lexing.lexeme l in
  for i=0 to String.length s - 1 do
    if s.[i] = '\n' then Lexing.new_line l
  done

exception Error of string
let syntax_error s = raise (Error s)

}

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z' '_']
let var = alpha (alpha | digit | '-' | '.' | '\'')*
let newline = '\r' | '\n' | "\r\n"
let white = [' ' '\t']+

rule program t = parse
  | white    { program t lexbuf }
  | newline  { Lexing.new_line lexbuf; program t lexbuf }
  | '"'      { string t lexbuf }
  | (digit+ as s) 'L' { p t "INT64 %s" s; INT64 (Int64.of_string s) }
  | (digit+ as s) 'l' { p t "INT32 %s" s; INT32 (Int32.of_string s) }
  | digit+ as s { p t "INT %s" s; INT (int_of_string s)}
  | "->"     { p t "ARROW"; ARROW }
  | "+"      { p t "PLUS"; PLUS }
  | "-"      { p t "MINUS"; MINUS }
  | "*"      { p t "TIMES"; TIMES }
  | "="      { p t "EQ"; EQ }
  | ","      { p t "COMMA"; COMMA }
  | ":"      { p t "COLON"; COLON }
  | ";"      { p t "SEMI"; SEMI }
  | "$"      { p t "DOLLAR"; DOLLAR }
  | "("      { p t "LPAR"; LPAR }
  | ")"      { p t "RPAR"; RPAR }
  | "["      { p t "LSQU"; LSQU }
  | "]"      { p t "RSQU"; RSQU }
  | "|"      { p t "BAR"; BAR }
  | "if"     { p t "IF"; IF }
  | "then"   { p t "THEN"; THEN }
  | "else"   { p t "ELSE"; ELSE }
  | "R"      { p t "R"; R }
  | "L"      { p t "L"; L }
  | "unit"   { p t "unit"; UNIT }
  | "get"    { p t "get"; GET }
  | "fst"    { p t "FST"; FST }
  | "snd"    { p t "SND"; SND }
  | "let"    { p t "LET"; LET }
  | "in"     { p t "IN"; IN }
  | "fun"    { p t "FUN"; FUN }
  | "rec"    { p t "REC"; REC }
  | "true"   { p t "TRUE"; BOOL true }
  | "false"  { p t "FALSE"; BOOL false }
  | "int"    { p t "int"; S_INT }
  | "int32"  { p t "int32"; S_INT32 }
  | "int64"  { p t "int64"; S_INT64 }
  | "list"   { p t "list"; S_LIST }
  | "array"  { p t "array"; S_ARRAY }
  | "option" { p t "option"; S_OPTION }
  | "return" { p t "RETURN"; RETURN }
  | ">>="    { p t ">>="; BIND }
  | "Ok"     { p t "Ok"; OK }
  | "Error"  { p t "Error"; ERROR }
  | "Some"   { p t "Some"; SOME }
  | "None"   { p t "None"; NONE }
  | "result" { p t "result"; S_RESULT }
  | "bool"   { p t "bool"; S_BOOL }
  | "string" { p t "string"; S_STRING }
  | "Lwt.t"  { p t "Lwt.t"; S_LWT }
  | var as s { p t "VAR %s" s; VAR s }
  | eof      { EOF }

and string t = parse
  | '"'     { get_string t }
  | newline { add_newline t lexbuf; string t lexbuf }
  | _ as c  { add_char t c; string t lexbuf }

{

let token t lexbuf = program t lexbuf

}
