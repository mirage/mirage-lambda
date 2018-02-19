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

type 'a t = { v: 'a; t: 'a Type.t }

let dump_string t v = Bytes.to_string @@ Marshal.(to_bytes {t; v} [Closures])

let dump t = Fmt.of_to_string (dump_string t)

let parse t str =
  if String.length str < Marshal.header_size then
    Error (`Msg "Invalid marshalled value")
  else
    let len =
      try Some (Marshal.data_size (Bytes.unsafe_of_string str) 0)
      with Failure _ -> None
    in
    match len with
    | None     -> Error (`Msg "Invalid header")
    | Some len ->
      if String.length str <> len + Marshal.header_size then
        Error (`Msg "Invalid size")
      else
        let x  = Marshal.from_bytes (Bytes.unsafe_of_string str) 0 in
        match Type.eq t x.t with
        | None           -> Error (`Msg "Invalid type")
        | Some Type.Refl -> Ok x.v

module Type = Type
