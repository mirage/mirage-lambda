module Int64 = struct
  let ( lsl ) a n = Int64.shift_left (Int64.of_int a) n
  let ( lsr ) a n = Int64.(to_int (shift_right a n))
  let ( lor ) = Int64.logor
  let ( land ) = Int64.logand
  let of_int = Int64.of_int
  let to_int = Int64.to_int
  let ( + ) = Int64.add
  let ( - ) = Int64.sub
  let succ = Int64.succ
end

type request = Request
type reply = Reply

module Decoder = struct
  type error

  let pp_error : error Fmt.t = fun _ _ -> assert false

  type 'kind t =
    { i_off : int
    ; i_pos : int
    ; i_len : int
    ; proto_size : int64
    ; block_size : int64
    ; block_n    : int64
    ; i_tmp : Cstruct.t
    ; kind  : 'kind
    ; state : 'kind state }
  and 'kind state =
    | Header of 'kind k
    | Raw of { kind : kind; consumed : int64 }
    | Stp of { kind : kind; consumed : int64; buf : Cstruct.t }
    | Exception of error
    | End
  and 'kind k = Cstruct.t -> 'kind t -> 'kind res
  and 'kind res =
    | Wait of 'kind t
    | Error of 'kind t * error
    | Flush of 'kind t * kind * Cstruct.t
    | Cont of 'kind t
    | Ok of 'kind t
  and kind = [ `Protobuf | `Block of int64 ]

  let pp_kind ppf = function
    | `Protobuf -> Fmt.string ppf "`Protobuf"
    | `Block n -> Fmt.pf ppf "(`Block %Ld)" n

  let pp_state ppf = function
    | Header _ -> Fmt.pf ppf "(Header #fun)"
    | Raw { kind; consumed; } -> Fmt.pf ppf "(Raw { @[<hov>kind = %a;@ consumed = %Ld;@] })" pp_kind kind consumed
    | Stp { kind; consumed; _ } -> Fmt.pf ppf "(Stp { @[<hov>kind = %a;@ consumed = %Ld;@] })" pp_kind kind consumed
    | Exception err -> Fmt.pf ppf "(Exception %a)" pp_error err
    | End -> Fmt.string ppf "End"

  let pp ppf t =
    Fmt.pf ppf "{ @[<hov>i_off = %d;@ \
                         i_pos = %d;@ \
                         i_len = %d;@ \
                         proto_size = %Ld;@ \
                         block_size = %Ld;@ \
                         block_n = %Ld;@ \
                         i_tmp = #buf;@ \
                         state = %a;@] }"
      t.i_off t.i_pos t.i_len
      t.proto_size t.block_size t.block_n
      pp_state t.state

  let await t = Wait t
  let error t err = Error ({ t with state = Exception err }, err)
  let ok t = Ok { t with state = End }
  let flush t kind consumed buf =
    Flush ({ t with state = Stp { kind; consumed; buf; }}, kind, buf)

  let rec get_byte ~ctor k src t =
    if (t.i_len - t.i_pos) > 0
    then let byte = Cstruct.get_uint8 src (t.i_off + t.i_pos) in
      k byte src
        { t with i_pos = t.i_pos + 1 }
    else await { t with state = ctor (fun src t -> (get_byte[@tailcall]) ~ctor k src t) }

  let rec get_int64 ~ctor k src t =
    let get_byte k src t = get_byte ~ctor k src t in

    if (t.i_len - t.i_pos) > 7
    then let n = Cstruct.LE.get_uint64 src (t.i_off + t.i_pos) in
      k n src
        { t with i_pos = t.i_pos + 8 }
    else if (t.i_len - t.i_pos) > 0
    then (get_byte
          @@ fun byte7 -> get_byte
          @@ fun byte6 -> get_byte
          @@ fun byte5 -> get_byte
          @@ fun byte4 -> get_byte
          @@ fun byte3 -> get_byte
          @@ fun byte2 -> get_byte
          @@ fun byte1 -> get_byte
          @@ fun byte0 ->
          let n =
            Int64.((byte7 lsl 56)
                   lor (byte6 lsl 48)
                   lor (byte5 lsl 40)
                   lor (byte4 lsl 32)
                   lor (byte3 lsl 24)
                   lor (byte2 lsl 16)
                   lor (byte1 lsl 8)
                   lor (of_int byte0)) in
          k n) src t
    else await { t with state = ctor (fun src t -> (get_int64[@tailcall]) ~ctor k src t) }

  module KHeader = struct
    let ctor k = Header k
    let get_byte k src t = get_byte ~ctor k src t
    let get_int64 k src t = get_int64 ~ctor k src t
  end

  let block src t n consumed =
    if consumed = t.block_size
    then flush t (`Block n) consumed (Cstruct.sub t.i_tmp 0 0)
    else
      let len = min (Cstruct.len t.i_tmp) (t.i_len - t.i_pos) in
      let len = min len Int64.(to_int (t.block_size - consumed)) in
      Cstruct.blit src (t.i_off + t.i_pos) t.i_tmp 0 len;
      flush { t with i_pos = t.i_pos + len } (`Block n) Int64.(consumed + (of_int len)) (Cstruct.sub t.i_tmp 0 len)

  let proto src t consumed =
    if consumed = t.proto_size
    then Cont { t with state =
                         if t.block_n = 0L
                         then End
                         else Stp { kind = `Protobuf
                                  ; consumed
                                  ; buf = Cstruct.sub t.i_tmp 0 (Int64.to_int consumed) } }
    else
      let len = min (Cstruct.len t.i_tmp) (t.i_len - t.i_pos) in
      let len = min len Int64.(to_int (t.proto_size - consumed)) in
      Cstruct.blit src (t.i_off + t.i_pos) t.i_tmp 0 len;
      flush { t with i_pos = t.i_pos + len } `Protobuf Int64.(consumed + (of_int len)) (Cstruct.sub t.i_tmp 0 len)

  let header src t =
    (KHeader.get_int64
     @@ fun proto_size -> KHeader.get_int64
     @@ fun block_size -> KHeader.get_int64
     @@ fun block_n _src t ->
     Cont { t with proto_size
                 ; block_size
                 ; block_n
                 ; state = Raw { kind = `Protobuf
                               ; consumed = 0L } })
    src t

  let default ?(len = 0x8000) kind =
    { i_off = 0
    ; i_pos = 0
    ; i_len = 0
    ; proto_size = 0L
    ; block_size = 0L
    ; block_n    = 0L
    ; i_tmp      = Cstruct.create len
    ; kind
    ; state = Header header }

  let reset t =
    { t with proto_size = 0L
           ; block_size = 0L
           ; block_n = 0L
           ; state = Header header }

  let eval0 src t =
    match t.state with
    | Header k -> k src t
    | Raw { kind = `Protobuf; consumed; } -> proto src t consumed
    | Raw { kind = (`Block n); consumed; } -> block src t n consumed
    | Stp { kind; consumed; buf; } -> flush t kind consumed buf
    | Exception err -> error t err
    | End -> ok t

  let eval src t =
    let rec loop t = match eval0 src t with
      | Flush (t, kind, buf) -> `Flush (t, kind, buf)
      | Wait t -> `Await t
      | Error (t, err) -> `Error (t, err)
      | Cont t -> loop t
      | Ok t -> `End t in
    loop t

  let flush t = match t.state with
    | Stp { kind = `Protobuf; consumed; _ } ->
      if consumed = t.proto_size
      then { t with state = if t.block_n = 0L then End else Raw { kind = `Block 0L; consumed = 0L } }
      else { t with state = Raw { kind = `Protobuf; consumed; } }
    | Stp { kind = `Block n; consumed; _ } ->
      if consumed = t.block_size
      then { t with state = if Int64.succ n = t.block_n then End else Raw { kind = `Block (Int64.succ n); consumed = 0L } }
      else { t with state = Raw { kind = `Block n; consumed; } }
    | _ -> invalid_arg "Rpc.Decoder.flush: invalid state"

  let refill off len t =
    { t with i_off = off
           ; i_len = len
           ; i_pos = 0 }

  let block_size t = t.block_size
  let proto_size t = t.proto_size
  let block_n t = t.block_n
end

module Encoder = struct
  type error

  let pp_error : error Fmt.t = fun _ _ -> assert false

  type 'kind t =
    { o_off : int
    ; o_pos : int
    ; o_len : int
    ; i_off : int
    ; i_pos : int
    ; i_len : int
    ; proto_size : int64
    ; block_size : int64
    ; block_n    : int64
    ; p_tmp : string
    ; o_tmp : Cstruct.t
    ; kind : 'kind
    ; state : 'kind state }
  and 'kind state =
    | Header of 'kind k
    | Protobuf of { consumed : int }
    | Block of { n : int64; consumed : int }
    | Exception of error
    | End
  and 'kind k = Cstruct.t -> Cstruct.t -> 'kind t -> 'kind res
  and 'kind res =
    | Cont of 'kind t
    | Flush of 'kind t
    | Wait of 'kind t
    | Ok of 'kind t
    | Error of 'kind t * error

  let pp_state ppf = function
    | Header _ -> Fmt.pf ppf "(Header #k)"
    | Protobuf { consumed; } -> Fmt.pf ppf "(Protobuf { @[<hov>consumed = %d;@] })" consumed
    | Block { n; consumed; } -> Fmt.pf ppf "(Block { @[<hov>n = %Ld;@ consumed = %d;@] })" n consumed
    | Exception err -> Fmt.pf ppf "(Exception %a)" pp_error err
    | End -> Fmt.string ppf "End"

  let pp ppf t =
    Fmt.pf ppf "{ @[<hov>o_off = %d;@ \
                         o_pos = %d;@ \
                         o_len = %d;@ \
                         i_off = %d;@ \
                         i_pos = %d;@ \
                         i_len = %d;@ \
                         proto_size = %Ld;@ \
                         block_size = %Ld;@ \
                         block_n = %Ld;@ \
                         p_tmp = #buffer;@ \
                         o_tmp = #buffer;@ \
                         state = %a;@] }"
      t.o_off t.o_pos t.o_len
      t.i_off t.i_pos t.i_len
      t.proto_size t.block_size t.block_n
      pp_state t.state

  let ok t = Ok { t with state = End }
  let await t = Wait t
  let flush t = Flush t
  let error t err = Error ({ t with state = Exception err }, err)

  let rec put_byte ~ctor byte k src dst t =
    if (t.o_len - t.o_pos) > 0
    then begin
      Cstruct.set_uint8 dst (t.o_off + t.o_pos) byte;
      k src dst { t with o_pos = t.o_pos + 1 }
    end else flush { t with state = ctor (fun dst t -> (put_byte[@tailcall]) ~ctor byte k dst t) }

  let rec put_int64 ~ctor n k src dst t =
    let put_byte byte k dst t = put_byte ~ctor byte k dst t in
    if (t.o_len - t.o_pos) > 8
    then begin
      Cstruct.LE.set_uint64 dst (t.o_off + t.o_pos) n;
      k src dst { t with o_pos = t.o_pos + 8 }
    end else if (t.o_len - t.o_pos) > 0
    then begin
      let byte7 = Int64.((n land 0xFF00000000000000L) lsr 56) in
      let byte6 = Int64.((n land 0x00FF000000000000L) lsr 48) in
      let byte5 = Int64.((n land 0x0000FF0000000000L) lsr 40) in
      let byte4 = Int64.((n land 0x000000FF00000000L) lsr 32) in
      let byte3 = Int64.((n land 0x00000000FF000000L) lsr 24) in
      let byte2 = Int64.((n land 0x0000000000FF0000L) lsr 16) in
      let byte1 = Int64.((n land 0x000000000000FF00L) lsr 8)  in
      let byte0 = Int64.(to_int (n land 0x00000000000000FFL)) in

      (put_byte byte0
       @@ put_byte byte1
       @@ put_byte byte2
       @@ put_byte byte3
       @@ put_byte byte4
       @@ put_byte byte5
       @@ put_byte byte6
       @@ put_byte byte7 k)
        src dst t
    end else flush { t with state = ctor (fun dst t -> (put_int64[@tailcall]) ~ctor n k dst t) }

  module KHeader = struct
    let ctor k = Header k
    let put_byte byte k src dst t = put_byte ~ctor byte k src dst t
    let put_int64 n k src dst t = put_int64 ~ctor n k src dst t
  end

  let block src dst t n consumed =
    if consumed = (Int64.to_int t.block_size)
    then flush { t with state =
                          if Int64.succ n = t.block_n
                          then End
                          else Block { n = Int64.succ n; consumed = 0 } }
    else begin
      let len = min (t.o_len - t.o_pos) (t.i_len - t.i_pos) in
      let len = min len (Int64.to_int t.block_size - consumed) in
      Cstruct.blit src (t.i_off + t.i_pos) dst (t.o_off + t.o_pos) len;

      match (t.i_len - (t.i_pos + len)) > 0, (t.o_len - (t.o_pos + len)) > 0 with
      | true, true
      | false, false
      | true, false -> flush { t with i_pos = t.i_pos + len
                                    ; o_pos = t.o_pos + len
                                    ; state = Block { n; consumed = consumed + len; } }
      | false, true -> await { t with i_pos = t.i_pos + len
                                    ; o_pos = t.o_pos + len
                                    ; state = Block { n; consumed = consumed + len; } }
    end

  let proto _src dst t consumed =
    if consumed = String.length t.p_tmp
    then Cont { t with state =
                         if t.block_n = 0L
                         then End
                         else Block { n = 0L; consumed = 0 } }
    else begin
      let len = min (t.o_len - t.o_pos) (String.length t.p_tmp - consumed) in
      let len = min len (Int64.to_int t.proto_size - consumed) in
      Cstruct.blit_from_string t.p_tmp consumed dst (t.o_off + t.o_pos) len;
      flush { t with o_pos = t.o_pos + len
                   ; state = Protobuf { consumed = consumed + len } }
    end

  let header src dst t =
    (KHeader.put_int64 t.proto_size
     @@ KHeader.put_int64 t.block_size
     @@ KHeader.put_int64 t.block_n
     @@ fun _src _dst t -> Cont { t with state = Protobuf { consumed = 0 } })
      src dst t

  type ('k, 'v) protobuf =
    | Request : (Lambda_types.request, request) protobuf
    | Reply   : (Lambda_types.reply,  reply) protobuf

  let default
    : type p k. (p, k) protobuf -> ?len:int -> p -> int64 -> int64 -> k t
    = fun kind ?(len = 0x8000) protobuf block_size block_n ->
      let encoder = Pbrt.Encoder.create () in
      let () = match kind with
        | Request -> Lambda_pb.encode_request protobuf encoder
        | Reply   -> Lambda_pb.encode_reply protobuf encoder in
      let p_tmp = Pbrt.Encoder.to_bytes encoder |> Bytes.unsafe_to_string in
      { i_off = 0
      ; i_pos = 0
      ; i_len = 0
      ; o_off = 0
      ; o_pos = 0
      ; o_len = 0
      ; proto_size = Int64.of_int (String.length p_tmp)
      ; block_size
      ; block_n
      ; p_tmp
      ; o_tmp = Cstruct.create len
      ; kind = (match kind with Request -> Request | Reply -> Reply) (* GADT LOLILOL *)
      ; state = Header header }

  let eval0 src dst t =
    match t.state with
    | Header k -> k src dst t
    | Protobuf { consumed } -> proto src dst t consumed
    | Block { n; consumed; } -> block src dst t n consumed
    | Exception err -> error t err
    | End -> ok t

  let eval src dst t =
    let rec loop t = match eval0 src dst t with
      | Wait t -> `Await t
      | Flush t -> `Flush t
      | Cont t -> loop t
      | Error (t, err) -> `Error (t, err)
      | Ok t -> `End t in
    loop t

  let refill off len t =
    { t with i_off = off
           ; i_len = len
           ; i_pos = 0 }

  let flush off len t =
    { t with o_off = off
           ; o_len = len
           ; o_pos = 0 }

  let used_out t = t.o_pos
  let used_in t = t.i_pos

  let block t = match t.state with
    | Block { n; consumed; } -> n, consumed
    | _ -> invalid_arg "block: invalid state"
end
