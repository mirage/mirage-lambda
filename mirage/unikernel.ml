open Lwt.Infix

module type BLOCK = Mirage_block_lwt.S
module type TCP = Mirage_stack_lwt.V4

[@@@warning "-40"]

let pp_chr =
  Fmt.using
    (function '\032' .. '\126' as x -> x
            | _ -> '.')
    Fmt.char

let pp_scalar : type buffer. get:(buffer -> int -> char) -> length:(buffer -> int) -> buffer Fmt.t
  = fun ~get ~length ppf b ->
  let l = length b in

  for i = 0 to l / 16
  do Fmt.pf ppf "%08x: " (i * 16);
    let j = ref 0 in

    while !j < 16
    do if (i * 16) + !j < l
      then Fmt.pf ppf "%02x" (Char.code @@ get b ((i * 16) + !j))
      else Fmt.pf ppf "  ";

      if !j mod 2 <> 0 then Fmt.pf ppf " ";

      incr j;
    done;

    Fmt.pf ppf "  ";
    j := 0;

    while !j < 16
    do if (i * 16) + !j < l
      then Fmt.pf ppf "%a" pp_chr (get b ((i * 16) + !j))
      else Fmt.pf ppf " ";

      incr j;
    done;

    Fmt.pf ppf "@\n"
  done

let pp_string    = pp_scalar ~get:String.get ~length:String.length
let pp_bytes     = pp_scalar ~get:Bytes.get ~length:Bytes.length
let pp_cstruct   = pp_scalar ~get:Cstruct.get_char ~length:Cstruct.len

let log_src = Logs.Src.create "lambda" ~doc:"lambda"
module Log = (val Logs.src_log log_src : Logs.LOG)

module Main (B: BLOCK) (S: TCP) = struct

  module AbstractTypes = struct
    (* XXX(dinosaure): we need to define types at top-level. *)

    let cstruct     = Lambda.Type.abstract "Cstruct.t"
    let formatter   = Lambda.Type.abstract "Format.formatter"
    let error       = Lambda.Type.abstract "Block.error"
    let write_error = Lambda.Type.abstract "Block.write_error"
    let info        = Lambda.Type.abstract "Mirage_block.info"
  end

  let make_environment b =
    let open AbstractTypes in
    let open Lambda_protobuf in
    List.fold_left
      (fun primitives (k, v) -> match v with
         | Lambda.Parsetree.Prm primitive -> Primitives.add k primitive primitives
         | _ -> Fmt.invalid_arg "Invalid expression as primitive")
      Primitives.empty
      Lambda.[
        primitive   "stdout"                   [ ] formatter Format.(formatter_of_out_channel stdout)
      ; primitive   "stderr"                   [ ] formatter Format.(formatter_of_out_channel stderr)
      ; primitive   "Block.error_to_string"    [ error; ]
          Type.string                          (Format.asprintf "%a%!" B.pp_error)
      ; primitive   "Block.write_error_to_string" [ write_error ]
          Type.string                          (Format.asprintf "%a%!" B.pp_write_error)
      ; primitive   "Block.pp_error"           [ formatter; error; ]
          Type.unit                            B.pp_error
      ; primitive   "Block.pp_write_error"     [ formatter; write_error; ]
          Type.unit                            B.pp_write_error
      ; L.primitive "Block.disconnect"         [ Type.unit ]
          Type.(lwt unit)                      (fun () -> B.disconnect b)
      ; L.primitive "Block.get_info"           [ Type.unit ]
          Type.(lwt info)                      (fun () -> B.get_info b)
      ; primitive   "read_write"               [ info ]
          Type.bool                            (fun b -> b.Mirage_block.read_write)
      ; primitive   "sector_size"              [ info ]
          Type.int                             (fun b -> b.Mirage_block.sector_size)
      ; primitive   "size_sectors"             [ info ]
          Type.int64                           (fun b -> b.Mirage_block.size_sectors)
      ; L.primitive "Block.read"               Type.[ int64; list cstruct; ]
          Type.(lwt (result unit error))       B.(read b)
      ; L.primitive "Block.write"              Type.[ int64; list cstruct; ]
          Type.(lwt (result unit write_error)) B.(write b)
      ; primitive   "Cstruct.to_bytes"         [ cstruct ]
          Type.bytes                           Cstruct.to_bytes
      ; primitive   "Cstruct.of_bytes"         [ Type.bytes ]
          cstruct                              (fun s -> Cstruct.of_bytes s)
      ; primitive   "Cstruct.blit"             Type.[ cstruct; int; cstruct; int; int; ]
          Type.unit                            Cstruct.blit
      ; primitive   "Cstruct.blit_to_bytes"    Type.[ cstruct; int; bytes; int; int; ]
          Type.unit                            Cstruct.blit_to_bytes
      ; primitive   "Cstruct.blit_from_bytes"  Type.[ bytes; int; cstruct; int; int; ]
          Type.unit                            Cstruct.blit_from_bytes
      ; primitive   "Cstruct.get_uint8"        Type.[ cstruct; int; ]
          Type.int                             Cstruct.get_uint8
      ; primitive   "Cstruct.set_uint8"        Type.[ cstruct; int; int; ]
          Type.unit                            Cstruct.set_uint8
      ; primitive   "Cstruct.LE.get_uint16"    Type.[ cstruct; int; ]
          Type.int                             Cstruct.LE.get_uint16
      ; primitive   "Cstruct.LE.get_uint32"    Type.[ cstruct; int; ]
          Type.int32                           Cstruct.LE.get_uint32
      ; primitive   "Cstruct.LE.get_uint64"    Type.[ cstruct; int; ]
          Type.int64                           Cstruct.LE.get_uint64
      ; primitive   "Cstruct.BE.get_uint16"    Type.[ cstruct; int; ]
          Type.int                             Cstruct.BE.get_uint16
      ; primitive   "Cstruct.BE.get_uint32"    Type.[ cstruct; int; ]
          Type.int32                           Cstruct.BE.get_uint32
      ; primitive   "Cstruct.BE.get_uint64"    Type.[ cstruct; int; ]
          Type.int64                           Cstruct.BE.get_uint64
      ; primitive   "Cstruct.LE.set_uint16"    Type.[ cstruct; int; int; ]
          Type.unit                            Cstruct.LE.set_uint16
      ; primitive   "Cstruct.LE.set_uint32"    Type.[ cstruct; int; int32; ]
          Type.unit                            Cstruct.LE.set_uint32
      ; primitive   "Cstruct.LE.set_uint64"    Type.[ cstruct; int; int64; ]
          Type.unit                            Cstruct.LE.set_uint64
      ; primitive   "Cstruct.BE.set_uint16"    Type.[ cstruct; int; int; ]
          Type.unit                            Cstruct.BE.set_uint16
      ; primitive   "Cstruct.BE.set_uint32"    Type.[ cstruct; int; int32; ]
          Type.unit                            Cstruct.BE.set_uint32
      ; primitive   "Cstruct.BE.set_uint64"    Type.[ cstruct; int; int64; ]
          Type.unit                            Cstruct.BE.set_uint64
      ; primitive   "int64_of_int"             Type.[ int; ]
          Type.int64                           Int64.of_int
      ; primitive   "int64_to_int"             Type.[ int64; ]
          Type.int                             Int64.to_int
      ; primitive   "int32_of_int"             Type.[ int; ]
          Type.int32                           Int32.of_int
      ; primitive   "int32_to_int"             Type.[ int32 ]
          Type.int                             Int32.to_int
      ],
    List.fold_left
      (fun gamma (k, v) -> Gamma.add k v gamma)
      Gamma.empty
      [ "Format.formatter",  Lambda.Type.abstract_injection formatter
      ; "Block.error",       Lambda.Type.abstract_injection error
      ; "Cstruct.t",         Lambda.Type.abstract_injection cstruct
      ; "Mirage_block.info", Lambda.Type.abstract_injection info
      ; "Block.write_error", Lambda.Type.abstract_injection write_error ]

  let bind_err flow x f =
    x >>= function
    | Ok ()   -> f ()
    | Error e ->
      Log.err (fun l -> l "Got %a, closing" S.TCPV4.pp_write_error e);
      S.TCPV4.close flow

  let result: type a. a Lambda.Type.t -> a -> (a, _) result Lwt.t =
    fun ret x ->
      match ret with
      | Lambda.Type.Apply (ty, Lambda.Type.Lwt) ->
        let Lambda.Type.App x = x in
        let x = Lambda.Type.Lwt.prj x in
        x >|= fun x ->
        let x = Lambda.Expr.return x in
        Ok x
      | _ ->
        Lwt.return (Ok x)

  let err fmt = Fmt.kstrf (fun e -> Lwt.return (Error (`Msg e))) fmt
  let list_init n f =
    let rec go acc = function
      | 0 -> List.rev acc
      | n -> go (f n :: acc) (n - 1) in
    go [] n

  let eval ~block_size ~blocks ~gamma ~primitives request =
    let allocated_outputs = ref None in
    let request_extracted = ref None in

    (* XXX(dinosaure): order (and control flow) to set references is __very__
       important to know where computation leaks exception - and by this way, be
       able to respond the best error message to the client.

       TODO(dinosaure): wrap this stuff in result monad. *)

    try
      Log.info (fun l -> l "Parse protobuf request:\n\n%a%!" pp_string request);

      let request = Pbrt.Decoder.of_bytes (Bytes.unsafe_of_string request) in
      let request = Lambda_protobuf.Pb.decode_request request in

      request_extracted := Some request;

      let ast, ret, output = Lambda_protobuf.of_request ~gamma ~primitives request in
      let Lambda.Type.V ret = Lambda.Type.typ ret in
      let expected =
        Lambda.Type.(list AbstractTypes.cstruct
                     @-> list AbstractTypes.cstruct
                     @-> ret)
      in
      let outputs =
        list_init
          (Int64.to_int output)
          (fun _ -> Cstruct.create (Int64.to_int block_size))
      in

      allocated_outputs := Some (output, outputs);

      (match Lambda.type_and_eval ast expected with
       | Error e -> err "%a" Lambda.pp_error e
       | Ok f    -> result ret (f blocks outputs))
      >|= fun res ->

      let pp_value = Lambda.Type.pp_val ret in

      Logs.info (fun l -> l "Process and eval: %a => %a.\n%!"
                    Lambda.Parsetree.pp ast
                    (Fmt.Dump.result ~ok:pp_value ~error:Rresult.R.pp_msg) res);

      let res = Rresult.R.map (Lambda.uncast ret) res in
      let res = Lambda_protobuf.to_reply res in
      let encoder = Lambda_protobuf.Rpc.Encoder.(default Reply res block_size output) in

      Ok (outputs, encoder)
    with exn -> match !allocated_outputs, !request_extracted with
      | Some (output, outputs), _ ->
        (* XXX(dinosaure): exception leaks after [Lambda_protobuf.of_request]. *)
        Logs.err (fun l -> l "Got an error with allocated outputs: %a" Fmt.exn exn);
        let res = Lambda_protobuf.to_reply (Rresult.R.error_msgf "%a" Fmt.exn exn) in
        let encoder = Lambda_protobuf.Rpc.Encoder.(default Reply res block_size output) in
        Lwt.return_ok (outputs, encoder)
      | None, Some request ->
        (* XXX(dinosaure): exceptions leaks after [Lambda_protobuf.Pb.decode_request] and
           before [Lambda_protobuf.of_request]. *)
        let output = Lambda_protobuf.output_of_request request in
        let outputs =
          list_init
            (Int64.to_int output)
            (fun _ -> Cstruct.create (Int64.to_int block_size)) in
        let res = Lambda_protobuf.to_reply (Rresult.R.error_msgf "%a" Fmt.exn exn) in
        let encoder = Lambda_protobuf.Rpc.Encoder.(default Reply res block_size output) in
        Lwt.return_ok (outputs, encoder)
      | None, None ->
        (* XXX(dinosaure): exception leaks before [Lambda_protobuf.Pb.decode_request]. *)
        Logs.err (fun l -> l "Got an error: %a" Fmt.exn exn);
        err "%a" Fmt.exn exn

  let send flow (blocks, encoder) =
    let (>>?) = bind_err flow in
    let open Lambda_protobuf in

    let src = Cstruct.create 0x8000 in
    let dst = Cstruct.create 0x8000 in

    let rec loop encoder = match Rpc.Encoder.eval src dst encoder with
      | `Await t ->
        let block_n, block_consumed = Rpc.Encoder.block t in
        let len =
          min
            (Cstruct.len src)
            (Cstruct.len (List.nth blocks (Int64.to_int block_n)) - block_consumed)
        in
        Cstruct.blit (List.nth blocks (Int64.to_int block_n)) block_consumed src 0 len;
        loop (Rpc.Encoder.refill 0 len t)

      | `Flush t ->
        S.TCPV4.write flow (Cstruct.sub dst 0 (Rpc.Encoder.used_out t)) >>? fun () ->
        loop (Rpc.Encoder.flush 0 (Cstruct.len dst) t)
      | `Error (_, err) ->
        Log.err (fun f ->
            f "Retrieve an error when we encode reply: %a." Rpc.Encoder.pp_error err);
        Lwt.return_unit
      | `End t ->
        (if Rpc.Encoder.used_out t > 0
         then S.TCPV4.write flow (Cstruct.sub dst 0 (Rpc.Encoder.used_out t))
         else Lwt.return_ok ()) >>? fun () ->
        Lwt.return_unit in
    loop encoder

  let process ~gamma ~primitives flow =
    let dst, dst_port = S.TCPV4.dst flow in

    Logs.info (fun f ->
        f "new tcp connection from IP %a on port %d"
          Ipaddr.V4.pp_hum dst dst_port);

    let buffer = Buffer.create 512 in
    let block_buffer = Cstruct_buffer.create 512 in
    let decoder = Lambda_protobuf.Rpc.(Decoder.default Request) in

    let (>>?) v f = v >>= function
      | Ok v -> f v
      | Error (`Msg err) ->
        Log.err (fun f -> f "Got an evaluation error: %s." err);
        Lwt.return_unit in

    let rec loop blocks decoder =
      S.TCPV4.read flow >>= function
      | Ok `Eof ->
        Logs.info (fun f -> f "Closing connection!");
        Lwt.return_unit
      | Error e ->
        Logs.warn (fun f ->
            f "Error reading data from established connection: %a"
              S.TCPV4.pp_error e);
        Lwt.return_unit
      | Ok (`Data src) ->

        let rec go blocks decoder =
          Log.info (fun f -> f "State of the decoder: %a." Lambda_protobuf.Rpc.Decoder.pp decoder);

          match Lambda_protobuf.Rpc.Decoder.eval src decoder with
          | `Await decoder -> Lwt.return (decoder, blocks)
          | `Flush (decoder, `Protobuf, raw) ->
            Buffer.add_string buffer (Cstruct.to_string raw);
            go blocks (Lambda_protobuf.Rpc.Decoder.flush decoder)
          | `Flush (decoder, `Block n, raw) ->
            let blocks =
              if Int64.to_int n = List.length blocks then blocks
              else
                (Log.info (fun f ->
                     f "Retrieve block %d:\n\n%a\n%!"
                       (List.length blocks)
                       pp_string (Cstruct_buffer.contents block_buffer))
                ; let block = Cstruct_buffer.contents block_buffer in
                  Cstruct_buffer.clear block_buffer
                ; block :: blocks) in

            Cstruct_buffer.add block_buffer raw;

            go blocks (Lambda_protobuf.Rpc.Decoder.flush decoder)
          | `Error (decoder, err) ->
            Logs.warn (fun f ->
                f "Retrieve an error: %a."
                  Lambda_protobuf.Rpc.Decoder.pp_error err);
            Lwt.return (Lambda_protobuf.Rpc.Decoder.reset decoder, [])
          | `End decoder ->

            let blocks =
              if Cstruct_buffer.has block_buffer > 0
              then (Log.info (fun f ->
                  f "Retrieve block %d:\n%a\n%!"
                    (List.length blocks)
                    pp_string (Cstruct_buffer.contents block_buffer))
                   ; let block = Cstruct_buffer.contents block_buffer in
                     Cstruct_buffer.clear block_buffer
                   ; block :: blocks)
              else blocks in

            eval
              ~block_size:(Lambda_protobuf.Rpc.Decoder.block_size decoder)
              ~blocks:(List.map (fun x -> Cstruct.of_string x) (List.rev blocks))
              ~gamma
              ~primitives
              (Buffer.contents buffer)
            >>? send flow >|= fun () ->
            Buffer.clear buffer;
            (Lambda_protobuf.Rpc.Decoder.reset decoder, [])
        in

        go blocks (Lambda_protobuf.Rpc.Decoder.refill 0 (Cstruct.len src) decoder)
        >>= fun (decoder, blocks) ->
        loop blocks decoder
    in
    loop [] decoder

  let start b s () =
    let primitives, gamma = make_environment b in

    let port = Key_gen.port () in
    S.listen_tcpv4 s ~port (process ~gamma ~primitives);
    S.listen s

end
