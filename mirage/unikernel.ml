open Lwt.Infix

module type BLOCK = Mirage_block_lwt.S
module type TCP = Mirage_stack_lwt.V4

[@@@warning "-40"]

let log_src = Logs.Src.create "lambda" ~doc:"lambda"
module Log = (val Logs.src_log log_src : Logs.LOG)

module Main (B: BLOCK) (S: TCP) = struct

  let make_environment b =
    let open Lambda_protobuf in
    let cstruct     = Lambda.Type.abstract "Cstruct.t"         in
    let formatter   = Lambda.Type.abstract "Format.formatter"  in
    let error       = Lambda.Type.abstract "Block.error"       in
    let write_error = Lambda.Type.abstract "Block.write_error" in
    let info        = Lambda.Type.abstract "Mirage_block.info" in

    List.fold_left
      (fun primitives (k, v) -> match v with
         | Lambda.Parsetree.Prm primitive -> Primitives.add k primitive primitives
         | _ -> Fmt.invalid_arg "Invalid expression as primitive")
      Primitives.empty
      Lambda.[ primitive   "Block.pp_error"           [ formatter; error; ]                     Type.unit                            B.pp_error
             ; primitive   "Block.pp_write_error"     [ formatter; write_error; ]               Type.unit                            B.pp_write_error
             ; L.primitive "Block.disconnect"         []                                        Type.(lwt unit)                      B.(disconnect b)
             ; L.primitive "Block.get_info"           []                                        Type.(lwt info)                      B.(get_info b)
             ; primitive   "read_write"               [ info ]                                  Type.bool                            (fun b -> b.Mirage_block.read_write)
             ; primitive   "sector_size"              [ info ]                                  Type.int                             (fun b -> b.Mirage_block.sector_size)
             ; primitive   "size_sectors"             [ info ]                                  Type.int64                           (fun b -> b.Mirage_block.size_sectors)
             ; L.primitive "Block.read"               Type.[ int64; list cstruct; ]             Type.(lwt (result unit error))       B.(read b)
             ; L.primitive "Block.write"              Type.[ int64; list cstruct; ]             Type.(lwt (result unit write_error)) B.(write b)
             ; primitive   "Cstruct.to_string"        [ cstruct ]                               Type.string                          Cstruct.to_string
             ; primitive   "Cstruct.of_string"        [ Type.string ]                           cstruct                              (fun s -> Cstruct.of_string s)
             ; primitive   "Cstruct.blit"             Type.[ cstruct; int; cstruct; int; int; ] Type.unit                            Cstruct.blit
             ; primitive   "Cstruct.blit_to_string"   Type.[ cstruct; int; bytes; int; int; ]   Type.unit                            Cstruct.blit_to_bytes
             ; primitive   "Cstruct.blit_from_string" Type.[ string; int; cstruct; int; int; ]  Type.unit                            Cstruct.blit_from_string ],
    List.fold_left
      (fun gamma (k, v) -> Gamma.add k v gamma)
      Gamma.empty
      [ "Format.formatter",  Lambda.Type.abstract_projection formatter
      ; "Block.error",       Lambda.Type.abstract_projection error
      ; "Cstruct.t",         Lambda.Type.abstract_projection cstruct
      ; "Mirage_block.info", Lambda.Type.abstract_projection info
      ; "Block.write_error", Lambda.Type.abstract_projection write_error ]

  let bind_err flow x f =
    x >>= function
    | Ok ()   -> f ()
    | Error e ->
      Log.err (fun l -> l "Got %a, closing" S.TCPV4.pp_write_error e);
      S.TCPV4.close flow

  let eval ~gamma ~primitives request =
    try
      let request = Pbrt.Decoder.of_bytes (Bytes.unsafe_of_string request) in
      let request = Lambda_protobuf.Pb.decode_request request in
      let ast, typ = Lambda_protobuf.request ~gamma ~primitives request in
      let Lambda.Type.V typ = Lambda.Type.typ typ in

      let res = match Lambda.type_and_eval ast typ with
        | Ok _ as x -> x
        | Error e -> Fmt.kstrf (fun e -> Error (`Msg e)) "%a" Lambda.pp_error e in

      let pp_value = Lambda.Type.pp_val typ in

      Logs.info (fun l -> l "Process and eval: %a => %a.\n%!"
                    Lambda.Parsetree.pp ast (Fmt.result ~ok:pp_value ~error:Rresult.R.pp_msg) res);
      Ok ()
    with exn ->
      Logs.err (fun l -> l "Retrieve an error: %s" (Printexc.to_string exn));
      Error (`Msg (Printexc.to_string exn))

  let process ~gamma ~primitives flow =
    let dst, dst_port = S.TCPV4.dst flow in

    Logs.info (fun f ->
        f "new tcp connection from IP %a on port %d"
          Ipaddr.V4.pp_hum dst dst_port);

    let (>>?) = bind_err flow in
    let buffer = Buffer.create 512 in
    let decoder = Lambda_protobuf.Rpc.Decoder.default () in

    let rec loop decoder =
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
        Logs.info (fun l -> l "We receive something.");

        let rec go decoder =
          Logs.info (fun l -> l "Loop back on the un-serialization.");

          match Lambda_protobuf.Rpc.Decoder.eval src decoder with
          | `Await decoder ->
            Logs.debug (fun l -> l "Await more input.");
            decoder
          | `Flush (decoder, `Protobuf, raw) ->
            Logs.debug (fun l -> l "Flush protobuf request.");
            Buffer.add_string buffer (Cstruct.to_string raw);
            go (Lambda_protobuf.Rpc.Decoder.flush decoder)
          | `Flush (decoder, `Block n, raw) ->
            Logs.debug (fun l -> l "Flush block %Ld." n);
            go (Lambda_protobuf.Rpc.Decoder.flush decoder)
          | `Error (decoder, err) ->
            Logs.debug (fun l -> l "Retrieve an error: %a." Lambda_protobuf.Rpc.Decoder.pp_error err);
            (Lambda_protobuf.Rpc.Decoder.reset decoder)
          | `End decoder ->
            eval ~gamma ~primitives (Buffer.contents buffer) |> fun res ->
            Buffer.clear buffer;
            (Lambda_protobuf.Rpc.Decoder.reset decoder) in

        let decoder = go (Lambda_protobuf.Rpc.Decoder.refill 0 (Cstruct.len src) decoder) in
        loop decoder
    in loop decoder

  let start b s () =
    let primitives, gamma = make_environment b in

    let port = Key_gen.port () in
    S.listen_tcpv4 s ~port (process ~gamma ~primitives);
    S.listen s

end
