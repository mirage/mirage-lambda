open Lwt.Infix

module type BLOCK = Mirage_block_lwt.S
module type TCP = Mirage_stack_lwt.V4

[@@@warning "-40"]

let log_src = Logs.Src.create "lambda" ~doc:"lambda"
module Log = (val Logs.src_log log_src : Logs.LOG)

module Main (B: BLOCK) (S: TCP) = struct

  let primitives b =
    (* XXX(samoht): we need support for more basic types *)
    let open Lambda in
    let cstruct = Type.abstract "Cstruct.t" in
    let error = Type.abstract "Block.error" in
    let write_error = Type.abstract "Block.write_error" in
    let formatter = Type.abstract "Format.formatter" in
    (* XXX(samoht): add first-class support for records *)
    let info = Type.abstract "Mirage_block.info" in
    [
      primitive "Block.pp_error" [formatter; error] Type.unit B.pp_error;
      primitive "Block.pp_write_error"
        [formatter; write_error] Type.unit B.pp_write_error;
      L.primitive "Block.disconnect" [] Type.(lwt unit) B.(disconnect b);

      (* info *)
      L.primitive "Block.get_info" [] Type.(lwt info) B.(get_info b);
      primitive "read_write" [info] Type.bool
        (fun b -> b.Mirage_block.read_write);
      primitive "sector_size" [info] Type.int
        (fun b -> b.Mirage_block.sector_size);
      primitive "size_sectores" [info] Type.int64
        (fun b -> b.Mirage_block.size_sectors);

      L.primitive "Block.read"
        Type.[int64; list cstruct]
        Type.(lwt (result unit error))
        B.(read b);
      L.primitive "Block.write"
        Type.[int64; list cstruct]
        Type.(lwt (result unit write_error))
        B.(write b);

      (* cstruct *)
      primitive "Cstruct.to_string" [cstruct] Type.string Cstruct.to_string;
      primitive "Cstruct.of_string" [Type.string] cstruct Cstruct.of_string;
      primitive "Cstruct.blit" Type.[cstruct; int; cstruct; int; int] Type.unit
        Cstruct.blit;
      primitive "Cstruct.blit_to_string" Type.[cstruct; int; bytes; int; int]
        Type.unit Cstruct.blit_to_bytes;
      primitive "Cstruct.blit_from_string" Type.[bytes; int; cstruct; int; int]
        Type.unit Cstruct.blit_from_bytes;
    ]

  let eval b s =
    match Lambda.parse ~primitives:(primitives b) s with
    | Error _ as e -> e
    | Ok ast ->
      match Lambda.type_and_eval ast Lambda.Type.int with
      | Ok _ as x -> x
      | Error e   -> Fmt.kstrf (fun e -> Error (`Msg e)) "%a" Lambda.pp_error e

  let prompt = "# "

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
      ; "Block.get_info",    Lambda.Type.abstract_projection info
      ; "Block.write_error", Lambda.Type.abstract_projection write_error ]

  let bind_err flow x f =
    x >>= function
    | Ok ()   -> f ()
    | Error e ->
      Log.err (fun l -> l "Got %a, closing" S.TCPV4.pp_write_error e);
      S.TCPV4.close flow

  let start b s () =
    let primitives, gamma = make_environment b in

    let port = Key_gen.port () in
    S.listen_tcpv4 s ~port (fun flow ->
        let dst, dst_port = S.TCPV4.dst flow in
        Logs.info (fun f ->
            f "new tcp connection from IP %a on port %d"
              Ipaddr.V4.pp_hum dst dst_port);
        let (>>?) = bind_err flow in
        let buffer = Buffer.create 512 in
        let decoder = Lambda_protobuf.Rpc.Decoder.default () in
        let rec loop decoder =
          S.TCPV4.write flow (Cstruct.of_string prompt) >>? fun () ->
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
            let rec go decoder = match Lambda_protobuf.Rpc.Decoder.eval src decoder with
              | `Await decoder -> decoder
              | `Flush (decoder, `Protobuf, raw) ->
                Buffer.add_string buffer (Cstruct.to_string raw);
                go (Lambda_protobuf.Rpc.Decoder.flush decoder)
              | `Flush (decoder, `Block n, raw) ->
                go (Lambda_protobuf.Rpc.Decoder.flush decoder)
              | `Error (decoder, err) ->
                Logs.err (fun l -> l "retrieve an error: %a" Lambda_protobuf.Rpc.Decoder.pp_error err);
                (Lambda_protobuf.Rpc.Decoder.reset decoder)
              | `End decoder ->
                let request = Buffer.contents buffer in
                let request = Pbrt.Decoder.of_bytes (Bytes.unsafe_of_string request) in
                let request = Lambda_protobuf.Pb.decode_request request in
                let ast, typ = Lambda_protobuf.request ~gamma ~primitives request in
                let typ = Lambda.Type.typ typ in

                let res = match Lambda.type_and_eval ast Lambda.Type.int with
                 | Ok _ as x -> x
                 | Error e   -> Fmt.kstrf (fun e -> Error (`Msg e)) "%a" Lambda.pp_error e in

                Buffer.clear buffer;
                (Lambda_protobuf.Rpc.Decoder.reset decoder) in
            let decoder = go (Lambda_protobuf.Rpc.Decoder.refill 0 (Cstruct.len src) decoder) in
            Logs.debug (fun f -> f "read: %d bytes:\n%s" (Cstruct.len src) (Cstruct.to_string src));
            let msg = Cstruct.of_string "ack" in
            S.TCPV4.write flow msg >>? fun () -> loop decoder
        in loop decoder);
    S.listen s

end
