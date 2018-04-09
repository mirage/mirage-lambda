open Lwt.Infix

module type BLOCK = Mirage_block_lwt.S
module type TCP = Mirage_stack_lwt.V4


let log_src = Logs.Src.create "lambda" ~doc:"lambda"
module Log = (val Logs.src_log log_src : Logs.LOG)

module Main (B: BLOCK) (S: TCP) = struct

  let primitives b =
    (* XXX(samoht): we need support for more basic types *)
    let open Lambda in
    let page_aligned_buffer = Type.abstract "Cstruct.t" in
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
      L.primitive "Block.get_info" [] Type.(lwt info) B.(get_info b);
      L.primitive "Block.read"
        Type.[int64; list page_aligned_buffer]
        Type.(lwt (result unit error))
        B.(read b);
      L.primitive "Block.write"
        Type.[int64; list page_aligned_buffer]
        Type.(lwt (result unit write_error))
        B.(write b)
    ]

  let eval b s =
    match Lambda.parse ~primitives:(primitives b) s with
    | Error _ as e -> e
    | Ok ast ->
      match Lambda.type_and_eval ast Lambda.Type.int with
      | Ok _ as x -> x
      | Error e   -> Fmt.kstrf (fun e -> Error (`Msg e)) "%a" Lambda.pp_error e

  let prompt = "# "

  let bind_err flow x f =
    x >>= function
    | Ok ()   -> f ()
    | Error e ->
      Log.err (fun l -> l "Got %a, closing" S.TCPV4.pp_write_error e);
      S.TCPV4.close flow

  let start b s () =
    let port = Key_gen.port () in
    S.listen_tcpv4 s ~port (fun flow ->
        let dst, dst_port = S.TCPV4.dst flow in
        Logs.info (fun f ->
            f "new tcp connection from IP %a on port %d"
              Ipaddr.V4.pp_hum dst dst_port);
        let (>>?) = bind_err flow in
        let rec loop () =
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
          | Ok (`Data q) ->
            Logs.debug (fun f ->
                f "read: %d bytes:\n%s" (Cstruct.len q) (Cstruct.to_string q));
            let reply = match eval b (Cstruct.to_string q) with
              | Ok x -> Fmt.to_to_string Lambda.Type.(pp_val int) x
              | Error (`Msg e) -> e
            in
            let msg = Cstruct.of_string (reply ^ "\n") in
            S.TCPV4.write flow msg >>? fun () ->
            loop ()
        in
        loop ()
      );
    S.listen s

end
