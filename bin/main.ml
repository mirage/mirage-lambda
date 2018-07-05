let eval ?(primitives = []) s =
  match Lambda.Request.parse ~primitives s with
  | Error _ as e -> e
  | Ok (ast, typ) ->
    let Lambda.Type.V typ = Lambda.Type.typ typ in
    match Lambda.type_and_eval ast typ with
    | Ok value -> Ok (Lambda.uncast typ value)
    | Error e -> Fmt.kstrf (fun e -> Error (`Msg e)) "%a" Lambda.pp_error e

let request ?(primitives = []) s =
  match Lambda.Request.parse ~primitives s with
  | Error _ as e -> e
  | Ok v ->
    let (ast, typ) = v in
    Fmt.(pf stdout) "Ready to send: %a:%a.\n%!" Lambda.Parsetree.pp ast Lambda.Parsetree.Type.pp typ;
    let encoder = Lambda_protobuf.Rpc.Encoder.default (Lambda_protobuf.make v) 0L 0L in
    Ok encoder

let make_socket addr port =
  Printf.printf "Connecting to %s:%d\n$!" addr port;
  let inet_addr = Unix.(gethostbyname addr).h_addr_list.(0) in
  let socket = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.connect socket Unix.(ADDR_INET (inet_addr, port));
  Unix.out_channel_of_descr socket, Unix.in_channel_of_descr socket

let send_request oc encoder =
  let open Lambda_protobuf in

  let src = Cstruct.create 0 in
  let dst = Cstruct.create 0x8000 in

  let rec go encoder = match Rpc.Encoder.eval src dst encoder with
    | `Await _ -> assert false
    | `Flush t ->
      let chunk = Cstruct.to_string dst in
      output_substring oc chunk 0 (Rpc.Encoder.used_out t);

      Fmt.(pf stdout) "Send a chunk: %S.\n%!" (String.sub chunk 0 (Rpc.Encoder.used_out t));

      go (Rpc.Encoder.flush 0 0x8000 t)
    | `Error (_, err) ->
      Fmt.epr "Retrive an error: %a\n%!" Rpc.Encoder.pp_error err
    | `End _ -> flush oc in
  go encoder

let repl socket =
  let rec go () =
    output_string stdout "# ";
    flush stdout;

    match input_line stdin with
    | expr ->
      (match request expr with
       | Ok encoder ->
         send_request socket encoder;
         Fmt.(pf stdout) "Send a lambda-calculus expression.\n%!";
         go ()
       | Error (`Msg e) -> Fmt.epr "Retrieve an error: %s" e)
    | exception End_of_file -> () in go ()

let () =
  let oc, _ = make_socket "localhost" 1234 in
  repl oc
