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

let pp_string = pp_scalar ~get:String.get ~length:String.length

let load_file filename =
  let ic = open_in filename in
  let ln = in_channel_length ic in
  let rs = Bytes.create ln in
  really_input ic rs 0 ln;
  Bytes.unsafe_to_string rs

let eval ?(primitives = []) s =
  match Lambda.Request.parse ~primitives s with
  | Error _ as e -> e
  | Ok (ast, typ) ->
    let Lambda.Type.V typ = Lambda.Type.typ typ in
    match Lambda.type_and_eval ast typ with
    | Ok value -> Ok (Lambda.uncast typ value)
    | Error e -> Fmt.kstrf (fun e -> Error (`Msg e)) "%a" Lambda.pp_error e

let request ~block_n ~block_size ?(primitives = []) s =
  match Lambda.Request.parse ~primitives s with
  | Error _ as e -> e
  | Ok v ->
    let (ast, typ) = v in
    Fmt.(pf stdout) "Ready to send: %a:%a.\n%!" Lambda.Parsetree.pp ast Lambda.Parsetree.Type.pp typ;
    let encoder = Lambda_protobuf.Rpc.Encoder.default (Lambda_protobuf.make v) (Int64.of_int block_size) (Int64.of_int block_n) in
    Ok encoder

let make_socket addr port =
  Printf.printf "Connecting to %s:%d\n%!" addr port;
  let inet_addr = Unix.(gethostbyname addr).h_addr_list.(0) in
  let socket = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.connect socket Unix.(ADDR_INET (inet_addr, port));
  Unix.out_channel_of_descr socket, Unix.in_channel_of_descr socket

let send_request oc blocks encoder =
  let open Lambda_protobuf in

  let src = Cstruct.create 0x8000 in
  let dst = Cstruct.create 0x8000 in

  let rec go encoder =
    Format.printf "Evaluation of: %a.\n%!" Rpc.Encoder.pp encoder;

    match Rpc.Encoder.eval src dst encoder with
    | `Await t ->
      Fmt.(pf stdout) "> Await.\n%!";

      let block_n, block_consumed = Rpc.Encoder.block t in
      let len = min (Cstruct.len dst) (String.length blocks.(Int64.to_int block_n) - block_consumed) in
      Cstruct.blit_from_string blocks.(Int64.to_int block_n) block_consumed src 0 len;

      go (Rpc.Encoder.refill 0 len t)
    | `Flush t ->
      Fmt.(pf stdout) "> Flush.\n%!";

      let chunk = Cstruct.to_string dst in
      output_substring oc chunk 0 (Rpc.Encoder.used_out t);

      Fmt.(pf stdout) "Send a chunk:\n\n%a\n%!" pp_string (String.sub chunk 0 (Rpc.Encoder.used_out t));

      go (Rpc.Encoder.flush 0 0x8000 t)
    | `Error (_, err) ->
      Fmt.epr "Retrieve an error: %a\n%!" Rpc.Encoder.pp_error err
    | `End t ->
      Fmt.(pf stdout) "> End: %a.\n%!" Rpc.Encoder.pp t;

      if Rpc.Encoder.used_out t > 0
      then (let chunk = Cstruct.to_string dst in output_substring oc chunk 0 (Rpc.Encoder.used_out t));

      Fmt.(pf stdout) "Send a chunk:\n\n%a\n%!" pp_string (String.sub (Cstruct.to_string dst) 0 (Rpc.Encoder.used_out t));

      flush oc in
  go encoder

let repl ?(block_n = 0) ?(block_size = 0) socket blocks =
  let rec go () =
    output_string stdout "# ";
    flush stdout;

    match input_line stdin with
    | expr ->
      (match request ~block_n ~block_size expr with
       | Ok encoder ->
         send_request socket blocks encoder;
         Fmt.(pf stdout) "Send a lambda-calculus expression.\n%!";
         go ()
       | Error (`Msg e) -> Fmt.epr "Retrieve an error: %s" e)
    | exception End_of_file -> () in go ()

let main host port blocks =
  let blocks = List.map load_file blocks in
  let socket, _ = make_socket host port in

  let status =
    let lens = List.map String.length blocks in

    try
      if List.for_all ((=) (List.hd lens)) lens
      then Ok (Some (List.length blocks, List.hd lens))
      else Error (Rresult.R.msgf "Size of blocks mismatchs")
    with _ -> Ok None in

  match status with
  | Ok (Some (block_n, block_size)) ->
    repl ~block_n ~block_size socket (Array.of_list blocks); `Ok ()
  | Ok None ->
    repl socket [||]; `Ok ()
  | Error (`Msg err) -> `Error (true, err)

open Cmdliner

let port =
  let doc = "Port to the unikernel" in
  Arg.(value & opt int 1234 & info ["p"; "port"] ~doc ~docv:"<port>")

let host =
  let doc = "Hostname to the unikernel" in
  Arg.(value & opt string "localhost" & info ["h"; "host"] ~doc ~docv:"<host>")

let file =
  let parser s =
    if Sys.file_exists s
    then Ok s else Error (Rresult.R.msgf "%s does not exist" s) in
  let pp = Fmt.string in
  Arg.conv (parser, pp)

let blocks =
  let doc = "List of blocks to send to the unikernel" in
  Arg.(value & opt (list file) [] & info ["blocks"] ~doc ~docv:"<list>")

let cmd =
  let doc = "Binary example to communicate with unikernel." in
  let exits = Term.default_exits in
  Term.(ret (const main $ host $ port $ blocks)),
  Term.info "main" ~version:"<none>" ~doc ~exits

let () = Term.(exit @@ eval cmd)
