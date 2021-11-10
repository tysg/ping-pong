open Common
open Unix

let start_connection addr =
  (* automatically cleans up upon handle connection returns *)
  Lwt_io.with_connection addr (fun (ic, oc) -> handle_connection ic oc)

let () =
  let port = ref "9000" in
  let host = ref "127.0.0.1" in

  let speclist =
    [ ("-p", Arg.Set_string port, "Set port, defaults to 9000") ]
  in
  Arg.parse speclist (fun input_host -> host := input_host) "";

  (* start client *)
  let infos = getaddrinfo !host !port [] in
  match infos with
  | [] ->
      prerr_endline ("cannot find the address of " ^ !host ^ ":" ^ !port);
      exit 1
  | info :: _ ->
      exit_on_error
        (fun () -> Lwt_main.run (start_connection info.ai_addr))
        ("error occured when connecting to " ^ !host ^ ":" ^ !port ^ ": ")
