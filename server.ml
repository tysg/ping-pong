open Common
open Unix

let accept_connection conn =
  let fd, _ = conn in
  let ic = Lwt_io.of_fd ~mode:Lwt_io.Input fd in
  let oc = Lwt_io.of_fd ~mode:Lwt_io.Output fd in
  handle_connection ic oc

let create_socket addr =
  let open Lwt_unix in
  let sock = socket PF_INET SOCK_STREAM 0 in
  let _ = bind sock @@ addr in
  listen sock 1;
  sock

let rec serve_connection sock =
  let%lwt conn = Lwt_unix.accept sock in
  let%lwt () = accept_connection conn in
  serve_connection sock

let () =
  let port = ref 9000 in
  let speclist = [ ("-p", Arg.Set_int port, "Set port") ] in
  Arg.parse speclist (fun _ -> ()) "";

  (* start server *)
  let addr = ADDR_INET (inet_addr_loopback, !port) in
  let sock =
    try create_socket addr
    with Unix_error (err, _, _) ->
      prerr_endline ("error in creating socket: " ^ error_message err);
      exit 1
  in
  exit_on_error
    (fun () ->
      Lwt_main.run
        (Lwt.finalize
           (fun () -> serve_connection sock)
           (fun () -> Lwt_unix.close sock)))
    "error occured when serving connection: "
