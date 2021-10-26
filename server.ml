open Common
open Lwt
open Unix

let accept_connection conn =
  let fd, _ = conn in
  let ic = Lwt_io.of_fd ~mode:Lwt_io.Input fd in
  let oc = Lwt_io.of_fd ~mode:Lwt_io.Output fd in
  handle_connection ic oc ()

let create_socket addr () =
  let open Lwt_unix in
  let sock = socket PF_INET SOCK_STREAM 0 in
  let _ = bind sock @@ addr in
  listen sock 1;
  sock

let create_server sock =
  let rec serve () = Lwt_unix.accept sock >>= accept_connection >>= serve in
  serve

let () =
  let port = ref 9000 in
  let speclist = [ ("-p", Arg.Set_int port, "Set port") ] in
  Arg.parse speclist (fun _ -> ()) "";

  (* start server *)
  let addr = ADDR_INET (inet_addr_loopback, !port) in
  let sock = create_socket addr () in
  let serve = create_server sock in
  Lwt_main.run @@ serve ()
