open Common
open Lwt
open Unix

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
