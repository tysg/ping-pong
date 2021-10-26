open Common
open Unix

let () =
  let port = ref 9000 in
  let host = ref "127.0.0.1" in

  let speclist =
    [
      ("-p", Arg.Set_int port, "Set port, defaults to 9000");
      ("-t", Arg.Set_string host, "Set target host, defaults to localhost");
    ]
  in
  Arg.parse speclist (fun _ -> ()) "";

  (* start client *)
  let addr = ADDR_INET (inet_addr_of_string !host, !port) in
  Lwt_main.run
    (let%lwt ic, oc = Lwt_io.open_connection addr in
     handle_connection ic oc ())
