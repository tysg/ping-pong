open Lwt

type msg_packet = Msg of string | Ack

let write_msg oc msg =
  match msg with
  | Ack -> Lwt_io.write_line oc "0"
  | Msg msg -> Lwt_io.write_line oc ("1" ^ msg)

let read_msg ic =
  let%lwt indicator = Lwt_io.read_line_opt ic in
  match indicator with
  | None -> return_none
  | Some line -> (
      match line.[0] with
      | '0' -> return (Some Ack)
      | '1' -> return (Some (Msg (String.sub line 1 (String.length line - 1))))
      | _ -> fail_with "unexepcted indicator")

let rec sender oc ac () =
  let%lwt msg = Lwt_io.read_line Lwt_io.stdin in
  let start_time = Unix.gettimeofday () in
  let%lwt () = write_msg oc (Msg msg) in
  let%lwt () = Lwt_mvar.take ac in
  let end_time = Unix.gettimeofday () in

  let%lwt () =
    Lwt_io.write_line Lwt_io.stdout
      ("Received ACK, RTT (in ms): " ^ string_of_float (end_time -. start_time))
  in
  sender oc ac ()

let rec responder ic oc ac () =
  let%lwt msg = read_msg ic in
  match msg with
  | None -> return ()
  | Some Ack ->
      let%lwt () = Lwt_mvar.put ac () in
      responder ic oc ac ()
  | Some (Msg msg) ->
      let%lwt () = write_msg oc Ack in
      let%lwt () = Lwt_io.write_line Lwt_io.stdout msg in
      responder ic oc ac ()

let handle_connection ic oc () =
  let ac = Lwt_mvar.create_empty () in
  pick [ responder ic oc ac (); sender oc ac () ]

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
