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
      (* a malicious input won't put the server in an undesirable state *)
      | _ -> return_none)

let rec sender oc ac =
  let%lwt msg = Lwt_io.read_line_opt Lwt_io.stdin in
  match msg with
  | None -> return ()
  | Some msg ->
      let start_time = Unix.gettimeofday () in
      let%lwt () = write_msg oc (Msg msg) in
      let%lwt () = Lwt_mvar.take ac in
      let end_time = Unix.gettimeofday () in

      let%lwt () =
        Lwt_io.write_line Lwt_io.stdout
          ("Received ACK, RTT (in ms): "
          ^ string_of_float (end_time -. start_time))
      in
      sender oc ac

let rec responder ic oc ac =
  let%lwt msg = read_msg ic in
  match msg with
  | None -> return ()
  | Some Ack ->
      let%lwt () = Lwt_mvar.put ac () in
      responder ic oc ac
  | Some (Msg msg) ->
      let%lwt () = write_msg oc Ack in
      let%lwt () = Lwt_io.write_line Lwt_io.stdout msg in
      responder ic oc ac

let handle_connection ic oc =
  let ac = Lwt_mvar.create_empty () in
  let%lwt () = pick [ responder ic oc ac; sender oc ac ] in
  Lwt_io.close oc

let exit_on_error fn msg =
  let open Unix in
  try fn () with
  | Unix_error (err, _, _) ->
      prerr_endline (msg ^ error_message err);
      exit 1
  | err ->
      prerr_endline (msg ^ Printexc.to_string err);
      exit 1
