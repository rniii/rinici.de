let chat_history, send_message =
  (Lwt_stream.create () : string Lwt_stream.t * _)

let tz_offset_s = -3 * 60 * 60

let timestamp () =
  let time = Unix.time () |> Ptime.of_float_s |> Option.get in
  let _, ((hh, mm, _), _) = Ptime.to_date_time ~tz_offset_s time in
  Printf.sprintf "%02i:%02i" hh mm
