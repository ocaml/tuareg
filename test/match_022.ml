(* TODO: *)
let subscribe_impl dir topic ~aborted =
  return (
      match Directory.subscribe dir topic with
      | None -> Error ()
      | Some pipe ->
         whenever (aborted >>| fun () -> Pipe.close_read pipe);
         Ok pipe
    )
         next_argument (* should be indented correctly, given the braces *)
