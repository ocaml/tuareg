let () =
  a
  >>= fun () ->
  b
  >>| fun () ->
  Deferred.all
