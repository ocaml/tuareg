let () =
  foobar (fun () ->
      step1
      >>= fun () -> step2)
