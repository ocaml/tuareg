let () =
  (
    foo bar
  )
  >>= fun () ->
  (
    foo
      bar
  )
  >>= fun () ->
  ()
