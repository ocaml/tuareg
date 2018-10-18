let () =
  begin
    foo bar
  end
  >>= fun () ->
  begin
    foo
      bar
  end
  >>= fun () ->
  ()
