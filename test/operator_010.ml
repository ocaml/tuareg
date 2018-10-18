let () =
  Config.load ()
  >>= fun config ->
  let quux = x in
  x
  >>= fun quux ->
  x
