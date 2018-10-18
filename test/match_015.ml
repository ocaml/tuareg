let () =
  match
    (a >>= fun a ->
     b >>= fun b ->
     c)
  with
    A -> _
