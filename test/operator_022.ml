let () =
  snoo ~f:(fun foo ->
         foo
         && snoo)
