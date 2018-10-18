let () =
  snoo ~f:(fun foo ->
         foo + bar
         && snoo)
