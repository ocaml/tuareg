let w f =
  List.map f ~f:(fun (a, b) ->
             L.r a
             >>= function
             | Ok s -> `Fst (b, s)
             | Error e -> `Snd (b, a, e))
