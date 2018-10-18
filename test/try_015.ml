let x =
  try a
  with Not_found ->
        if a then b
     | flag when String.is_prefix flag ~prefix:"-" ->
        a
     | _ ->
        c
