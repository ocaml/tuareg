let test () =                           (* bug#927 *)
  if a then
    if b then x
    else if c then y
    else z
  else something
