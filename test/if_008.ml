let f errors input =
  let ( @@ ) string bool = if not bool then errors := string :: !errors in
  input @@ false
