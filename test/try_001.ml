let r x =
  try  f x;
       g x;
       y x;
  with e -> raise e
