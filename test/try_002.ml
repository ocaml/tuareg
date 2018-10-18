let g x =
  try let a = b in
      f x;
      g x;
      y x;
  with e -> raise e
