let () =
  match e with
  | `T d ->
     notify `O `T d;
     cancel t u ~now
