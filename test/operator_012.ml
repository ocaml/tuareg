let () =
  f 1
  |! (fun x ->
    g x x)
  |! (fun y ->
    h y y)
