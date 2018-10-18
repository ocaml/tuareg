let x =
  v 1 2
    3 4
    5 6 >>= fun x ->
  y+1 >>= (* foo! *) fun z ->
  f 1 2 3
    4 5 6 >>= fun y ->
  w*3 >>= fun q -> r
