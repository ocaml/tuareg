(* Indent comment to following code.  *)
let () =
  try                                   (* foo!
                                           bar *)
    let a = f g c d in
    a b
  with _ -> ()
