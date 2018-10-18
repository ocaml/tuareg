(* TODO: *)
let x = match y with            (* Issue #71 *)
  | A | B | C ->
    do_something ()
