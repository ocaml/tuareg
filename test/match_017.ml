let a = match b with
  | Some c ->  Some {
                 d = c;
                 e = e
               }
  | None -> {
      d = c;
      e = e
    }
