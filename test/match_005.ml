let bar x = fun u ->
  match u with
  | Some _ -> true
  | None -> false
