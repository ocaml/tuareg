type 'a foo = 'a option =         (* Issue #98 *)
  | None
  | Some of 'a
