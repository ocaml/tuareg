let c f =
  if S.is_file f then (
    S.load f C.t
    |! fun x -> c := Some x
  ) else (
    C.s C.default |! S.save f
    |! fun () -> c := None)
