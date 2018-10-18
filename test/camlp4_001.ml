let () =
  match var with
  | <:expr< $lid:f$ >> ->
    KO
  | <:expr< $lid:f$ >> when f x ->
    KO
  | y when f y ->
    OK
  | z when g z
    ->
    OK
  | long_pattern
    when f long_pattern -> (* Should be more indented than the clause body *)
    z
