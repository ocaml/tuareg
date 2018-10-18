(* TODO: *)
let () =
  Config.load ()
  >>> fun config ->
  let quux = config.Config.bazonk.Config.Bazonk.quux in
  load_quux ~input quux config
  >>> fun quux ->
  let da = Poo.Snapshot.merge quux in
  load_foobar config ~input
  >>> fun foobar ->
  whatever foobar
