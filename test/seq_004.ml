let () =
  (* Beware of lexing ".;" as a single token!  *)
  A.Axes.box vp;
  A.fx vp (E.on_ray u0) 0. 2000.;
  A.Viewport.set_color vp A.Color.green
