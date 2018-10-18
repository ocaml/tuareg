let f a1 a2 a3
      b1 b2 b3 d1 d2 d3 = {
  aa = func1 a1 a2 a3;
  bb = func2
         b1 b2 b3;
  (* FIXME: Here it is reasonable to have '|' aligned with 'match' *)
  cc = (match c with
        | A -> 1
        | B -> 2);
  dd = func3
         d1 d2 d3;
}
