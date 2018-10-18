(* TODO: *)
;; (* http://caml.inria.fr/mantis/view.php?id=4247 *)
let x = {
  Foo.
  a = b;
  c = d;
  e = {Bar.
        f = 1;
        g = 2;
      };
  h = {  Quux.
        i = 3;
        j = 4;
      };
}
