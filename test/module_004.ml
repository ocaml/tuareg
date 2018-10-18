module type M = M2
  with type t1 = int
   and type t2 = int
   and module S = M3
  with type t2 = int
  with type t3 = int
