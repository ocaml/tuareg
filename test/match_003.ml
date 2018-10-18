let rec count_append l1 l2 count =
  (* http://caml.inria.fr/resources/doc/guides/guidelines.en.html *)
  match l1 with
  | []               ->                         l2
  | [x1]             -> x1                   :: l2
  | [x1; x2]         -> x1 :: x2             :: l2
  | [x1; x2; x3]     -> x1 :: x2 :: x3       :: l2
  | [x1; x2; x3; x4] -> x1 :: x2 :: x3 :: x4 :: l2
  | x1 :: x2 :: x3 :: x4 :: x5 :: tl ->
     x1 :: x2 :: x3 :: x4 :: x5 ::
       (if count > 1000
        then slow_append tl l2
        else count_append tl l2 (count + 1))
  (* New in OCaml-4.02.  *)
  | exception Not_Found ->
     l2
