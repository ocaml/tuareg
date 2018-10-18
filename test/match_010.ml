(* TODO: *)
let d x = function
  (* FIXME: Should we leave it like this or align "|" with "match"?
     I chose with "match" because it looks otherwise odd and is more
     consistent with the "try" alignments above.  *)
  | A -> (match x with
          | X ->
             false
          | Y -> true
          |  Z ->
              false)
  | B -> false
