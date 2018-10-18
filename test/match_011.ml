(* TODO: *)
let a f = function
  | A ->
     1
  |   B ->
       2
  |      C ->
          (function
           |  X  ->
               a
           | Y ->
              b) 12
  | D ->
     (match z with
      | 4 -> 3
      |  5 -> 7)
