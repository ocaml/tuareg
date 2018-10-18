(* OOP elements (from Marc Simpson <marc AT 0branch DOT com>).  *)

class useless = object
  val n = 10

  method incremented () =
    succ n

  method add_option = function
    | Some x -> Some(n + x)
    | None   -> None
end
