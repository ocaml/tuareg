class useless' = object(self)
  val n = 10

  method incremented () =
    succ n

  method add_option = function
    | Some x -> Some(n + x)
    | None   -> None
end
