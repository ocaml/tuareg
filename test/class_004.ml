class useless' = object(self)
  val n = 10

  initializer
    print_endline "Initialised."

  method incremented () =
    succ n

  method private add x =
    n + x

  method add_option = function
    | Some x -> Some(self#add x)
    | None   -> None
end
