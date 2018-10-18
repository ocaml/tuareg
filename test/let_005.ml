let test1 = with_connection (fun conn ->
    do_something conn x;
    ...
  )
    toto
