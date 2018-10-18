let foo =
  (* Comment.  *)
  List.map z
           ~f:(fun m ->
             M.q m
             |! T.u ~pr ~verbose:false
                    ~p:H.P.US ~is_bar:false)
  |! List.sort ~cmp:(fun a b ->
                 compare
                   (I.r a.T.s)
                   (I.r b.T.s))
