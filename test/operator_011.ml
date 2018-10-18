let () =
  Hashtbl.iter times ~f:(fun ~key:time ~data:azot ->
                 Clock.at time
                 >>> fun () ->
                 Db.iter t.db ~f:(fun dbo ->
                           if S.mem azot (Dbo.azo dbo) then
                             Dbo.dont dbo))
