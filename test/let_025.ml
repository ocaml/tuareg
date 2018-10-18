let () =
  IO.println
    out (tagL
           "ol" (List.map
                   ~f:(tag ~a:[] "li") (
                     (List.map
                        results ~f:(fun (what,_) ->
                          tag "a" ~a:[("href","#" ^ what)] (what_title what)))
                     @ [tag "a" ~a:[("href","#" ^ message_id)] message_title;
                        tag "a" ~a:[("href","#" ^ legend_id)] legend_title])))
