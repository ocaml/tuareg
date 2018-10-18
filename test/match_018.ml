let variants a =
  match String.split a ~on:'-' with
  | [ s1; s2; s3 ] ->
     let a0 = String.concat ~sep:"" [ s1; s2] in
     let a1 = String.concat ~sep:"-" [ s1; s2; s3; "055" ] in (* Comment.  *)
     List.map [ a0; a1; a]
              ~f:(fun a_s -> lookup a_s)
     |! List.flatten
  | _ -> failwith "bad"
