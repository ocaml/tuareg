(* This fail contains code samples that are currently not indented
   properly.

   As indentation bugs are fixed, the corresponding samples should
   be moved to the file indent-test.ml. *)

let quux list = List.map list ~f:(fun item ->
                           print_item item
                         )

let h x =
  try  ff a b
          c d;
       gg 1 2
          3 4;
  with e -> raise e

let x = foo ~f:(fun _ -> 0              (* Comment.  *)
               )

let () =
  foo (sprintf ("a: %s"
                ^ " b: %s")
               a
               b)

let () =
  Hashtbl.iter times ~f:(fun ~key:time ~data:azot ->
                 Clock.at time
                 >>> fun () ->
                 Db.iter t.db ~f:(fun dbo ->
                           if S.mem azot (Dbo.azo dbo) then
                             Dbo.dont dbo))

let w f =
  List.map f ~f:(fun (a, b) ->
             L.r a
             >>= function
             | Ok s -> `Fst (b, s)
             | Error e -> `Snd (b, a, e))

let a =
  B.c d ~e:f [
        "g";
        "h";
      ]

let a =
  foo
    ~f:(fun () -> a
       )

let () =
  (* Comment.  *)
  bar a b
      c d;
  foo ~size
      (* Comment.  *)
      ~min:foo
      ?reduce
      ?override
      ()

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

let () =
  snoo ~f:(fun foo ->
         foo = bar
         && snoo)

let () =
  snoo ~f:(fun foo ->
         foo + bar
         && snoo)

let () =
  snoo ~f:(fun foo ->
         foo
         && snoo)

let variants a =
  match String.split a ~on:'-' with
  | [ s1; s2; s3 ] ->
     let a0 = String.concat ~sep:"" [ s1; s2] in
     let a1 = String.concat ~sep:"-" [ s1; s2; s3; "055" ] in (* Comment.  *)
     List.map [ a0; a1; a]
              ~f:(fun a_s -> lookup a_s)
     |! List.flatten
  | _ -> failwith "bad"

let optional_sci_float =
  do_something ~a:1e-7
               ~b:(fun x -> x + 1)

let array_args =
  fold s multi_sms.(0).message_number folder
       more_args (* FIXME *)

let () =
  match var with
  | <:expr< $lid:f$ >> ->
     KO
  | <:expr< $lid:f$ >> when f x ->
     KO
  | y when f y ->
     OK
  | long_pattern
       when f long_pattern -> (* Should be more indented than the clause body *)
     z

let subscribe_impl dir topic ~aborted =
  return (
      match Directory.subscribe dir topic with
      | None -> Error ()
      | Some pipe ->
         whenever (aborted >>| fun () -> Pipe.close_read pipe);
         Ok pipe
    )
         next_argument (* should be indented correctly, given the braces *)


let command =
  Command.Spec.(
    empty
    +> flag "-hello" (optional_with_default "Hello" string)
            ~doc:" The 'hello' of 'hello world'"
    +> flag "-world" (optional_with_default "World" string)
            ~doc:" The 'world' of 'hello world'"
  )

let server_comments request t =
  t >>= Grep.server_comments
    lazy
    parser
    every

let x = match y, z with
  | A, (B | C)
  | X, Y -> do_something()      (* Issue #78 *)

type t = a
 and typey = 4
 and x = b

type 'a v = id:O.t ->
           ssss:Ssss.t ->
           dddd:ddd.t ->
           t:S_m.t ->
           mmm:Safe_float.t ->
           qqq:int ->
           c:C.t ->
           uuuu:string option ->
           aaaaaa:Aaaaaa.t ->
           a:A.t ->
           rrrrr:Rrrrr.t ->
           time:Time.t ->
           typ:[ `L_p of Safe_float.t ] ->
           bazonk:present option ->
           o_p_e:O_m.t option ->
           only_hjkl:present option ->
           show_junk:int option ->
           d_p_o: Safe_float.t option ->
           asdf:present option ->
           generic:Sexp.t list ->
           'a

let () =
  try f a
  with A () ->
       ()
     | B () ->
        ()
     |     C () ->
            ()

let () =
  match _ with
  | foo ->
     bar
     >>| function _ ->
                  _

let foo x =
  f1 x >= f2 x
  && f3
      (f4 x)

let foo x =
  (>=)
    (f1 x) (f2 x)
  && f3
      (f4 x)

let splitting_long_expression =
  quad.{band, i3} <- quad.{band, i3} +. g +.
                      area_12 *. (P.potential x13 y13 +. P.potential x23 y23)

let x =
  try a
  with Not_found ->
       b
     | _ ->
        c
let x =
  try a
  with Not_found ->
       if a then b
     | flag when String.is_prefix flag ~prefix:"-" ->
        a
     | _ ->
        c

let () =
  match var with
  | <:expr< $lid:f$ >> ->
     KO
  | <:expr< $lid:f$ >> when f x ->
     KO
  | y when f y ->
     OK
  | long_pattern
       when f long_pattern -> (* Should be more indented than the clause body *)
     z

let _ =
  List.map
    (function x ->
      blabla    (* FIXME: indentation afer "(function" *)
        blabla
        blabla)
    l
