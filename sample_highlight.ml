type 'a t = Int : int t | String : string t
let trois : type a . a t -> a = f

type a
type 'a t
type 'al t
type 'al'l t
type +'b u
type ('a, 'b) t
type (+'a, 'b) t
type t =
  | A
type t +=
   | A

type t =
  { first: A.t; (* and *)
    second: B.t;
    third: C.t }

(* FAIL sprintf (but if one change the line it get well re-highlighted) *)
(* Probably an effect of [let ...] *)
let html_date_of_post p =
  match p.date with
  | None -> []
  | Some d ->
     let date =
       let open Syndic.Date in
       let open! Infix in
       sprintf "%s %02d, %d" (string_of_month(month d)) (day d) (year d) in
     [`Data date]

let (x: t) = expr
let (x:t) = expr
let (x: t list) = exp
let (x:t list) = 1
let x : t = expr
let x, (yyy: t) = 1
let (x, y) = A.f ()
let x, y = A.f ()
let x, y, z = A.f ()
let (x, y), z = A.f ()
let (x: y :> u) = 1

let x as y = 1
let (x,y) as z = (1,2)

let X x = A.f ()
let X(x) = A.f ()
let `X x = A.f ()
let A.X x, A.Y (y:t) = f()
let A.X x, (`Y y:t) = f()
let X(x, y) = A.f ()
let X (x, y) = A.f ()
let A.X(x, y, z) = A.f ()
let A.X x = A.f ()
let x : ('a, 'b) t = A.f ()
let x = (x : X.t)
let x = (val X : X.t)
let m = __MODULE__
let typecheck ast = ast_starts_with_as

let _ = (x :: not_a_type)
let _ = (x : 'a t)
let _ = (x :> t)
let _ = (let x : t = 1 in x)   (* not a type *)
let _ = (1 + let x : t = 1 in x)
let _ = (1 + 1 : int)
let _ = (z : Map.t)
let _ = (z x : Map.t)
let _ = (z x : _ Map.t)
let _ = (z x : ('a, int) Map.t)
let _ = {first = x; second = y; third = z}

let () =
  printf "(v:t) in strings (expected: %g" n; printf ")"

let x = ref 1
let f x = ignore(x+1)
let f ref c = 1
let f (x, (y, z)) = 1
let (x, (y, z)) = 1
let ((x, y): t) = 1

let f () = 1
let f (type t) x = 1
let f (module M: T) = M.f
let _ = f (module M)
let x = A.b
let z = (compare (x: int) (1 + y: int) : t)
let f x = A.B.c
let f x
      y = 3
let f = fun x -> 4
let f = function x -> 1
let f x y z : t = 2
let f (x,y) z : t = 2
let f (x: t) (y: ('a, (a, int)) t) = 2
let f ~x y = 3
let f ~x ?(y=2) = 3
let f (X x) y = 1
let f = fun (x: int) u (y,z) -> 4
let f = fun (x: int) u
            (y,z) -> 1
let f (x: int) u
      (y,z) = 1
let f = fun ?(x=y-1) z -> 1
let f = fun ?(x=true) z -> 1
let f = fun ?(x=1=1) z -> "two type of '=' in option 'x'"
let f x = fun u v (u,c) ?(u=v-1) ~(e: int) -> 1
let f = fun x u->1
let f u0 ~s:a s = "s: does not introduce a type"
let f u0 ~s:(a,b) s = "s: does not introduce a type"
let f x : ret = body
let f (x) : ret = body
let f ?x:(y = 1) ?(y = (x: t)) = body
let f ?x:(y = 1) ?(y = (x: t)) : ret = body
let f ?x:(y = expr + 1) (y: t) ~z:u : ret = body
let f ?x:(y = (expr + 1)) (y: t) ?z:t : ret = body
let x = call ~l:(fun x -> y = z)
let f {first; second; third} = body
let f ({first; second; third} as all) = body
let f a {first; second; third} b = body
let f a ({first; second; third} as all) b = body
let f a ({first = x; second = y; third} as all) b = body
let f a [x; y; z] u = body
let f (type a) x (type b) y = body
let f (A(x:t), B x, {z = s; p = Q e}) = body
let f' u =
  (* function *)
  if u.low >= 0. then f'_pos u.low u.high
let f' u =
  (* fun *)
  if u.low >= 0. then f'_pos u.low u.high
let rec f (A(x:t), B x, {z = s; p = Q e}) = body

(* Labels, type annotations, and operators *)
let _ =
  f ~foo:x;
  (f ~foo:x);
  (f ~foo:x y);
  (grault ~garply:(x));
  let x = 1 + 3 / 2 in
  I.(1 +:2);
  I.(1 +: 2);
  K.(x |+ y ?: z);
  (expr ~- expr : ty);
  (expr ~label : ty)

let andfoo = 1.
let[@x] andfoo = 1.
and+ andfoo = 2
let valfoo = 1

let x = 1 [@@@x rzfhjoi[x]]
let x = 1 [@@x "payload"]
let z = [%%foo let x = 2 in x + 1]
let[@foo] x = 2 in x + 1
let%m[@foo] x = 2 in x + 1
let _ = begin[@foo][@bar x] ... end
module[@foo] M = struct end
type[@foo] t = T
type%foo[@foo] nonrec t = t

let x = first ;%x second
let%xx x = 1
let%xx f x = 1
let%x f x = 1
let%foo x = 2 in x + 1
let x = begin%foo ... end
val%foo f : t -> t
val%foo[@bar] f : t -> t
module%foo Mo = struct end
module%foo type Mo = struct%loo[@bah] end
[%%foo module M = struct end ]
val%foo f : t -> t
let f = fun%foo x -> x + 1
let f = fun%foo[@bar] x -> x + 1
let f = fun[@bar] x -> x + 1
let f = function%foo[@bar] x -> x + 1

let content = [%html{|<div id="content">some content</div>|}]
let svgpath = [%svg{|<path d="M 0 1 L 1 0"></path>|}]

let my_text =
  [%html
      {|This is an <b>HTML</b> formated <i>content</i>.|}]

let my_span = Html.(span ~a:[a_class ["mytext"]] my_text)
let%html content = {|<div id="content">some content</div>|}
let my_head = [%html "<head>" my_title "</head>"]


let x = `failwith  (* Constructor, not builtin *)
let y = `Not_found (* Constructor, not builtin *)

(* FIXME: not a type*)
let _ = (Jacobi.jacobi n ~alpha:nu ~beta:nu x)**3.

open A.B
open! A.B
module X = Y
module rec X = struct end
module rec A.B  (* path not allowed *)
module type x   (* lowercase allowed! *)
(* with type t open! mutable virtual *)
module type X = Y with module Z = A
module type X = Y with module Z.U = A
                   and module A = B.C
module type X = Y with type t = u
                   and type u = l
module A = B.C
module A = B.C(String)
module A = B.C(U(V).T)
module A = B.C(U(V))
module A : E = B.C(U(V))
module A : B.C(String).T = A
module F(A : Y) = T
module F(A: X.Y) = T
module F(A : X.t) = T
module F(A : X(Y).T) = T
module F(A : X(Y(Z)).T) = A
module F(A : A1)(B:B1) = Z
module F = functor (A: A1) -> functor(B:B1) -> A.B.f

let module X = F(G) in ()

include Make  (* make sure the coloring does not extend on spaces *)
include Make(IO)
include (Make(IO) : module type of Make(IO) with type t := t)

include Make(IO).T  (* in a module sig *)

module Make_client
         (IO:S.IO with type 'a t = 'a Lwt.t)
         (Request:Request with module IO = IO)
         (Response:Response with module IO = IO)
         (Net:Net with module IO = IO) = struct end

class printable_point x_init =
object (s)
  val mutable x = x_init
  method get_x = x
  method move d = x <- x + d
  method print = print_int s#get_x
end;;
class virtual abstract_point x_init =
        object (self)
          method virtual get_x : int
          method get_offset = self#get_x - x_init
          method! virtual move : int -> unit
          method private virtual x = body
          method virtual private y = body
        end;;
class ['a] re x_init = object
  val mutable x = (x_init : 'a)
  method get = x
  method set y = x <- y
end;;
class type c2 = object ('a) method m : 'a end;;
class type c2 = object (_) method m : 'a end;;
class type virtual c2 = object ('a) method m : 'a end;;

class xx x y z = object
  method x yellow
           zero = 1
  method virtual x ?(y=1) t = 1
  method virtual private x (y:t) z = 1
  method private x y z = body
  method private x y z : t = body
  method private virtual x y z
  method x private x = 1
end

val x
val! x
val mutable x
val mutable virtual x
val virtual x
val virtual mutable x
val mutable
val f : int -> 'a t
class virtual x = object
          method virtual x : int -> float
        end;;
class x = object
  method virtual x : int -> float
end;;
class ['a] x ~ne (z: y) = object
  method virtual x : int -> float
end;;
object(self) end;;
object (self) end;;
object(self : ('a) t) end;;
object (self : ('a, 'b) t) end;;

external f

let x = if x then y else z

exception E of string
let _ = failwithf {| message |}

let z = .< x + 1 .>
;;

module type T = sig
  val f : t -> t -> t
end
;;
(* Local Variables: *)
(* End: *)
(* tuareg-support-metaocaml: t *)
