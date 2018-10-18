type t = {
  a : B.t;
  c : D.t;

  e : F.t;

  g : H.t I.t;
  j :
    K.t L.t;
  m : N.t O.t;
  p :
    ((q:R.t
      -> s:T.U.t
      -> v:(W.t -> X.t option)
      -> y:(Z.t -> A.t -> B.t C.D.t E.t)
      -> f:(G.t -> H.t I.t option)
      -> j:(K.t -> L.t M.t option)
      -> n:(O.t -> p option)
      -> q:R.t
      -> s:(string -> unit) -> T.t
     )
     -> U.t
     -> V.W.t
     -> X.t);
  y : Z.t A.t;
  b : C.t D.t E.t;
  f : (G.t -> H.t -> I.t J.t);
} with sexp_of
