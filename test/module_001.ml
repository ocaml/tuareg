module Make_comp(C : Comparitor) : Comparitor_intf (* issue #7 *)
       with type t := C.t
and module X = Z =
  struct
    type t = C.t
    let ret = C.comp
  end
