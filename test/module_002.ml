module M = struct
  type t =
    | A
    | B
    | C
  with sexp

  type s = [
    | `A
    | `B
    | `C
    ]

  type u =
    | D
    | E
  with sexp
end
