type _ gadt =
  | A : int -> int gadt
  | B : unit gadt
  | C : float -> float gadt
