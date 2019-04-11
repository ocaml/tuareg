(* Test evaluation (C-cC-e). *)

let a = 1 let b = 2    (* Try with cursor on second let *)

let c = 1 (* comment *) let d = 2

let e = 1 + (* cursor → *) 2

let f = 2   (* cursor after the comment → f *)

let g = 1;;
(* Test with cursor on this line → g *)
(* Test with cursor on this line → g *)

let h = 1;;
(* Force new phrase after this comment *);;
(* Evaluating on this line sends an empty phrase (refused) *)


let not_well_braced = (1
