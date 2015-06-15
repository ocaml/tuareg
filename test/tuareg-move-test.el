;;; tuareg-move-test.el --- Quoted minor mode testsuite

;;; Commentary:
;;

;;; Code:

(defvar tuareg-debug-p t)

(defvar tuareg-teststring-1 "let rec eval env = function
    | Num i -> i
    | Var x -> List.assoc x env
    | Let (x, e1, in_e2) ->
       let val_x = eval env e1 in
       eval ((x, val_x) :: env) in_e2
    | Binop (op, e1, e2) ->
       let v1 = eval env e1 in
       let v2 = eval env e2 in
       eval_op op v1 v2
  and eval_op op v1 v2 =
    match op with
    | \"+\" -> v1 + v2
    | \"-\" -> v1 - v2
    | \"\*\" -> v1 \* v2
    | \"/\" -> v1 / v2
    | _ -> failwith (\"Unknown operator: \" ^ op);;
")

(defvar tuareg-teststring-2 "let rec1 eval env = function
    | Num i -> i
    | Var x -> List.assoc x env
    | Let (x, e1, in_e2) ->
       let val_x = eval env e1 in
       eval ((x, val_x) :: env) in_e2
    | Binop (op, e1, e2) ->
       let v1 = eval env e1 in
       let v2 = eval env e2 in
       eval_op op v1 v2
  and eval_op op v1 v2 =
    match op with
    | \"+\" -> v1 + v2
    | \"-\" -> v1 - v2
    | \"\*\" -> v1 \* v2
    | \"/\" -> v1 / v2
    | _ -> failwith (\"Unknown operator: \" ^ op);;

let rec2 eval env = function
    | Num i -> i
    | Var x -> List.assoc x env
    | Let (x, e1, in_e2) ->
       let val_x = eval env e1 in
       eval ((x, val_x) :: env) in_e2
    | Binop (op, e1, e2) ->
       let v1 = eval env e1 in
       let v2 = eval env e2 in
       eval_op op v1 v2
  and eval_op op v1 v2 =
    match op with
    | \"+\" -> v1 + v2
    | \"-\" -> v1 - v2
    | \"\*\" -> v1 \* v2
    | \"/\" -> v1 / v2
    | _ -> failwith (\"Unknown operator: \" ^ op);;
")

(ert-deftest tuareg-toplevel-backward-test ()
  (tuareg-test-with-temp-buffer
      tuareg-teststring-2
    (search-backward "Var" nil t)
    (tuareg-backward-top-level)
    (should (eq (char-after) ?l))
    (should (looking-at "let"))
    (tuareg-backward-top-level)
    (should (eq (char-after) ?l))
    (should (looking-at "let"))

    ))

(ert-deftest tuareg-toplevel-forward-test ()
  (tuareg-test-with-temp-buffer-point-min
      tuareg-teststring-2
    (search-forward "Var" nil t)
    (tuareg-forward-top-level)
    (should (eq (char-before) ?\;))
    (tuareg-forward-top-level)
    (sleep-for 1)
    (should (eq (char-before) ?\;))
    ))

(ert-deftest tuareg-toplevel-forward-bol-test ()
  (tuareg-test-with-temp-buffer-point-min
      tuareg-teststring-2
    (search-forward "Var" nil t)
    (tuareg-forward-top-level-bol)
    (should (eolp))
    (search-forward "Var" nil t)
    (tuareg-forward-top-level-bol)
    (should (eobp))))

(ert-deftest tuareg-backward-expression-test ()
  (tuareg-test-with-temp-buffer-point-min
      tuareg-teststring-1
    (search-forward "match")
    (tuareg-backward-expression)
    (should (eq (char-after) ?m))
    (tuareg-backward-expression)
    (should (eq (char-after) ?a))
    (tuareg-backward-expression)
    (should (eq (char-after) ?e))
    (tuareg-backward-expression)
    (should (eq (char-after) ?l))
    (tuareg-backward-expression)
    (should (eq (char-after) ?l))
    (tuareg-backward-expression)
    (should (eq (char-after) ?|))
    (tuareg-backward-expression)
    (should (eq (char-after) ?e))
    (tuareg-backward-expression)
    (should (eq (char-after) ?l))
    (tuareg-backward-expression)
    (should (eq (char-after) ?|))
    (tuareg-backward-expression)
    (should (eq (char-after) ?|))
    (tuareg-backward-expression)
    (should (eq (char-after) ?|))
    (tuareg-backward-expression)
    (should (eq (char-after) ?l))
    ))

(ert-deftest tuareg-forward-expression-test ()
  (tuareg-test-with-temp-buffer-point-min
      tuareg-teststring-1
    (tuareg-forward-expression)
    (should (eq (char-before) ?n))
    (tuareg-forward-expression)
    (should (eq (char-before) ?i))
    (tuareg-forward-expression)
    (should (eq (char-before) ?v))
    (tuareg-forward-expression)
    (should (eq (char-before) ?>))
    (tuareg-forward-expression)
    (should (eq (char-before) ?n))
    (tuareg-forward-expression)
    (should (eq (char-before) ?2))
    (tuareg-forward-expression)
    (should (eq (char-before) ?>))
    (tuareg-forward-expression)
    (should (eq (char-before) ?n))
    (tuareg-forward-expression)
    (should (eq (char-before) ?n))
    (tuareg-forward-expression)
    (should (eq (char-before) ?2))
    (tuareg-forward-expression)
    (should (eq (char-before) ?=))
    (tuareg-forward-expression)
    (should (eq (char-before) ?h))
    (tuareg-forward-expression)
    (should (eq (char-before) ?2))
    (search-forward "failwith")
    (tuareg-forward-expression)
    (should (eq (char-before) ?\;))
    ))

(provide 'tuareg-move-test)

;;; tuareg-move-test.el ends here
