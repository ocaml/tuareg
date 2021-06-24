;;; tests for tuareg.el                       -*- lexical-binding: t -*-

(require 'tuareg)
(require 'ert)

(defconst tuareg-test-dir
  (file-name-directory (or load-file-name buffer-file-name)))

(defun tuareg-test--remove-indentation ()
  "Remove all indentation in the current buffer."
  (goto-char (point-min))
  (while (re-search-forward (rx bol (+ (in " \t"))) nil t)
    (let ((syntax (save-match-data (syntax-ppss))))
      (unless (or (nth 3 syntax)        ; not in string literal
                  (nth 4 syntax))       ; nor in comment
        (replace-match "")))))

(ert-deftest tuareg-indent-good ()
  "Check indentation that we do handle satisfactorily."
  (let ((file (expand-file-name "indent-test.ml" tuareg-test-dir))
        (text (lambda () (buffer-substring-no-properties
                          (point-min) (point-max)))))
    (with-temp-buffer
      (insert-file-contents file)
      (tuareg-mode)
      (let ((orig (funcall text)))
        ;; Remove the indentation and check that we get the original text.
        (tuareg-test--remove-indentation)
        (indent-region (point-min) (point-max))
        (should (equal (funcall text) orig))
        ;; Indent again to verify idempotency.
        (indent-region (point-min) (point-max))
        (should (equal (funcall text) orig))))))

(ert-deftest tuareg-indent-bad ()
  "Check indentation that we do not yet handle satisfactorily."
  :expected-result :failed
  (let ((file (expand-file-name "indent-test-failed.ml" tuareg-test-dir))
        (text (lambda () (buffer-substring-no-properties
                          (point-min) (point-max)))))
    (with-temp-buffer
      (insert-file-contents file)
      (tuareg-mode)
      (let ((orig (funcall text)))
        ;; Remove the indentation and check that we get the original text.
        (tuareg-test--remove-indentation)
        (indent-region (point-min) (point-max))
        (should (equal (funcall text) orig))
        ;; Indent again to verify idempotency.
        (indent-region (point-min) (point-max))
        (should (equal (funcall text) orig))))))

(defmacro tuareg--lets (&rest forms)
  "Execute FORMS in sequence, binding new vars as they occur.
Every expression in FORMS can be any normal ELisp expression,
with the added form (let VAR VAL) which will bind VAR to the value of VAL.
Returns the value of the last FORM."
  (declare (indent 0) (debug (&rest [&or ("let" symbolp form) form])))
  (let ((exps '())
        (bindings '()))
    (dolist (form forms)
      (pcase form
        (`(let ,(and (pred symbolp) var) ,val)
         (push (list var (macroexp-progn (nreverse (cons val exps))))
               bindings)
         (setq exps '()))
        (_ (push form exps))))
    `(let* ,(nreverse bindings) . ,(nreverse exps))))

(ert-deftest tuareg-beginning-of-defun ()
  ;; Check that `beginning-of-defun' works as expected: move backwards
  ;; to the beginning of the current top-level definition (defun), or
  ;; the previous one if already at the beginning; return t if one was
  ;; found, nil if none.
  (with-temp-buffer
    (tuareg-mode)
    (tuareg--lets
      (insert "(* first line *)\n\n")
      (let p1 (point))
      (insert "type ty =\n"
              "  | Goo\n"
              "  | Baa of int\n\n")
      (let p2 (point))
      (insert "let a = ho hum\n"
              ";;\n\n")
      (let p3 (point))
      (insert "let g u =\n"
              "  while mo ma do\n"
              "    we wo;\n")
      (let p4 (point))
      (insert "    ze zo\n"
              "  done\n")

      ;; Check without argument.
      (goto-char p4)
      (should (equal (beginning-of-defun) t))
      (should (equal (point) p3))
      (should (equal (beginning-of-defun) t))
      (should (equal (point) p2))
      (should (equal (beginning-of-defun) t))
      (should (equal (point) p1))
      (should (equal (beginning-of-defun) nil))
      (should (equal (point) (point-min)))

      ;; Check with positive argument.
      (goto-char p4)
      (should (equal (beginning-of-defun 1) t))
      (should (equal (point) p3))
      (goto-char p4)
      (should (equal (beginning-of-defun 2) t))
      (should (equal (point) p2))
      (goto-char p4)
      (should (equal (beginning-of-defun 3) t))
      (should (equal (point) p1))
      (goto-char p4)
      (should (equal (beginning-of-defun 4) nil))
      (should (equal (point) (point-min)))

      ;; Check with negative argument.
      (goto-char (point-min))
      (should (equal (beginning-of-defun -1) t))
      (should (equal (point) p1))
      (should (equal (beginning-of-defun -1) t))
      (should (equal (point) p2))
      (should (equal (beginning-of-defun -1) t))
      (should (equal (point) p3))
      (should (equal (beginning-of-defun -1) nil))
      (should (equal (point) (point-max)))

      (goto-char (point-min))
      (should (equal (beginning-of-defun -2) t))
      (should (equal (point) p2))
      (goto-char (point-min))
      (should (equal (beginning-of-defun -3) t))
      (should (equal (point) p3))
      (goto-char (point-min))
      (should (equal (beginning-of-defun -4) nil))
      (should (equal (point) (point-max)))

      ;; We don't test with a zero argument as the behaviour for that
      ;; case does not seem to be very well-defined.
      )))

(ert-deftest tuareg-chained-defun ()
  ;; Check motion by defuns that are chained by "and".
  (with-temp-buffer
    (tuareg-mode)
    (tuareg--lets
      (insert "(* *)\n\n")
      (let p0 (point))
      (insert "type t1 =\n"
              "  A\n")
      (let p1 (point))
      (insert "and t2 =\n"
              "  B\n")
      (let p2a (point))
      (insert "\n")
      (let p2b (point))
      (insert "and t3 =\n"
              "  C\n")
      (let p3a (point))
      (insert "\n")
      (let p3b (point))
      (insert "let f1 x =\n"
              "  aa\n")
      (let p4 (point))
      (insert "and f2 x =\n"
              "  bb\n")
      (let p5a (point))
      (insert "\n")
      (let p5b (point))
      (insert "and f3 x =\n"
              "  let ff1 y =\n"
              "    cc\n"
              "  and ff2 y = (\n")
      (let p6 (point))
      (insert "    qq ww) + dd\n"
              "  and ff3 y =\n"
              "    for i = 1 to 10 do\n"
              "      ee;\n")
      (let p7 (point))
      (insert "      ff;\n"
              "    done\n")
      (let p8a (point))
      (insert "\n")
      (let p8b (point))
      (insert "exception E\n")

      ;; Walk backwards from the end.
      (goto-char (point-max))
      (beginning-of-defun)
      (should (equal (point) p8b))
      (beginning-of-defun)
      (should (equal (point) p5b))
      (beginning-of-defun)
      (should (equal (point) p4))
      (beginning-of-defun)
      (should (equal (point) p3b))
      (beginning-of-defun)
      (should (equal (point) p2b))
      (beginning-of-defun)
      (should (equal (point) p1))
      (beginning-of-defun)
      (should (equal (point) p0))
      (beginning-of-defun)
      (should (equal (point) (point-min)))

      ;; Walk forwards from the beginning.
      (end-of-defun)
      (should (equal (point) p1))
      (end-of-defun)
      (should (equal (point) p2a))
      (end-of-defun)
      (should (equal (point) p3a))
      (end-of-defun)
      (should (equal (point) p4))
      (end-of-defun)
      (should (equal (point) p5a))
      (end-of-defun)
      (should (equal (point) p8a))
      (end-of-defun)
      (should (equal (point) (point-max)))

      ;; Jumps from inside a defun.
      (goto-char p7)
      (beginning-of-defun)
      (should (equal (point) p5b))

      (goto-char p6)
      (end-of-defun)
      (should (equal (point) p8a)))))

(ert-deftest tuareg-phrase-discovery ()
  (with-temp-buffer
    (tuareg-mode)
    (tuareg--lets
     (insert "let a = 1 and b = 2 in a + b\n")
     (let p1 (point))
     (insert "let f x =\n"
             "  x + 1\n")
     (let p2a (point))
     (insert "and g x =\n"
             "  x * 2\n")
     (let p2b (point))
     (insert ";;\n")
     (let p2c (point))
     (insert "(1 < 2) = false;;\n")
     (let p3 (point))
     (insert "'a';;\n")
     (let p4 (point))
     (insert "\"abc\" ^ \" \" ^ \"def\";;\n")
     (let p5 (point))
     (insert "{|with \\ special \" chars|};;\n")
     (let p6 (point))

     (goto-char (point-min))
     (end-of-defun)
     (should (equal (point) p1))
     (end-of-defun)
     (should (equal (point) p2a))
     (end-of-defun)
     (should (equal (point) p2b))
     (end-of-defun)
     (should (equal (point) p3))
     (end-of-defun)
     (should (equal (point) p4))
     (end-of-defun)
     (should (equal (point) p5))
     (end-of-defun)
     (should (equal (point) p6))

     (beginning-of-defun)
     (should (equal (point) p5))
     (beginning-of-defun)
     (should (equal (point) p4))
     (beginning-of-defun)
     (should (equal (point) p3))
     (beginning-of-defun)
     (should (equal (point) p2c))
     (beginning-of-defun)
     (should (equal (point) p2a))
     (beginning-of-defun)
     (should (equal (point) p1))
     (beginning-of-defun)
     (should (equal (point) (point-min)))

     (should (equal (tuareg-discover-phrase (point-min))
                    (list (point-min) (1- p1) (1- p1))))
     (should (equal (tuareg-discover-phrase p1)
                    (list p1 (1- p2b) (1- p2b))))
     (should (equal (tuareg-discover-phrase p2c)
                    (list p2c (1- p3) (1- p3))))
     (should (equal (tuareg-discover-phrase p3)
                    (list p3 (1- p4) (1- p4))))
     (should (equal (tuareg-discover-phrase p4)
                    (list p4 (1- p5) (1- p5))))
     (should (equal (tuareg-discover-phrase p5)
                    (list p5 (1- p6) (1- p6))))
     )))

(ert-deftest tuareg-defun-separator ()
  ;; Check correct handling of ";;"-separated defuns/phrases.
  (with-temp-buffer
    (tuareg-mode)
    (tuareg--lets
     (insert "let _ = tata 3 ;;\n")
     (let p1 (point))
     (insert "let _ = titi 4 ;;\n")
     (let p2 (point))
     (insert "abc def ;;\n")
     (let p3 (point))
     (insert "ghi jkl ;;\n")
     (let p4 (point))

     (goto-char (point-min))
     (end-of-defun)
     (should (equal (point) p1))
     (end-of-defun)
     (should (equal (point) p2))
     (end-of-defun)
     (should (equal (point) p3))
     (end-of-defun)
     (should (equal (point) p4))
     (beginning-of-defun)
     (should (equal (point) p3))
     (beginning-of-defun)
     (should (equal (point) p2))
     (beginning-of-defun)
     (should (equal (point) p1))
     (beginning-of-defun)
     (should (equal (point) (point-min)))

     (should (equal (tuareg-discover-phrase (point-min))
                    (list (point-min) (1- p1) (1- p1))))
     (should (equal (tuareg-discover-phrase p1)
                    (list p1 (1- p2) (1- p2))))
     (should (equal (tuareg-discover-phrase (+ p1 2))
                    (list p1 (1- p2) (1- p2))))
     (should (equal (tuareg-discover-phrase p2)
                    (list p2 (1- p3) (1- p3))))
     (should (equal (tuareg-discover-phrase p3)
                    (list p3 (1- p4) (1- p4))))
     )))

(ert-deftest tuareg-class-type ()
  (with-temp-buffer
    (tuareg-mode)
    (tuareg--lets
     (insert "class type my_class_type =\n"
             "  object\n"
             "    method meth_1 : int\n"
             "    method meth_2 : unit\n"
             "  end;;\n")
     (let p1 (point))

     (goto-char (point-min))
     (end-of-defun)
     (should (equal (point) p1))
     (beginning-of-defun)
     (should (equal (point) (point-min)))
     (should (equal (tuareg-discover-phrase (point-min))
                    (list (point-min) (1- p1) (1- p1)))))))

(provide 'tuareg-tests)
