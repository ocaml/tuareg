;;; tests for tuareg.el                       -*- lexical-binding: t -*-

(require 'tuareg)
(require 'compile)
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

(ert-deftest tuareg-phrase-discovery-1 ()
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
     (insert "type ta = A\n"
             "        | B of tb\n")
     (let p3a (point))
     (insert "and tb = C\n"
             "       | D of ta\n")
     (insert ";;\n")
     (let p3b (point))

     (goto-char (point-min))
     (end-of-defun)
     (should (equal (point) p1))
     (end-of-defun)
     (should (equal (point) p2a))
     (end-of-defun)
     (should (equal (point) p2b))
     (end-of-defun)
     (should (equal (point) p3a))
     (end-of-defun)
     (should (equal (point) p3b))

     (beginning-of-defun)
     (should (equal (point) p3a))
     (beginning-of-defun)
     (should (equal (point) p2b))
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
     (should (equal (tuareg-discover-phrase p2b)
                    (list p2b (1- p3b) (1- p3b)))))))

(ert-deftest tuareg-phrase-discovery-2 ()
  (let ((lines
         '("(1 < 2) = false;;"
           "'a';;"
           "\"abc\" ^ \" \" ^ \"def\";;"
           "{|with \\ special \" chars|};;"
           "max 1 2;;"
           "if true then 1 else 2 ;;"
           "while false do print_endline \"a\" done ;;"
           "for i = 1 to 3 do print_int i done ;;"
           "open Stdlib.Printf;;"
           "begin print_char 'a'; print_char 'b'; end ;;"
           "match [1;2] with a :: _ -> a | [] -> 3 ;;"
           "exception E of int * string ;;"
           "external myid : 'a -> 'a = \"%identity\";;"
           "class k = object method m = 1 end;;")))

    (with-temp-buffer
      (tuareg-mode)
      (dolist (line lines)
        (insert line "\n"))

      ;; Check movement by defun.
      (goto-char (point-min))
      (let ((pos (point-min)))
        (dolist (line lines)
          (let ((next-pos (+ pos (length line) 1)))
            (ert-info ((prin1-to-string line) :prefix "line: ")
              (end-of-defun)
              (should (equal (point) next-pos))
              (setq pos next-pos))))

        (dolist (line (reverse lines))
          (let ((prev-pos (- pos (length line) 1)))
            (ert-info ((prin1-to-string line) :prefix "line: ")
              (beginning-of-defun)
              (should (equal (point) prev-pos))
              (setq pos prev-pos)))))

      ;; Check phrase discovery.
      (let ((pos (point-min)))
        (dolist (line lines)
          (let ((next-pos (+ pos (length line) 1)))
            (ert-info ((prin1-to-string line) :prefix "line: ")
              (should (equal (tuareg-discover-phrase pos)
                             (list pos (1- next-pos) (1- next-pos))))
              (setq pos next-pos))))))))

(ert-deftest tuareg-defun-separator ()
  ;; Check correct handling of ";;"-separated defuns/phrases.
  (with-temp-buffer
    (tuareg-mode)
    (tuareg--lets
     (insert "let _ = tata 3 ;;\n")
     (let p1 (point))
     (insert "abc def ;;\n")
     (let p2 (point))
     (insert "let _ = tata 3\n"
             ";;\n")
     (let p3 (point))
     (insert "ghi jkl\n"
             ";;\n")
     (let p4 (point))
     (insert "type spell =\n"
             "  | Frotz\n"
             "  | Xyzzy\n"
             ";;\n")
     (let p5 (point))

     (goto-char (point-min))
     (end-of-defun)
     (should (equal (point) p1))
     (end-of-defun)
     (should (equal (point) p2))
     (end-of-defun)
     (should (equal (point) p3))
     (end-of-defun)
     (should (equal (point) p4))
     (end-of-defun)
     (should (equal (point) p5))
     (beginning-of-defun)
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
     (should (equal (tuareg-discover-phrase p4)
                    (list p4 (1- p5) (1- p5))))
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

(defconst tuareg-test--compilation-messages
  '((("File \"file.ml\", line 4, characters 6-7:\n"
      "Error: This expression has type int\n"
      "This is not a function; it cannot be applied.\n")
     ((1 error "file.ml" 4 4 6 6)))
    (("File \"file.ml\", line 3, characters 6-7:\n"
      "Warning 26: unused variable y.\n")
     ((1 warning "file.ml" 3 3 6 6)))

    (("File \"helloworld.ml\", line 2, characters 36-64:\n"
      "2 | module rec A: sig type t += A end = struct type t += A = B.A end\n"
      "                                        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^\n"
      "Error: Cannot safely evaluate the definition of the following cycle\n"
      "       of recursively-defined modules: A -> B -> A.\n")
     ((1 error "helloworld.ml" 2 2 36 63)))
    (("File \"helloworld.ml\", lines 4-7, characters 6-3:\n"
      "4 | ......struct\n"
      "5 |   module F(X:sig end) = struct end\n"
      "6 |   let f () = B.value\n"
      "7 | end\n"
      "Error: Cannot safely evaluate the definition of the following cycle\n"
      "       of recursively-defined modules: A -> B -> A.\n")
     ((1 error "helloworld.ml" 4 7 6 2)))
    (("File \"robustmatch.ml\", lines 33-37, characters 6-23:\n"
      " 9 | ......match t1, t2, x with\n"
      "10 |       | AB, AB, A -> ()\n"
      "11 |       | MAB, _, A -> ()\n"
      "12 |       | _,  AB, B -> ()\n"
      "13 |       | _, MAB, B -> ()\n"
      "Warning 8: this pattern-matching is not exhaustive.\n"
      "Here is an example of a case that is not matched:\n"
      "(AB, MAB, A)\n")
     ((1 warning "robustmatch.ml" 33 37 6 22)))
    (("File \"robustmatch.ml\", lines 33-37, characters 6-23:\n"
      " 9 | ......match t1, t2, x with\n"
      "10 |       | AB, AB, A -> ()\n"
      "11 |       | MAB, _, A -> ()\n"
      "12 |       | _,  AB, B -> ()\n"
      "13 |       | _, MAB, B -> ()\n"
      "Warning 8 [partial-match]: this pattern-matching is not exhaustive.\n"
      "Here is an example of a case that is not matched:\n"
      "(AB, MAB, A)\n")
     ((1 warning "robustmatch.ml" 33 37 6 22)))
    (("File \"main.ml\", line 13, characters 34-35:\n"
      "13 |   let f : M.t -> M.t = fun M.C -> y\n"
      "                                       ^\n"
      "Error: This expression has type M/2.t but an expression was expected of type\n"
      "         M/1.t\n"
      "       File \"main.ml\", line 10, characters 2-41:\n"
      "         Definition of module M/1\n"
      "       File \"main.ml\", line 7, characters 0-32:\n"
      "         Definition of module M/2\n")
     ((1 error "main.ml" 13 13 34 34)
      (225 info "main.ml" 10 10 2 40)
      (308 info "main.ml" 7 7 0 31)))
    (("File \"alrt.ml\", line 25, characters 9-10:\n"
      "25 |   val x: t [@@ocaml.deprecated]\n"
      "              ^\n"
      "Alert deprecated: t\n")
     ((1 warning "alrt.ml" 25 25 9 9)))
    (("Fatal error: exception Bad.Disaster(\"oh no!\")\n"
      "Raised at file \"bad.ml\", line 5, characters 4-22\n"
      "Called from file \"worse.ml\" (inlined), line 9, characters 2-5\n"
      "Called from file \"worst.ml\", line 12, characters 8-18\n")
     ((47 error "bad.ml" 5 5 4 21)
      (96 error "worse.ml" 9 9 2 4)
      (158 error "worst.ml" 12 12 8 17)))
    (("Fatal error: exception Bad.Disaster(\"oh no!\")\n"
      "Raised at Bad.f in file \"bad.ml\", line 5, characters 4-22\n"
      "Called from Bad.g in file \"worse.ml\" (inlined), line 9, characters 2-5\n"
      "Called from Bad in file \"worst.ml\", line 12, characters 8-18\n")
     ((47 error "bad.ml" 5 5 4 21)
      (105 error "worse.ml" 9 9 2 4)
      (176 error "worst.ml" 12 12 8 17)))
    (("Fatal error: exception Hell\n"
      "Raised by primitive operation at Murky.depths in file \"inferno.ml\", line 399, characters 28-54\n"
      "Called from Nasty.f in file \"nasty.ml\", line 7, characters 13-40\n"
      "Re-raised at Smelly.f in file \"smelly.ml\", line 14, characters 12-19\n"
      "Called from Rubbish.g in file \"rubbish.ml\", line 17, characters 2-5\n")
     ((29 error "inferno.ml" 399 399 28 53)
      (124 error "nasty.ml" 7 7 13 39)
      (189 error "smelly.ml" 14 14 12 18)
      (258 error "rubbish.ml" 17 17 2 4))))
  "Compilation message test data.
Each element is (STRINGS ERRORS) where

 STRINGS is a list of strings forming the message when concatenated
 ERRORS is a list of error descriptions, each being

  (POS TYPE FILE LINE-START LINE-END COLUMN-START COLUMN-END)

 where

  POS is the position of the error in the message (1-based)
  TYPE is one of `error', `warning' or `info'
  FILE is the file name of the error
  LINE-START, LINE-END, COLUMN-START and COLUMN-END are the reported
   line and column numbers, start and end, for that error")

(defun tuareg-test--extract-message-info (string pos)
  "Parse STRING as a compilation message.
Return (FILE TYPE START-LINE END-LINE START-COL END-COL)."
  (with-temp-buffer
    ;; This function makes some assumptions about the compilation-mode
    ;; internals and may need adjustment to work with future Emacs
    ;; versions.
    (font-lock-mode -1)
    (let ((compilation-locs (make-hash-table)))
      (insert string)
      (compilation-parse-errors (point-min) (point-max))
      (let ((msg (get-text-property pos 'compilation-message)))
        (and msg
             (let* ((loc (compilation--message->loc msg))
                    (end-loc (compilation--message->end-loc msg))
                    (type (compilation--message->type msg))
                    (start-line (compilation--loc->line loc))
                    (start-col (compilation--loc->col loc))
                    (end-line (compilation--loc->line end-loc))
                    (end-col (compilation--loc->col end-loc))
                    (fs (compilation--loc->file-struct loc))
                    (file (caar fs)))
               (list file
                     (pcase type
                       (0 'info)
                       (1 'warning)
                       (2 'error))
                     start-line end-line
                     ;; Emacs internally adds 1 to the end column so
                     ;; we compensate for that to get the actual
                     ;; number in the message.
                     start-col (and end-col (1- end-col)))))))))

(defun tuareg-test--have-end-column-bug ()
  "Check for the compilation message end-column bug."
  (let ((compilation-error-regexp-alist
         `((,(rx bol
                 (group (+ alnum)) ","
                 (group (+ digit)) ","
                 (group (+ digit)) ","
                 (group (+ digit)) ","
                 (+ digit) ";")
            1 (2 . 3) (4 . (lambda () 17))))))
    (pcase (tuareg-test--extract-message-info "abc,1,2,3,4; error\n" 1)
      (`(,_ ,_ ,_ ,_ ,_ 16) t)
      (`(,_ ,_ ,_ ,_ ,_ 17) nil)
      (x (error "%S" x)))))

(ert-deftest tuareg-compilation-message ()
  (let ((buggy-emacs-28 (and (equal emacs-major-version 28)
                             (tuareg-test--have-end-column-bug))))
    (dolist (case tuareg-test--compilation-messages)
      (let ((str (apply #'concat (nth 0 case)))
            (errors (nth 1 case)))
        (ert-info (str :prefix "message: ")
          (pcase-dolist (`(,pos ,type ,file ,start-line ,end-line
                                ,start-col ,end-col)
                         errors)
            ;; Temporary hack to make the tests pass until the Emacs snapshot
            ;; used in the CI has been updated to the version expected by
            ;; the code (ie, where the compilation message column bug has been
            ;; fixed). The bug was fixed in emacs/master
            ;; aa5437493b1ca539409495ecdc54cf420ea110b9.
            (when buggy-emacs-28
              (setq end-col (1- end-col)))
            (let ((message-info (tuareg-test--extract-message-info str pos)))
              (when (< emacs-major-version 27)
                ;; Prior to Emacs 27, a bug in compilation-mode caused the
                ;; message type to be wrong in some cases (Emacs bug#34479).
                ;; Pretend that the test passed anyway.
                (setq type (nth 1 message-info)))
              (should (equal message-info
                             (list file type
                                   start-line end-line
                                   start-col end-col))))))))))

(defun tuareg-test--comment-region (text)
  (with-temp-buffer
    (tuareg-mode)
    (insert text)
    (comment-region (point-min) (point-max))
    (buffer-string)))

(ert-deftest tuareg-comment-region-style ()
  "Check that commenting out code works as expected. See issue #216."
  ;; Non-indented code.
  (should (let ((comment-style 'indent))
            (equal (tuareg-test--comment-region
                    "let f x =\n  g x\n    y\n")
                   "(* let f x = *)\n(*   g x *)\n(*     y *)\n")))
  (should (let ((comment-style 'multi-line)
                (comment-continue " * "))
            (equal (tuareg-test--comment-region
                    "let f x =\n  g x\n    y\n")
                   "(* let f x =\n *   g x\n *     y *)\n")))
  (should (let ((comment-style 'multi-line))
            ;; `comment-continue' should default to " * "
            (equal (tuareg-test--comment-region
                    "let f x =\n  g x\n    y\n")
                   "(* let f x =\n *   g x\n *     y *)\n")))
  (should (let ((comment-style 'multi-line)
                (comment-continue "   "))
            (equal (tuareg-test--comment-region
                    "let f x =\n  g x\n    y\n")
                   "(* let f x =\n     g x\n       y *)\n")))
  ;; Indented code.
  (should (let ((comment-style 'indent))
            (equal (tuareg-test--comment-region
                    "  epsilon\n    tau\n")
                   "  (* epsilon *)\n  (*   tau *)\n")))
  (should (let ((comment-style 'multi-line)
                (comment-continue " * "))
            (equal (tuareg-test--comment-region
                    "  epsilon\n    tau\n")
                   "  (* epsilon\n   *   tau *)\n")))
  (should (let ((comment-style 'multi-line)
                (comment-continue "   "))
            (equal (tuareg-test--comment-region
                    "  epsilon\n    tau\n")
                   "  (* epsilon\n       tau *)\n"))))

(defun tuareg-test--comment-uncomment-region (text)
  (equal text
         (with-temp-buffer
           (tuareg-mode)
           (insert text)
           (comment-region (point-min) (point-max))
           (uncomment-region (point-min) (point-max))
           (buffer-string))))

(ert-deftest tuareg-comment-uncomment-region ()
  "Check that commenting out code then uncommenting it leads to
the original code."
  (should (let ((comment-style 'indent))
            (tuareg-test--comment-uncomment-region
             "let f x =\n  g x\n    y\n")))
  (should (let ((comment-style 'multi-line)
                (comment-continue " * "))
            (tuareg-test--comment-uncomment-region
             "let f x =\n  g x\n    y\n")))
  (should (let ((comment-style 'multi-line))
            (tuareg-test--comment-uncomment-region
             "let f x =\n  g x\n    y\n")))
  (should (let ((comment-style 'multi-line)
                (comment-continue "   "))
            (tuareg-test--comment-uncomment-region
             "let f x =\n  g x\n    y\n"))))

(defun tuareg-test--do-at (text pos fun)
  "Call FUN in TEXT at POS and return the resulting text."
  (with-temp-buffer
    (tuareg-mode)
    (electric-indent-mode 1)
    (insert text)
    (goto-char pos)
    (funcall fun)
    (buffer-substring-no-properties (point-min) (point-max))))

(defun tuareg-test--line-start (text line)
  "Position of start of LINE (0-based) in TEXT."
  (let ((ofs 0))
    (while (and (> line 0)
                (let ((nl (string-match-p "\n" text ofs)))
                  (setq ofs (1+ nl))
                  (setq line (1- line)))))
    (1+ ofs)))

(defun tuareg-test--do-at-line (text line fun)
  "Call FUN in TEXT at start of LINE (0-based) and return the resulting text."
  (tuareg-test--do-at text (tuareg-test--line-start text line) fun))

(ert-deftest tuareg-indent-comment-text ()
  ;; Indenting a line should use the indentation of the previous line's text.
  (should (equal (tuareg-test--do-at-line
                  (concat "  (** alpha\n"
                          "beta\n")
                  1 #'indent-for-tab-command)
                 (concat "  (** alpha\n"
                         "      beta\n")))
  ;; Tab should indent even at the end of the line.
  (should (equal (tuareg-test--do-at
                  (concat "  (** alpha\n"
                          "beta")
                  17 #'indent-for-tab-command)
                 (concat "  (** alpha\n"
                         "      beta")))
  ;; An interactive `newline' should indent the new line correctly
  ;; in Emacs 28 and later.
  (when (>= emacs-major-version 28)
    (should (equal (tuareg-test--do-at
                    "(** alpha"
                    10 (lambda () (call-interactively #'newline)))
                   (concat "(** alpha\n"
                           "    "))))
  ;; The previous line's indentation should be respected and preserved.
  (should (equal (tuareg-test--do-at-line
                  (concat "  (* alpha\n"
                          "       beta\n"
                          " gamma\n")
                  2 #'indent-for-tab-command)
                 (concat "  (* alpha\n"
                         "       beta\n"
                         "       gamma\n")))
  ;; Use the previous nonempty line for indentation.
  (should (equal (tuareg-test--do-at-line
                  (concat "  (* alpha\n"
                          "      beta\n"
                          "  \n"
                          " gamma\n")
                  3 #'indent-for-tab-command)
                 (concat "  (* alpha\n"
                         "      beta\n"
                         "  \n"
                         "      gamma\n")))
  ;; Indent text after @-tags in doc comments by 2 more spaces.
  (should (equal (tuareg-test--do-at-line
                  (concat "  (** alpha\n"
                          "        @param beta\n"
                          " gamma\n")
                  2 #'indent-for-tab-command)
                 (concat "  (** alpha\n"
                         "        @param beta\n"
                         "          gamma\n")))
  ;; An @-tag starts a new paragraph.
  (should (equal (tuareg-test--do-at-line
                  (concat "  (** alpha\n"
                          "        @param beta\n"
                          " @return gamma\n")
                  2 #'indent-for-tab-command)
                 (concat "  (** alpha\n"
                         "        @param beta\n"
                         "        @return gamma\n")))
  ;; @-tags are not special in plain comments.
  (should (equal (tuareg-test--do-at-line
                  (concat "  (* alpha\n"
                          "        @param beta\n"
                          " gamma\n")
                  2 #'indent-for-tab-command)
                 (concat "  (* alpha\n"
                         "        @param beta\n"
                         "        gamma\n")))
  ;; Filling one paragraph does not affect other paragraphs.
  (should (equal (tuareg-test--do-at-line
                  (concat "  (* alpha beta gamma\n"
                          "delta epsilon\n"
                          "\n"
                          "zeta eta theta iota kappa *)\n")
                  1 (lambda () (let ((fill-column 17))
                                 (funcall (local-key-binding (kbd "M-q"))))))
                 (concat "  (* alpha beta\n"
                         "     gamma delta\n"
                         "     epsilon\n"
                          "\n"
                          "zeta eta theta iota kappa *)\n")))
  ;; Filling affects the preceding paragraph, not the succeeding.
  (should (equal (tuareg-test--do-at-line
                  (concat "  (* alpha beta gamma\n"
                          "delta epsilon\n"
                          "\n"
                          "zeta eta theta iota kappa *)\n")
                  2 (lambda () (let ((fill-column 17))
                                 (funcall (local-key-binding (kbd "M-q"))))))
                 (concat "  (* alpha beta\n"
                         "     gamma delta\n"
                         "     epsilon\n"
                          "\n"
                          "zeta eta theta iota kappa *)\n")))

  ;; A paragraph's indentation is determined by its first line.
  (should (equal (tuareg-test--do-at-line
                  (concat "  (* alpha\n"
                          "     beta\n"
                          "\n"
                          "       gamma\n"
                          "delta epsilon zeta eta *)\n")
                  3 (lambda () (let ((fill-column 19))
                                 (funcall (local-key-binding (kbd "M-q"))))))
                  (concat "  (* alpha\n"
                          "     beta\n"
                          "\n"
                          "       gamma delta\n"
                          "       epsilon zeta\n"
                          "       eta *)\n")))
  ;; @-tags separate paragraphs in doc comments.
  (should (equal (tuareg-test--do-at-line
                  (concat "  (** alpha\n"
                          "      beta\n"
                          "       @param gamma delta epsilon\n"
                          "       @param zeta eta theta iota\n")
                  4 (lambda () (let ((fill-column 25))
                                 (funcall (local-key-binding (kbd "M-q"))))))
                  (concat "  (** alpha\n"
                          "      beta\n"
                          "       @param gamma delta epsilon\n"
                          "       @param zeta eta\n"
                          "         theta iota\n")))
  )


(provide 'tuareg-tests)
