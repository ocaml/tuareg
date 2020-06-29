;;; tests for tuareg.el                       -*- lexical-binding: t -*-

(require 'tuareg)
(require 'ert)

(ert-deftest tuareg-beginning-of-defun ()
  ;; Check that `beginning-of-defun' works as expected: move backwards
  ;; to the beginning of the current top-level definition (defun), or
  ;; the previous one if already at the beginning; return t if one was
  ;; found, nil if none.
  (with-temp-buffer
    (tuareg-mode)
    (let (p1 p2 p3 p4)
      (insert "(* first line *)\n\n")
      (setq p1 (point))
      (insert "type ty =\n"
              "  | Goo\n"
              "  | Baa of int\n\n")
      (setq p2 (point))
      (insert "let a = ho hum\n"
              ";;\n\n")
      (setq p3 (point))
      (insert "let g u =\n"
              "  while mo ma do\n"
              "    we wo;\n")
      (setq p4 (point))
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

(provide 'tuareg-tests)
