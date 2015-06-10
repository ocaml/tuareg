;;; tuareg-test.el --- ocaml tuareg-mode tests

;; Copyright (C) 2015  Andreas Röhler

;; Author: Andreas Röhler <andreas.roehler@easy-emacs.de>

;; Keywords: languages, lisp

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(ert-deftest tuareg-ert-in-comment-p-test ()
  (tuareg-test-with-temp-buffer
      "(* "
    (should (tuareg-in-comment-p))))

(ert-deftest tuareg-ert-in-multiline-comment-p-test ()
  (tuareg-test-with-temp-buffer
      "(*
* "
    (should (tuareg-in-comment-p))))

(ert-deftest tuareg-ert-in-string-p-test ()
  (tuareg-test-with-temp-buffer
      "\" "
    (should (tuareg-in-string-p))))

(provide 'tuareg-test)
;;; tuareg-test.el ends here
