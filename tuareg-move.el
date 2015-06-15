;;; tuareg-move.el --- Navigate tuareg source

;; Copyright (C) 2015  Andreas Roehler

;; Author: Andreas Roehler <andreas.roehler@online.de>
;; Keywords: languages

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

(defun tuareg--beginning-of-expression-p ()
  "Returns position, if cursor is at the beginning of a `expression', nil otherwise. "
  (let ((orig (point))
        erg)
    (save-excursion
      (unless (and (eolp)(bolp))
        (tuareg-forward-expression))
      (tuareg-backward-expression)
      (when (eq orig (point))
        (setq erg orig))
      erg)))

(defun tuareg--end-of-expression-p ()
  "Returns position, if cursor is at the end of a expression, nil otherwise. "
  (let ((orig (point))
	erg)
    (save-excursion
      (tuareg-backward-expression)
      (tuareg-forward-expression)
      (when (eq orig (point))
	(setq erg orig))
      erg)))

(defun tuareg-end-of-literal (&optional beginning-of-string-position)
  "Go to end of string at point if any, if successful return position. "
  (interactive)
  (let ((orig (point))
	(start (tuareg-in-literal-p)))
    (when start
      (goto-char start)
      (forward-sexp)
      (and (< orig (point)) (setq erg (point))))
    (when (and tuareg-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defun tuareg-backward-expression (&optional orig done limit)
  "Go to the initial line of a simple expression.

For beginning of compound expression use tuareg-backward-block.
For beginning of clause tuareg-backward-clause. "
  (interactive)
  (save-restriction
    (unless (bobp)
      (let* ((orig (or orig (point)))
             (this (point))
             (cui (current-indentation))

             (pps (progn (goto-char this)
                         (parse-partial-sexp (or limit (point-min))(point))))
             (done done)
             erg)
        (unless done
          (and (< 0 (abs (skip-chars-backward " \t\r\n\f")))
               (setq pps (parse-partial-sexp (or limit (point-min))(point)))))
        (cond
         ((and (bolp)(eolp))
          (skip-chars-backward " \t\r\n\f")
          (tuareg-backward-expression orig done limit))
         ((nth 8 pps)
          ;; inside string
          (and (nth 3 pps) (setq done t))
          (goto-char (nth 8 pps))
          (tuareg-backward-expression orig done limit))
         ((nth 1 pps)
          (goto-char (nth 1 pps))
          (tuareg--skip-to-semicolon-backward
           (save-excursion (back-to-indentation)(point)))
          (setq done t)
          (tuareg-backward-expression orig done limit))
         ((tuareg--preceding-line-backslashed-p)
          (forward-line -1)
          (back-to-indentation)
          (setq done t)
          (tuareg-backward-expression orig done limit))
         ;; BOL or at space before comment
         ((and (looking-at "[ \t]*#")(looking-back "^[ \t]*"))
          (forward-comment -1)
          (while (and (not (bobp))
                      (looking-at "[ \t]*#")(looking-back "^[ \t]*"))
            (forward-comment -1))
          (unless (bobp)
            (tuareg-backward-expression orig done limit)))
         ;; at inline comment
         ((looking-at "[ \t]*#")
          (when (tuareg--skip-to-semicolon-backward
                 (save-excursion (back-to-indentation)(point)))
            (skip-chars-forward " \t")
            (unless (bobp)
              (tuareg-backward-expression orig done limit))))
         ;; at beginning of string
         ((and (not done) (looking-at tuareg-literal-delim-re))
          (when (< 0 (abs (skip-chars-backward " \t\r\n\f")))
            (setq done t))
          (back-to-indentation)
          (tuareg-backward-expression orig done limit))
         ;; after end of expression
         ((and (not done) (eq (char-before) ?\;))
          (skip-chars-backward ";")
          (tuareg-backward-expression orig done limit))
         ;; travel until indentation or semicolon
         ((and (not done)
               (tuareg--skip-to-semicolon-backward
                (save-excursion (back-to-indentation)(point))))
          (tuareg-backward-expression orig done limit))
         ;; at current indent
         ((and (not done) (not (eq 0 (skip-chars-backward " \t\r\n\f"))))
          (tuareg-backward-expression orig done limit))
         ((and (member (char-after) (list ?\" ?\'))
               (progn (back-to-indentation) (eq ?@ (char-after))))
          (back-to-indentation) (setq done t)
          (tuareg-backward-expression orig done limit)))
        ;; return nil when before comment
        (unless (and (looking-at "[ \t]*#") (looking-back "^[ \t]*"))
          (when (< (point) orig)(setq erg (point))))
        (when (and tuareg-verbose-p (interactive-p)) (message "%s" erg))
        erg))))

(defun tuareg-forward-expression (&optional orig done repeat)
  "Go to the last char of current expression.

Optional argument REPEAT, the number of loops done already,
is checked for tuareg-max-specpdl-size error.
Avoid eternal loops due to missing string delimters etc. "
  (interactive)
  (unless (eobp)
    (let ((repeat (or (and repeat (1+ repeat)) 0))
          (orig (or orig (point)))
          erg pos last
          ;; use by scan-lists
          parse-sexp-ignore-comments
          forward-sexp-function
          stringchar stm pps err)
      (unless done (tuareg--skip-to-comment-or-semicolon))
      (setq pps (parse-partial-sexp (point-min) (point)))
      ;; (origline (or origline (tuareg-count-lines)))
      (cond
       ((< tuareg-max-specpdl-size repeat)
        (error "tuareg-forward-expression reached loops max.
If no error, customize `tuareg-max-specpdl-size'"))
       ;; list
       ((nth 1 pps)
        (if (<= orig (point))
            (progn
              (setq orig (point))
              ;; do not go back at a possible unclosed list
              (goto-char (nth 1 pps))
              (let ((parse-sexp-ignore-comments t))
                (if
                    (ignore-errors (forward-list))
                    (progn
                      (when (looking-at ":[ \t]*$")
                        (forward-char 1))
                      (setq done t)
                      (skip-chars-forward "^#" (line-end-position))
                      (skip-chars-backward " \t\r\n\f"
                                           (line-beginning-position))
                      (tuareg-forward-expression orig done repeat))
                  (setq err (tuareg--record-list-error pps))
                  (goto-char orig))))))
       ;; string
       ((nth 3 pps)
        (when (tuareg-end-of-literal)
          (end-of-line)
          (skip-chars-backward " \t\r\n\f")
          (setq pps (parse-partial-sexp (point-min) (point)))
          (unless (and done
                       (not (or (nth 1 pps) (nth 8 pps)))
                       (eolp))
            (tuareg-forward-expression orig done repeat))))
       ;; in non-terminated string

       ;; in comment
       ((nth 4 pps)
        (tuareg--end-of-comment-intern (point))
        (tuareg--skip-to-comment-or-semicolon)
        (while (and (eq (char-before (point)) ?\\ )
                    (tuareg--escaped)(setq last (point)))
          (forward-line 1)(end-of-line))
        (and last (goto-char last)
             (forward-line 1)
             (back-to-indentation))
        (tuareg-forward-expression orig done repeat))
       ((tuareg--current-line-backslashed-p)
        (end-of-line)
        (skip-chars-backward " \t\r\n\f" (line-beginning-position))
        (while (and (eq (char-before (point)) ?\\ )
                    (tuareg--escaped))
          (forward-line 1)
          (end-of-line)
          (skip-chars-backward " \t\r\n\f" (line-beginning-position)))
        (unless (eobp)
          (tuareg-forward-expression orig done repeat)))
       ((eq orig (point))
        (skip-chars-forward " \t\r\n\f#'\"")
        (tuareg--skip-to-comment-or-semicolon)
        (tuareg-forward-expression orig done repeat))
       ((eq (current-indentation) (current-column))
        (tuareg--skip-to-comment-or-semicolon)
        ;; (setq pps (parse-partial-sexp (point-min) (point)))
        (unless done
          (tuareg-forward-expression orig done repeat)))

       ((and (looking-at "[[:print:]]+$")
             (not done)
             (tuareg--skip-to-comment-or-semicolon))
        (tuareg-forward-expression orig done repeat)))
      (unless
          (or
           (eq (point) orig)
           (member (char-before) (list 10 32 9)))
        (setq erg (point)))
      (if (and tuareg-verbose-p err)
          (tuareg--message-error err)
        (and tuareg-verbose-p (interactive-p) (message "%s" erg)))
      erg)))

(defun tuareg-forward-expression-bol ()
  "Go to the beginning-of-line following current expression."
  (interactive)
  (let ((erg (tuareg-forward-expression)))
    (setq erg (tuareg--beginning-of-line-form))
    (when (and tuareg-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defun tuareg-down-expression ()
  "Go to the beginning of next expression downwards in buffer.

Return position if expression found, nil otherwise. "
  (interactive)
  (let* ((orig (point))
         (erg
          (cond ((tuareg--end-of-expression-p)
                 (and (tuareg-forward-expression) (tuareg-backward-expression)))
                ((ignore-errors (< orig (progn (tuareg-forward-expression) (tuareg-backward-expression))))
                 (point))
                (t (goto-char orig) (and (tuareg-forward-expression) (tuareg-forward-expression)(tuareg-backward-expression))))))
    (when (and tuareg-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defun tuareg-backward-top-level ()
  "Go up to beginning of statments until level of indentation is null.

Returns position if successful, nil otherwise "
  (interactive)
  (let (erg)
    (unless (bobp)
      (while (and (not (bobp)) (setq erg (tuareg-backward-expression))
                  (or (< 0 (current-indentation))(looking-at "\\_<end\\_>"))))
      (when (and tuareg-verbose-p (interactive-p)) (message "%s" erg))
      erg)))

(defun tuareg--forward-top-level-intern ()
  (let (last)
    (unless (tuareg--beginning-of-expression-p)
      (tuareg-backward-expression))
    (unless (eq 0 (current-column))
      (tuareg-backward-top-level))
    (unless (< orig (point))
      (while (and
	      (not (eobp))
	      (save-excursion
		(tuareg-forward-expression)
		(setq last (point)))
	      (tuareg-down-expression)(< 0 (current-indentation)))))
    ;; (if (looking-at (tuareg-rx builtin-declaration))
    ;; (tuareg-forward-top-level)
    (and last (goto-char last))
    ;; (tuareg-forward-expression)
    ;;)
    ))

(defun tuareg-forward-top-level ()
  "Go to end of a top-level form.

Returns position if successful, nil otherwise"
  (interactive)
  (let ((orig (point))
        erg)
    (unless (eobp)
      (if (and (tuareg--forward-top-level-intern)
	       (< orig (point)))
	  (setq erg (point))
	(tuareg-down-expression)
	(tuareg-forward-top-level)))
    (when (and tuareg-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defun tuareg-forward-top-level-bol ()
  "Go to beginning of line after end of a top-level form.

Returns position if successful, nil otherwise"
  (interactive)
  (let ((orig (point))
        erg last)
    (unless (eobp)
      (when (tuareg--forward-top-level-intern)
	(if (eobp)
	    (newline)
	  (forward-line 1)
	  (beginning-of-line)))
      (when (< orig (point))
	(setq erg (point))))
    (when (and tuareg-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(provide 'tuareg-move)
;;; tuareg-move.el ends here
