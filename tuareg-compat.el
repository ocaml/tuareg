;;; tuareg-compat.el                  -*- lexical-binding:t -*-

;; FIX: make sure `comment-region' supports `comment-continue' made
;; only of spaces (and in a consistent fashion even for older Emacs).

(require 'newcomment)

;; Emacs < 26

(defun tuareg--comment-padright--advice (origfn &rest args)
  (let ((str (nth 0 args)))
    (unless (and (eq major-mode 'tuareg-mode)
                 (stringp str) (not (string-match "\\S-" str)))
      (apply origfn args))))

(when (and (< emacs-major-version 26) (fboundp 'comment-region-default))
  (advice-add 'comment-padright :around #'tuareg--comment-padright--advice))

;; Emacs < 27
(defun tuareg--comment-region-default (beg end &optional arg)
  (let* ((numarg (prefix-numeric-value arg))
	 (style (cdr (assoc comment-style comment-styles)))
	 (lines (nth 2 style))
	 (block (nth 1 style))
	 (multi (nth 0 style)))

    ;; We use `chars' instead of `syntax' because `\n' might be
    ;; of end-comment syntax rather than of whitespace syntax.
    ;; sanitize BEG and END
    (goto-char beg) (skip-chars-forward " \t\n\r") (beginning-of-line)
    (setq beg (max beg (point)))
    (goto-char end) (skip-chars-backward " \t\n\r") (end-of-line)
    (setq end (min end (point)))
    (if (>= beg end) (error "Nothing to comment"))

    ;; sanitize LINES
    (setq lines
	  (and
	   lines ;; multi
	   (progn (goto-char beg) (beginning-of-line)
		  (skip-syntax-forward " ")
		  (>= (point) beg))
	   (progn (goto-char end) (end-of-line) (skip-syntax-backward " ")
		  (<= (point) end))
	   (or block (not (string= "" comment-end)))
           (or block (progn (goto-char beg) (re-search-forward "$" end t)))))

    ;; don't add end-markers just because the user asked for `block'
    (unless (or lines (string= "" comment-end)) (setq block nil))

    (cond
     ((consp arg) (uncomment-region beg end))
     ((< numarg 0) (uncomment-region beg end (- numarg)))
     (t
      (let ((multi-char (/= (string-match "[ \t]*\\'" comment-start) 1))
	    indent triple)
	(if (eq (nth 3 style) 'multi-char)
	    (save-excursion
	      (goto-char beg)
	      (setq indent multi-char
		    ;; Triple if we will put the comment starter at the margin
		    ;; and the first line of the region isn't indented
		    ;; at least two spaces.
		    triple (and (not multi-char) (looking-at "\t\\|  "))))
	  (setq indent (nth 3 style)))

	;; In Lisp and similar modes with one-character comment starters,
	;; double it by default if `comment-add' says so.
	;; If it isn't indented, triple it.
	(if (and (null arg) (not multi-char))
	    (setq numarg (* comment-add (if triple 2 1)))
	  (setq numarg (1- (prefix-numeric-value arg))))

	(comment-region-internal
	 beg end
	 (let ((s (comment-padright comment-start numarg)))
	   (if (string-match comment-start-skip s) s
	     (comment-padright comment-start)))
	 (let ((s (comment-padleft comment-end numarg)))
	   (and s (if (string-match comment-end-skip s) s
		    (comment-padright comment-end))))
	 (if multi (or (comment-padright comment-continue numarg)
                       (and (stringp comment-continue) comment-continue)))
	 (if multi
	     (comment-padleft (comment-string-reverse comment-continue) numarg))
	 block
	 lines
	 indent))))))

(defun tuareg--comment-region-default--advice (origfn &rest args)
  (apply (if (eq major-mode 'tuareg-mode)
             'tuareg--comment-region-default
           origfn)
         args))

(when (and (< emacs-major-version 27) (fboundp 'comment-region-default))
  (advice-add 'comment-region-default :around
              #'tuareg--comment-region-default--advice))


;; Emacs 27
(defun tuareg--comment-region-default-1 (beg end &optional arg noadjust)
  "Comment region between BEG and END.
See `comment-region' for ARG.  If NOADJUST, do not skip past
leading/trailing space when determining the region to comment
out."
  (let* ((numarg (prefix-numeric-value arg))
	 (style (cdr (assoc comment-style comment-styles)))
	 (lines (nth 2 style))
	 (block (nth 1 style))
	 (multi (nth 0 style)))

    (if noadjust
        (when (bolp)
          (setq end (1- end)))
      ;; We use `chars' instead of `syntax' because `\n' might be
      ;; of end-comment syntax rather than of whitespace syntax.
      ;; sanitize BEG and END
      (goto-char beg)
      (skip-chars-forward " \t\n\r")
      (beginning-of-line)
      (setq beg (max beg (point)))
      (goto-char end)
      (skip-chars-backward " \t\n\r")
      (end-of-line)
      (setq end (min end (point)))
      (when (>= beg end)
        (error "Nothing to comment")))

    ;; sanitize LINES
    (setq lines
	  (and
	   lines ;; multi
	   (progn (goto-char beg) (beginning-of-line)
		  (skip-syntax-forward " ")
		  (>= (point) beg))
	   (progn (goto-char end) (end-of-line) (skip-syntax-backward " ")
		  (<= (point) end))
	   (or block (not (string= "" comment-end)))
           (or block (progn (goto-char beg) (re-search-forward "$" end t)))))

    ;; don't add end-markers just because the user asked for `block'
    (unless (or lines (string= "" comment-end)) (setq block nil))

    (cond
     ((consp arg) (uncomment-region beg end))
     ((< numarg 0) (uncomment-region beg end (- numarg)))
     (t
      (let ((multi-char (/= (string-match "[ \t]*\\'" comment-start) 1))
	    indent triple)
	(if (eq (nth 3 style) 'multi-char)
	    (save-excursion
	      (goto-char beg)
	      (setq indent multi-char
		    ;; Triple if we will put the comment starter at the margin
		    ;; and the first line of the region isn't indented
		    ;; at least two spaces.
		    triple (and (not multi-char) (looking-at "\t\\|  "))))
	  (setq indent (nth 3 style)))

	;; In Lisp and similar modes with one-character comment starters,
	;; double it by default if `comment-add' says so.
	;; If it isn't indented, triple it.
	(if (and (null arg) (not multi-char))
	    (setq numarg (* comment-add (if triple 2 1)))
	  (setq numarg (1- (prefix-numeric-value arg))))

	(comment-region-internal
	 beg end
	 (let ((s (comment-padright comment-start numarg)))
	   (if (string-match comment-start-skip s) s
	     (comment-padright comment-start)))
	 (let ((s (comment-padleft comment-end numarg)))
	   (and s (if (string-match comment-end-skip s) s
		    (comment-padright comment-end))))
	 (if multi
             (or (comment-padright comment-continue numarg)
                 ;; `comment-padright' returns nil when
                 ;; `comment-continue' contains only whitespace
                 (and (stringp comment-continue) comment-continue)))
	 (if multi
	     (comment-padleft (comment-string-reverse comment-continue) numarg))
	 block
	 lines
	 indent))))))

(defun tuareg--comment-region-default-1--advice (origfn &rest args)
  (apply (if (eq major-mode 'tuareg-mode)
             'tuareg--comment-region-default-1
           origfn)
         args))

(when (and (= emacs-major-version 27) (fboundp 'comment-region-default-1))
  (advice-add 'comment-region-default-1 :around
              #'tuareg--comment-region-default-1--advice))


(provide 'tuareg-compat)
