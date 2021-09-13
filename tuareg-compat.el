;;; tuareg-compat.el                  -*- lexical-binding:t -*-

;; FIX: make sure `comment-region' supports `comment-continue' made
;; only of spaces (and in a consistent fashion even for older Emacs).

(require 'newcomment)

;; Emacs < 26
(defun tuareg--comment-padright--advice (orig-fun &rest args)
  (let ((str (nth 0 args)))
    (unless (and (eq major-mode 'tuareg-mode)
                 (stringp str) (not (string-match "\\S-" str)))
      (apply orig-fun args))))

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

(defun tuareg--comment-region-default--advice (orig-fun &rest args)
  (apply (if (eq major-mode 'tuareg-mode)
             'tuareg--comment-region-default
           orig-fun)
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

(defun tuareg--comment-region-default-1--advice (orig-fun &rest args)
  (apply (if (eq major-mode 'tuareg-mode)
             'tuareg--comment-region-default-1
           orig-fun)
         args))

(when (and (= emacs-major-version 27) (fboundp 'comment-region-default-1))
  (advice-add 'comment-region-default-1 :around
              #'tuareg--comment-region-default-1--advice))

;; FIX: uncommenting

;; Emacs < 27
(defun tuareg--uncomment-region-default (beg end &optional arg)
  "Uncomment each line in the BEG .. END region.
The numeric prefix ARG can specify a number of chars to remove from the
comment delimiters.
This function is the default value of `uncomment-region-function'."
  (goto-char beg)
  (setq end (copy-marker end))
  (let* ((numarg (prefix-numeric-value arg))
	 (ccs comment-continue)
	 (srei (or (comment-padright ccs 're)
                   (and (stringp comment-continue) comment-continue)))
	 (csre (comment-padright comment-start 're))
	 (sre (and srei (concat "^\\s-*?\\(" srei "\\)")))
	 spt)
    (while (and (< (point) end)
		(setq spt (comment-search-forward end t)))
      (let ((ipt (point))
	    ;; Find the end of the comment.
	    (ept (progn
		   (goto-char spt)
		   (unless (or (comment-forward)
			       ;; Allow non-terminated comments.
			       (eobp))
		     (error "Can't find the comment end"))
		   (point)))
	    (box nil)
	    (box-equal nil))	   ;Whether we might be using `=' for boxes.
	(save-restriction
	  (narrow-to-region spt ept)

	  ;; Remove the comment-start.
	  (goto-char ipt)
	  (skip-syntax-backward " ")
	  ;; A box-comment starts with a looong comment-start marker.
	  (when (and (or (and (= (- (point) (point-min)) 1)
			      (setq box-equal t)
			      (looking-at "=\\{7\\}")
			      (not (eq (char-before (point-max)) ?\n))
			      (skip-chars-forward "="))
			 (> (- (point) (point-min) (length comment-start)) 7))
		     (> (count-lines (point-min) (point-max)) 2))
	    (setq box t))
	  ;; Skip the padding.  Padding can come from comment-padding and/or
	  ;; from comment-start, so we first check comment-start.
	  (if (or (save-excursion (goto-char (point-min)) (looking-at csre))
		  (looking-at (regexp-quote comment-padding)))
	      (goto-char (match-end 0)))
	  (when (and sre (looking-at (concat "\\s-*\n\\s-*" srei)))
	    (goto-char (match-end 0)))
	  (if (null arg) (delete-region (point-min) (point))
            (let ((opoint (point-marker)))
              (skip-syntax-backward " ")
              (delete-char (- numarg))
              (unless (and (not (bobp))
                           (save-excursion (goto-char (point-min))
                                           (looking-at comment-start-skip)))
                ;; If there's something left but it doesn't look like
                ;; a comment-start any more, just remove it.
                (delete-region (point-min) opoint))))

	  ;; Remove the end-comment (and leading padding and such).
	  (goto-char (point-max)) (comment-enter-backward)
	  ;; Check for special `=' used sometimes in comment-box.
	  (when (and box-equal (not (eq (char-before (point-max)) ?\n)))
	    (let ((pos (point)))
	      ;; skip `=' but only if there are at least 7.
	      (when (> (skip-chars-backward "=") -7) (goto-char pos))))
	  (unless (looking-at "\\(\n\\|\\s-\\)*\\'")
	    (when (and (bolp) (not (bobp))) (backward-char))
	    (if (null arg) (delete-region (point) (point-max))
	      (skip-syntax-forward " ")
	      (delete-char numarg)
	      (unless (or (eobp) (looking-at comment-end-skip))
		;; If there's something left but it doesn't look like
		;; a comment-end any more, just remove it.
		(delete-region (point) (point-max)))))

	  ;; Unquote any nested end-comment.
	  (comment-quote-nested comment-start comment-end t)

	  ;; Eliminate continuation markers as well.
	  (when sre
	    (let* ((cce (comment-string-reverse (or comment-continue
						    comment-start)))
		   (erei (and box (comment-padleft cce 're)))
		   (ere (and erei (concat "\\(" erei "\\)\\s-*$"))))
	      (goto-char (point-min))
	      (while (progn
		       (if (and ere (re-search-forward
				     ere (line-end-position) t))
			   (replace-match "" t t nil (if (match-end 2) 2 1))
			 (setq ere nil))
		       (forward-line 1)
		       (re-search-forward sre (line-end-position) t))
		(replace-match "" t t nil (if (match-end 2) 2 1)))))
	  ;; Go to the end for the next comment.
	  (goto-char (point-max))))))
  (set-marker end nil))

(defun tuareg--uncomment-region-default--advice (orig-fun &rest args)
  (apply (if (eq major-mode 'tuareg-mode)
             'tuareg--uncomment-region-default
           orig-fun)
         args))

(when (and (< emacs-major-version 27) (fboundp 'uncomment-region-default))
  (advice-add 'uncomment-region-default :around
              #'tuareg--uncomment-region-default--advice))

;; Emacs 27
(defun tuareg--uncomment-region-default-1 (beg end &optional arg)
  "Uncomment each line in the BEG .. END region.
The numeric prefix ARG can specify a number of chars to remove from the
comment delimiters.
This function is the default value of `uncomment-region-function'."
  (goto-char beg)
  (setq end (copy-marker end))
  (let* ((numarg (prefix-numeric-value arg))
	 (ccs comment-continue)
	 (srei (or (comment-padright ccs 're)
                   (and (stringp comment-continue) comment-continue)))
	 (csre (comment-padright comment-start 're))
	 (sre (and srei (concat "^\\s-*?\\(" srei "\\)")))
	 spt)
    (while (and (< (point) end)
		(setq spt (comment-search-forward end t)))
      (let ((ipt (point))
	    ;; Find the end of the comment.
	    (ept (progn
		   (goto-char spt)
		   (unless (or (comment-forward)
			       ;; Allow non-terminated comments.
			       (eobp))
		     (error "Can't find the comment end"))
		   (point)))
	    (box nil)
	    (box-equal nil))	   ;Whether we might be using `=' for boxes.
	(save-restriction
	  (narrow-to-region spt ept)

	  ;; Remove the comment-start.
	  (goto-char ipt)
	  (skip-syntax-backward " ")
	  ;; A box-comment starts with a looong comment-start marker.
	  (when (and (or (and (= (- (point) (point-min)) 1)
			      (setq box-equal t)
			      (looking-at "=\\{7\\}")
			      (not (eq (char-before (point-max)) ?\n))
			      (skip-chars-forward "="))
			 (> (- (point) (point-min) (length comment-start)) 7))
		     (> (count-lines (point-min) (point-max)) 2))
	    (setq box t))
	  ;; Skip the padding.  Padding can come from comment-padding and/or
	  ;; from comment-start, so we first check comment-start.
	  (if (or (save-excursion (goto-char (point-min)) (looking-at csre))
		  (looking-at (regexp-quote comment-padding)))
	      (goto-char (match-end 0)))
	  (when (and sre (looking-at (concat "\\s-*\n\\s-*" srei)))
	    (goto-char (match-end 0)))
	  (if (null arg) (delete-region (point-min) (point))
            (let ((opoint (point-marker)))
              (skip-syntax-backward " ")
              (delete-char (- numarg))
              (unless (and (not (bobp))
                           (save-excursion (goto-char (point-min))
                                           (looking-at comment-start-skip)))
                ;; If there's something left but it doesn't look like
                ;; a comment-start any more, just remove it.
                (delete-region (point-min) opoint))))

	  ;; Remove the end-comment (and leading padding and such).
	  (goto-char (point-max)) (comment-enter-backward)
	  ;; Check for special `=' used sometimes in comment-box.
	  (when (and box-equal (not (eq (char-before (point-max)) ?\n)))
	    (let ((pos (point)))
	      ;; skip `=' but only if there are at least 7.
	      (when (> (skip-chars-backward "=") -7) (goto-char pos))))
	  (unless (looking-at "\\(\n\\|\\s-\\)*\\'")
	    (when (and (bolp) (not (bobp))) (backward-char))
	    (if (null arg) (delete-region (point) (point-max))
	      (skip-syntax-forward " ")
	      (delete-char numarg)
	      (unless (or (eobp) (looking-at comment-end-skip))
		;; If there's something left but it doesn't look like
		;; a comment-end any more, just remove it.
		(delete-region (point) (point-max)))))

	  ;; Unquote any nested end-comment.
	  (comment-quote-nested comment-start comment-end t)

	  ;; Eliminate continuation markers as well.
	  (when sre
	    (let* ((cce (comment-string-reverse (or comment-continue
						    comment-start)))
		   (erei (and box (comment-padleft cce 're)))
		   (ere (and erei (concat "\\(" erei "\\)\\s-*$"))))
	      (goto-char (point-min))
	      (while (progn
		       (if (and ere (re-search-forward
				     ere (line-end-position) t))
			   (replace-match "" t t nil (if (match-end 2) 2 1))
			 (setq ere nil))
		       (forward-line 1)
		       (re-search-forward sre (line-end-position) t))
		(replace-match "" t t nil (if (match-end 2) 2 1)))))
	  ;; Go to the end for the next comment.
	  (goto-char (point-max)))
        ;; Remove any obtrusive spaces left preceding a tab at `spt'.
        (when (and (eq (char-after spt) ?\t) (eq (char-before spt) ? )
                   (> tab-width 0))
          (save-excursion
            (goto-char spt)
            (let* ((fcol (current-column))
                   (slim (- (point) (mod fcol tab-width))))
              (delete-char (- (skip-chars-backward " " slim)))))))))
  (set-marker end nil))

(defun tuareg--uncomment-region-default-1--advice (orig-fun &rest args)
  (apply (if (eq major-mode 'tuareg-mode)
             'tuareg--uncomment-region-default-1
           orig-fun)
         args))

(when (and (<= emacs-major-version 28) (fboundp 'uncomment-region-default-1))
  (advice-add 'uncomment-region-default-1 :around
              #'tuareg--uncomment-region-default-1--advice))

(provide 'tuareg-compat)
