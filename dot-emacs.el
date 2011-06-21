
;;; Tuareg quick installation: Append this file to .emacs.

(add-to-list 'auto-mode-alist '("\\.ml[iylp]?" . tuareg-mode))
(autoload 'tuareg-mode "tuareg" "Major mode for editing Caml code" t)
(autoload 'camldebug "camldebug" "Run the Caml debugger" t)
(dolist (ext '(".cmo" ".cmx" ".cma" ".cmxa" ".cmi"))
  (add-to-list 'completion-ignored-extensions ext))

;;; Sample customization

(add-hook 'tuareg-mode-hook
  '(lambda ()
     ;; indent `=' like a standard keyword
     (setq tuareg-lazy-= t)
     ;; indent [({ like standard keywords
     (setq tuareg-lazy-paren t)
     ;; no indentation after `in' keywords
     (setq tuareg-in-indent 0)
     ;; turn on auto-fill minor mode
     (auto-fill-mode 1)))
