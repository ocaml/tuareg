(require 'yuareg)

;; See README
(setq yuareg-indent-align-with-first-arg nil)

(add-hook
 'yuareg-mode-hook
 (lambda()
   (setq show-trailing-whitespace t)
   (setq indicate-empty-lines t)

   ;; Enable the representation of some keywords using fonts
   (when (functionp 'prettify-symbols-mode)
     (prettify-symbols-mode))

   (when (functionp 'flyspell-prog-mode)
     (flyspell-prog-mode))
   ;; See README
   ;;(setq yuareg-match-patterns-aligned t)
   ;;(electric-indent-mode 0)
   ))


;; Easy keys to navigate errors after compilation:
(define-key yuareg-mode-map [(f12)] 'next-error)
(define-key yuareg-mode-map [(shift f12)] 'previous-error)


;; Use Merlin if available
(when (require 'merlin nil t)
  (setq merlin-command 'opam)
  (add-to-list 'auto-mode-alist '("/\\.merlin\\'" . conf-mode))

  (when (functionp 'merlin-document)
    (define-key yuareg-mode-map (kbd "\C-c\C-h") 'merlin-document))

  ;; Run Merlin if a .merlin file in the parent dirs is detected
  (add-hook 'yuareg-mode-hook
            (lambda()
              (let ((fn (buffer-file-name)))
                (if (and fn (locate-dominating-file fn ".merlin"))
                    (merlin-mode))))))

;; Choose modes for related config. files
(setq auto-mode-alist
      (append '(("_oasis\\'" . conf-mode)
		("_tags\\'" . conf-mode)
		("_log\\'" . conf-mode))
	      auto-mode-alist))
