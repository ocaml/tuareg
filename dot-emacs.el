;; -*- lexical-binding: t; -*-
;; (require 'tuareg)

;; See README
(setq tuareg-indent-align-with-first-arg nil)

(add-hook 'tuareg-mode-hook #'my-tuareg-mode-setup)
(defun my-tuareg-mode-setup ()
   (setq show-trailing-whitespace t)
   (setq indicate-empty-lines t)

   ;; Enable the representation of some keywords using fonts
   (prettify-symbols-mode)

   (flyspell-prog-mode)                ;Spell check strings and comments.

   ;; Easy keys to navigate errors after compilation:
   (define-key tuareg-mode-map [(f12)] #'next-error)
   (define-key tuareg-mode-map [(shift f12)] #'previous-error)

   ;; See README
   ;;(setq tuareg-match-patterns-aligned t)
   ;;(electric-indent-mode 0)
   )


;;;; Use Merlin when available ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; FIXME: Should be included in Merlin's own support code.
(add-to-list 'auto-mode-alist '("/\\.merlin\\'" . conf-mode))

(with-eval-after-load 'merlin
  (when (fboundp 'merlin-document)
    (with-eval-after-load 'tuareg
      (define-key tuareg-mode-map (kbd "\C-c\C-h") #'merlin-document))))

(when (fboundp 'merlin-mode)
  ;; Run Merlin if a .merlin file in the parent dirs is detected
  (add-hook 'tuareg-mode-hook
            (lambda()
              (let ((fn (buffer-file-name)))
                (if (and fn (locate-dominating-file fn ".merlin"))
                    (merlin-mode))))))

;;;; Choose modes for related config files ;;;;;;;;;;;;;;;;;;
(setq auto-mode-alist
      ;; FIXME: Are these still in use?  Which tools are they used for?
      (append '(("_oasis\\'" . conf-mode)
		("_tags\\'" . conf-mode)
		("_log\\'" . conf-mode))
	      auto-mode-alist))
