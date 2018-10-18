;;; yuareg-site-file.el --- Automatically extracted autoloads.
;;; Code:
(add-to-list 'load-path
             (or (file-name-directory load-file-name) (car load-path)))

;;;### (autoloads nil "ocamldebug" "ocamldebug.el" (23482 1640 856093
;;;;;;  784000))
;;; Generated autoloads from ocamldebug.el

(autoload 'ocamldebug "ocamldebug" "\
Run ocamldebug on program FILE in buffer *ocamldebug-FILE*.
The directory containing FILE becomes the initial working directory
and source-file directory for ocamldebug.  If you wish to change this, use
the ocamldebug commands `cd DIR' and `directory'.

\(fn PGM-PATH)" t nil)

(defalias 'camldebug 'ocamldebug)

;;;***

;;;### (autoloads nil "yuareg" "yuareg.el" (23496 32331 82006 230000))
;;; Generated autoloads from yuareg.el
(add-to-list 'auto-mode-alist '("\\.ml[ip]?\\'" . yuareg-mode))
(add-to-list 'auto-mode-alist '("\\.eliomi?\\'" . yuareg-mode))
(dolist (ext '(".cmo" ".cmx" ".cma" ".cmxa" ".cmi"
               ".annot" ".cmt" ".cmti"))
 (add-to-list 'completion-ignored-extensions ext))

(autoload 'yuareg-mode "yuareg" "\
Major mode for editing OCaml code.

Dedicated to Emacs and XEmacs, version 21 and higher.  Provides
automatic indentation and compilation interface.  Performs font/color
highlighting using Font-Lock.  It is designed for OCaml but handles
Caml Light as well.

The Font-Lock minor-mode is used according to your customization
options.

You have better byte-compile yuareg.el.

For customization purposes, you should use `yuareg-mode-hook'
\(run for every file) or `yuareg-load-hook' (run once) and not patch
the mode itself.  You should add to your configuration file something like:
  (add-hook 'yuareg-mode-hook
            (lambda ()
               ... ; your customization code
            ))
For example you can change the indentation of some keywords, the
`electric' flags, Font-Lock colors... Every customizable variable is
documented, use `C-h-v' or look at the mode's source code.

`dot-emacs.el' is a sample customization file for standard changes.
You can append it to your `.emacs' or use it as a tutorial.

`M-x ocamldebug' FILE starts the OCaml debugger ocamldebug on the executable
FILE, with input and output in an Emacs buffer named *ocamldebug-FILE*.

A Yuareg Interactive Mode to evaluate expressions in a REPL (aka toplevel) is
included.  Type `M-x yuareg-run-ocaml' or simply `M-x run-ocaml' or see
special-keys below.

Short cuts for the Yuareg mode:
\\{yuareg-mode-map}

Short cuts for interactions with the REPL:
\\{yuareg-interactive-mode-map}

\(fn)" t nil)

(autoload 'yuareg-run-ocaml "yuareg" "\
Run an OCaml REPL process.  I/O via buffer `*OCaml*'.

\(fn)" t nil)

(defalias 'run-ocaml 'yuareg-run-ocaml)

(add-to-list 'interpreter-mode-alist '("ocamlrun" . yuareg-mode))

(add-to-list 'interpreter-mode-alist '("ocaml" . yuareg-mode))

;;;***

;;;### (autoloads nil "yuareg-jbuild" "yuareg-jbuild.el" (23482 1800
;;;;;;  978848 789000))
;;; Generated autoloads from yuareg-jbuild.el

(autoload 'yuareg-jbuild-mode "yuareg-jbuild" "\
Major mode to edit jbuild files.

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\(?:\\`\\|/\\)jbuild\\(?:\\.inc\\)?\\'" . yuareg-jbuild-mode))

;;;***

;;;### (autoloads nil "yuareg-menhir" "yuareg-menhir.el" (23482 1442
;;;;;;  89890 306000))
;;; Generated autoloads from yuareg-menhir.el

(add-to-list 'auto-mode-alist '("\\.mly\\'" . yuareg-menhir-mode))

(autoload 'yuareg-menhir-mode "yuareg-menhir" "\
Major mode to edit Menhir (and Ocamlyacc) files.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "yuareg-opam" "yuareg-opam.el" (23482 1800
;;;;;;  978848 789000))
;;; Generated autoloads from yuareg-opam.el

(autoload 'yuareg-opam-mode "yuareg-opam" "\
Major mode to edit opam files.

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("[./]opam_?\\'" . yuareg-opam-mode))

;;;***

