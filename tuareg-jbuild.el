;;; tuareg-jbuild.el --- Mode for editing jbuild files   -*- coding: utf-8 -*-

;; Copyright (C) 2017- Christophe Troestler

(defvar tuareg-jbuild-mode-hook nil)

(defvar tuareg-jbuild-skeleton
  "(jbuild_version 1)\n
(library
  (name        )
  (public_name )
  (synopsis \"\")
  (libraries ()))

(executables
  (names        ())
  (public_names ())
  (libraries ()))\n"
  "If not nil, propose to fill new files with this skeleton")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                       Syntax highlighting

(defconst tuareg-jbuild-keywords-regex
  (regexp-opt
   '("jbuild_version" "library" "executable" "executables" "rule"
     "ocamllex" "ocamlyacc" "menhir" "alias" "install") 'symbols)
  "Keywords in jbuild files.")

(defconst tuareg-jbuild-fields-regex
  (regexp-opt
   '("name" "public_name" "synopsis" "modules" "libraries" "wrapped"
     "preprocess" "preprocessor_deps" "optional" "c_names" "cxx_names"
     "install_c_headers" "modes" "no_dynlink" "kind"
     "ppx_runtime_libraries" "virtual_deps" "js_of_ocaml" "flags"
     "ocamlc_flags" "ocamlopt_flags" "library_flags" "c_flags"
     "cxx_flags" "c_library_flags" "self_build_stubs_archive"
     ;; + for "executable" and "executables":
     "package" "link_flags" "modes" "names" "public_names"
     ;; + for "rule":
     "targets" "action" "deps" "fallback"
     ;; + for "menhir":
     "merge_into"
     ;; + for "install"
     "section" "files" "lib" "libexec" "bin" "sbin" "toplevel" "share"
     "share_root" "etc" "doc" "stublibs" "man" "misc") 'symbols)
  "Field names allowed in jbuild files.")

(defvar tuareg-jbuild-font-lock-keywords
  `((,tuareg-jbuild-keywords-regex . font-lock-keyword-face)
    (,tuareg-jbuild-fields-regex . font-lock-constant-face)
    ("${\\([a-zA-Z:]+\\|[<@]\\)}" 1 font-lock-variable-name-face)
    ("$(\\([a-zA-Z:]+\\|[<@]\\))" 1 font-lock-variable-name-face)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;###autoload
(define-derived-mode tuareg-jbuild-mode lisp-mode "Tuareg-jbuild"
  "Major mode to edit jbuild files."
  (setq font-lock-defaults '(tuareg-jbuild-font-lock-keywords))
  (setq indent-tabs-mode nil)
  (make-local-variable 'require-final-newline)
  (setq require-final-newline mode-require-final-newline)
  (let ((fname (buffer-file-name)))
    (when (and tuareg-jbuild-skeleton
               (not (and fname (file-exists-p fname)))
               (y-or-n-p "New file; fill with skeleton?"))
      (save-excursion (insert tuareg-jbuild-skeleton))))
  (run-mode-hooks 'tuareg-jbuild-mode-hook))


;;;###autoload
(add-to-list 'auto-mode-alist
             '("\\(?:\\'\\|/\\)jbuild\\'" . tuareg-jbuild-mode))


(provide 'tuareg-jbuild-mode)
