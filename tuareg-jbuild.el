;;; tuareg-jbuild.el --- Mode for editing jbuild files   -*- coding: utf-8 -*-

;; Copyright (C) 2017- Christophe Troestler

;; This file is not part of GNU Emacs.

;; Permission to use, copy, modify, and distribute this software for
;; any purpose with or without fee is hereby granted, provided that
;; the above copyright notice and this permission notice appear in
;; all copies.
;;
;; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL
;; WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
;; WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE
;; AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR
;; CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
;; LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT,
;; NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
;; CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

(require 'scheme)

(defvar tuareg-jbuild-mode-hook nil
  "Hooks for the `tuareg-jbuild-mode'.")

(defvar tuareg-jbuild-flymake nil
  "If t, check your jbuild file with flymake.")

(defvar tuareg-jbuild-skeleton
  "(jbuild_version 1)\n
\(library
 ((name        )
  (public_name )
  (synopsis \"\")
  (libraries ())))

\(executables
 ((names        ())
  (public_names ())
  (libraries ())))\n"
  "If not nil, propose to fill new files with this skeleton")


(defvar tuareg-jbuild-temporary-file-directory
  (expand-file-name "Tuareg-jbuild" temporary-file-directory)
  "Directory where to duplicate the files for flymake.")

(defvar tuareg-jbuild-program "jbuilder-lint"
  "Script to use to check the jbuild file.")
;; See https://github.com/janestreet/jbuilder/issues/241

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                     Syntax highlighting

(defconst tuareg-jbuild-keywords-regex
  (regexp-opt
   '("jbuild_version" "library" "executable" "executables" "rule"
     "ocamllex" "ocamlyacc" "menhir" "alias" "install")
   'symbols)
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
     "share_root" "etc" "doc" "stublibs" "man" "misc")
   'symbols)
  "Field names allowed in jbuild files.")

(defvar tuareg-jbuild-font-lock-keywords
  `((,tuareg-jbuild-keywords-regex . font-lock-keyword-face)
    (,tuareg-jbuild-fields-regex . font-lock-constant-face)
    ("${\\([a-zA-Z:]+\\|[<@]\\)}" 1 font-lock-variable-name-face)
    ("$(\\([a-zA-Z:]+\\|[<@]\\))" 1 font-lock-variable-name-face)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                           Linting

(require 'flymake)

(defun tuareg-jbuild-flymake-create-temp (filename _prefix)
  ;; based on `flymake-create-temp-with-folder-structure'.
  ;; Should also copy <pkg>.opam files to provide context to jbuild.
  (unless (stringp filename)
    (error "Invalid filename"))
  (let* ((dir (file-name-directory filename))
         (slash-pos (string-match "/" dir))
         (temp-dir  (expand-file-name (substring dir (1+ slash-pos))
                                      tuareg-jbuild-temporary-file-directory)))
    (file-truename (expand-file-name (file-name-nondirectory filename)
                                     temp-dir))))

(defun tuareg-jbuild-flymake-cleanup ()
  "Attempt to delete temp dir created by `tuareg-jbuild-flymake-create-temp', do not fail on error."
  (let ((dir (file-name-directory flymake-temp-source-file-name)))
    (flymake-log 3 "Clean up %s" flymake-temp-source-file-name)
    (flymake-safe-delete-file flymake-temp-source-file-name)
    (condition-case nil
        (delete-directory (expand-file-name "_build" dir) t)
      (error
       (flymake-log 2 "Failed to delete dir %s, error ignored" dir-name)))
    (while (and (not (string-equal dir tuareg-jbuild-temporary-file-directory))
                (> (length dir) 0))
      (condition-case nil
          (progn
            (delete-directory dir)
            (setq dir (file-name-directory dir)))
        (error ; then top the loop
         (setq dir ""))))))

(defun tuareg-jbuild-flymake-init ()
  (let ((fname (flymake-init-create-temp-buffer-copy
                'tuareg-jbuild-flymake-create-temp)))
    (list tuareg-jbuild-program (list fname))))

(defvar tuareg-jbuild--allowed-file-name-masks
  '("\\(?:\\`\\|/\\)jbuild\\'" tuareg-jbuild-flymake-init
                               tuareg-jbuild-flymake-cleanup)
  "Flymake entry for jbuild files.  See `flymake-allowed-file-name-masks'.")

(defvar tuareg-jbuild--err-line-patterns
  '(("File \"\\([^\"]+\\)\", line \\([0-9]+\\), \
characters \\([0-9]+\\)-\\([0-9]+\\): +\\([^\n]*\\)$"
     1 2 3 5))
  "Value of `flymake-err-line-patterns' for jbuild files.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;###autoload
(define-derived-mode tuareg-jbuild-mode scheme-mode "Tuareg-jbuild"
  "Major mode to edit jbuild files."
  (setq-local font-lock-defaults '(tuareg-jbuild-font-lock-keywords))
  (setq indent-tabs-mode nil)
  (setq-local lisp-indent-offset 1)
  (setq-local require-final-newline mode-require-final-newline)
  (push tuareg-jbuild--allowed-file-name-masks flymake-allowed-file-name-masks)
  (setq-local flymake-err-line-patterns tuareg-jbuild--err-line-patterns)
  (when (and tuareg-jbuild-flymake buffer-file-name)
    (flymake-mode t))
  (let ((fname (buffer-file-name)))
    (when (and tuareg-jbuild-skeleton
               (not (and fname (file-exists-p fname)))
               (y-or-n-p "New file; fill with skeleton?"))
      (save-excursion (insert tuareg-jbuild-skeleton))))
  (run-mode-hooks 'tuareg-jbuild-mode-hook))


;;;###autoload
(add-to-list 'auto-mode-alist
             '("\\(?:\\`\\|/\\)jbuild\\'" . tuareg-jbuild-mode))


(provide 'tuareg-jbuild-mode)
