;;; yuareg-jbuild.el --- Mode for editing jbuild files   -*- coding: utf-8 -*-

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

(defvar yuareg-jbuild-mode-hook nil
  "Hooks for the `yuareg-jbuild-mode'.")

(defvar yuareg-jbuild-flymake nil
  "If t, check your jbuild file with flymake.")

(defvar yuareg-jbuild-temporary-file-directory
  (expand-file-name "Tuareg-jbuild" temporary-file-directory)
  "Directory where to duplicate the files for flymake.")

(defvar yuareg-jbuild-program
  (expand-file-name "jbuild-lint" yuareg-jbuild-temporary-file-directory)
  "Script to use to check the jbuild file.")

(defgroup yuareg-jbuild nil
  "Support for Jbuilder files."
  :group 'languages)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                     Syntax highlighting

(defface yuareg-jbuild-error-face
  '((t (:foreground "yellow" :background "red" :bold t)))
  "Face for errors (e.g. obsolete constructs).")

(defvar yuareg-jbuild-error-face 'yuareg-jbuild-error-face
  "Face for errors (e.g. obsolete constructs).")

(defconst yuareg-jbuild-keywords-regex
  (eval-when-compile
    (concat (regexp-opt
             '("jbuild_version" "library" "executable" "executables" "rule"
               "ocamllex" "ocamlyacc" "menhir" "alias" "install"
               "copy_files" "copy_files#" "include"
               "documentation")
             ) "\\(?:\\_>\\|[[:space:]]\\)"))
  "Keywords in jbuild files.")

(defconst yuareg-jbuild-fields-regex
  (eval-when-compile
    (regexp-opt
     '("name" "public_name" "synopsis" "modules" "libraries" "wrapped"
       "inline_tests" "inline_tests.backend"
       "preprocess" "preprocessor_deps" "optional" "c_names" "cxx_names"
       "install_c_headers" "modes" "no_dynlink" "kind"
       "ppx_runtime_libraries" "virtual_deps" "js_of_ocaml" "flags"
       "ocamlc_flags" "ocamlopt_flags" "library_flags" "c_flags"
       "cxx_flags" "c_library_flags" "self_build_stubs_archive"
       "modules_without_implementation"
       ;; + for "executable" and "executables":
       "package" "link_flags" "modes" "names" "public_names"
       ;; + for "rule":
       "targets" "action" "deps" "mode"
       ;; + for "menhir":
       "merge_into"
       ;; + for "install"
       "section" "files" "lib" "libexec" "bin" "sbin" "toplevel" "share"
       "share_root" "etc" "doc" "stublibs" "man" "misc"
       ;; for "documentation":
       "mld_files")
     'symbols))
  "Field names allowed in jbuild files.")

(defvar yuareg-jbuild-builtin-regex
  (eval-when-compile
    (concat (regexp-opt
             '(;; Actions
               "run" "chdir" "setenv"
               "with-stdout-to" "with-stderr-to" "with-outputs-to"
               "ignore-stdout" "ignore-stderr" "ignore-outputs"
               "progn" "echo" "write-file" "cat" "copy" "copy#" "system"
               "bash" "diff" "diff?"
               ;; inline_tests and inline_tests.backend
               ;; FIXME: "flags" is already a field and we do not have enough
               ;; context to distinguishing both.
               "backend" "generate_runner" "runner_libraries" "flags"
               "extends"
               ;; Dependency specification
               "file" "alias" "alias_rec" "glob_files" "files_recursively_in"
               "universe" "package")
             t)
            "\\(?:\\_>\\|[[:space:]]\\)"))
  "Builtin sub-fields in jbuild")

(defvar yuareg-jbuild-var-kind-regex
  (eval-when-compile
    (regexp-opt
     '("path" "path-no-dep" "exe" "bin" "lib" "libexec" "lib-available"
       "version" "read" "read-lines" "read-strings")
     'words))
  "Optional prefix to variable names.")

(defvar yuareg-jbuild-var-regex
      (concat "\\(!?\\)\\(\\(?:" yuareg-jbuild-var-kind-regex
              ":\\)?\\)\\([a-zA-Z][a-zA-Z0-9_.-]*\\|[<@^]\\)"
              "\\(\\(?::[a-zA-Z][a-zA-Z0-9_.-]*\\)?\\)"))

(defmacro yuareg-jbuild--field-vals (field &rest vals)
  `(list (concat "(" ,field "[[:space:]]+" ,(regexp-opt vals t))
         1 font-lock-constant-face))

(defvar yuareg-jbuild-font-lock-keywords
  `((,yuareg-jbuild-keywords-regex . font-lock-keyword-face)
    (,(concat "(" yuareg-jbuild-fields-regex) 1 font-lock-function-name-face)
    ("\\(true\\|false\\)" 1 font-lock-constant-face)
    ("(\\(select\\)[[:space:]]+[^[:space:]]+[[:space:]]+\\(from\\)\\>"
     (1 font-lock-constant-face)
     (2 font-lock-constant-face))
    ,(eval-when-compile
       (yuareg-jbuild--field-vals "kind" "normal" "ppx_rewriter" "ppx_deriver"))
    ,(eval-when-compile
       (yuareg-jbuild--field-vals "mode" "standard" "fallback" "promote"
                                "promote-until-clean"))
    (,(concat "(" yuareg-jbuild-builtin-regex) 1 font-lock-builtin-face)
    ("(preprocess[[:space:]]+(\\(pps\\)" 1 font-lock-builtin-face)
    (,(eval-when-compile
        (concat "(" (regexp-opt '("fallback") t)))
     1 yuareg-jbuild-error-face)
    (,(concat "${" yuareg-jbuild-var-regex "}")
     (1 yuareg-jbuild-error-face)
     (2 font-lock-builtin-face)
     (4 font-lock-variable-name-face)
     (5 font-lock-variable-name-face))
    (,(concat "$(" yuareg-jbuild-var-regex ")")
     (1 yuareg-jbuild-error-face)
     (2 font-lock-builtin-face)
     (4 font-lock-variable-name-face)
     (5 font-lock-variable-name-face))
    ("\\(:[a-zA-Z]+\\)\\b" 1 font-lock-builtin-face)))

(defvar yuareg-jbuild-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\; "< b" table)
    (modify-syntax-entry ?\n "> b" table)
    (modify-syntax-entry ?\( "()" table)
    (modify-syntax-entry ?\) ")(" table)
    (modify-syntax-entry ?\{ "(}" table)
    (modify-syntax-entry ?\} "){" table)
    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\] ")[" table)
    table)
  "Tuareg-jbuild syntax table.")

;; (defun yuareg-jbuild-syntax-propertize (start end)
;;     (funcall
;;      (syntax-propertize-rules))
;; )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                             SMIE

(require 'smie)

(defvar yuareg-jbuild-smie-grammar
  (when (fboundp 'smie-prec2->grammar)
    (smie-prec2->grammar
     (smie-bnf->prec2 '()))))

(defun yuareg-jbuild-smie-rules (kind token)
  (cond
   ((eq kind :close-all) '(column . 0))
   ((and (eq kind :after) (equal token ")"))
    (save-excursion
      (goto-char (cadr (smie-indent--parent)))
      (if (looking-at-p yuareg-jbuild-keywords-regex)
          '(column . 0)
        1)))
   ((eq kind :before)
    (if (smie-rule-parent-p "(")
        (save-excursion
          (goto-char (cadr (smie-indent--parent)))
          (cond
           ((looking-at-p yuareg-jbuild-keywords-regex) 1)
           ((looking-at-p yuareg-jbuild-fields-regex)
            (smie-rule-parent 0))
           ((smie-rule-sibling-p) (cons 'column (current-column)))
           (t (cons 'column (current-column)))))
      '(column . 0)))
   (t 1)))

(defun verbose-yuareg-jbuild-smie-rules (kind token)
  (let ((value (yuareg-jbuild-smie-rules kind token)))
    (message
     "%s '%s'; sibling-p:%s parent:%s hanging:%s = %s"
     kind token
     (ignore-errors (smie-rule-sibling-p))
     (ignore-errors smie--parent)
     (ignore-errors (smie-rule-hanging-p))
     value)
    value))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                           Linting

(require 'flymake)

(defun yuareg-jbuild-create-lint-script ()
  "Create the lint script if it does not exist.  This is nedded as long as See https://github.com/ocaml/dune/issues/241 is not fixed."
  (unless (file-exists-p yuareg-jbuild-program)
    (let ((dir (file-name-directory yuareg-jbuild-program))
          (pgm "#!/usr/bin/env ocaml
;;
#load \"unix.cma\";;
#load \"str.cma\";;

open Printf

let filename = Sys.argv.(1)
let root = try Some(Sys.argv.(2)) with _ -> None

let read_all fh =
  let buf = Buffer.create 1024 in
  let b = Bytes.create 1024 in
  let len = ref 0 in
  while len := input fh b 0 1024; !len > 0 do
    Buffer.add_subbytes buf b 0 !len
  done;
  Buffer.contents buf

let errors =
  let root = match root with
    | None | Some \"\" -> \"\"
    | Some r -> \"--root=\" ^ Filename.quote r in
  let cmd = sprintf \"jbuilder external-lib-deps %s %s\" root
              (Filename.quote (Filename.basename filename)) in
  let env = Unix.environment() in
  let (_,_,fh) as p = Unix.open_process_full cmd env in
  let out = read_all fh in
  match Unix.close_process_full p with
  | Unix.WEXITED (0|1) ->
     (* jbuilder will normally exit with 1 as it will not be able to
        perform the requested action. *)
     out
  | Unix.WEXITED 127 -> printf \"jbuilder not found in path.\\n\"; exit 1
  | Unix.WEXITED n -> printf \"jbuilder exited with status %d.\\n\" n; exit 1
  | Unix.WSIGNALED n -> printf \"jbuilder was killed by signal %d.\\n\" n;
                        exit 1
  | Unix.WSTOPPED n -> printf \"jbuilder was stopped by signal %d\\n.\" n;
                       exit 1


let () =
  let re = \"\\\\(:?\\\\)[\\r\\n]+\\\\([a-zA-Z]+\\\\)\" in
  let errors = Str.global_substitute (Str.regexp re)
                 (fun s -> let colon = Str.matched_group 1 s = \":\" in
                           let f = Str.matched_group 2 s in
                           if f = \"File\" then \"\\n File\"
                           else if colon then \": \" ^ f
                           else \", \" ^ f)
                 errors in
  print_string errors"))
      (make-directory dir t)
      (append-to-file pgm nil yuareg-jbuild-program)
      (set-file-modes yuareg-jbuild-program #o777)
      )))

(defun yuareg-jbuild--temp-name (absolute-path)
  "Full path of the copy of the filename in `yuareg-jbuild-temporary-file-directory'."
  (let ((slash-pos (string-match "/" absolute-path)))
    (file-truename (expand-file-name (substring absolute-path (1+ slash-pos))
                                     yuareg-jbuild-temporary-file-directory))))

(defun yuareg-jbuild-flymake-create-temp (filename _prefix)
  ;; based on `flymake-create-temp-with-folder-structure'.
  (unless (stringp filename)
    (error "Invalid filename"))
  (yuareg-jbuild--temp-name filename))

(defun yuareg-jbuild--opam-files (dir)
  "Return all opam files in the directory DIR."
  (let ((files nil))
    (dolist (f (directory-files-and-attributes dir t ".*\\.opam\\'"))
      (when (null (cadr f))
        (push (car f) files)))
    files))

(defun yuareg-jbuild--root (filename)
  "Return the root and copy the necessary context files for jbuild."
  ;; FIXME: the root depends on jbuild-workspace.  If none is found,
  ;; assume the commands are issued from the dir where opam files are found.
  (let* ((dir (locate-dominating-file (file-name-directory filename)
                                     #'yuareg-jbuild--opam-files)))
    (when dir
      (setq dir (expand-file-name dir)); In case it is ~/...
      (make-directory (yuareg-jbuild--temp-name dir) t)
      (dolist (f (yuareg-jbuild--opam-files dir))
        (copy-file f (yuareg-jbuild--temp-name f) t)))
    dir))

(defun yuareg-jbuild--delete-opam-files (dir)
  "Delete all opam files in the directory DIR."
  (dolist (f (yuareg-jbuild--opam-files dir))
    (flymake-safe-delete-file f)))

(defun yuareg-jbuild-flymake-cleanup ()
  "Attempt to delete temp dir created by `yuareg-jbuild-flymake-create-temp', do not fail on error."
  (let ((dir (file-name-directory flymake-temp-source-file-name))
        (temp-dir (concat (directory-file-name
                           yuareg-jbuild-temporary-file-directory) "/")))
    (flymake-log 3 "Clean up %s" flymake-temp-source-file-name)
    (flymake-safe-delete-file flymake-temp-source-file-name)
    (condition-case nil
        (delete-directory (expand-file-name "_build" dir) t)
      (error nil))
    ;; Also delete parent dirs if empty or only contain opam files
    (while (and (not (string-equal dir temp-dir))
                (> (length dir) 0))
      (condition-case nil
          (progn
            (yuareg-jbuild--delete-opam-files dir)
            (delete-directory dir)
            (setq dir (file-name-directory (directory-file-name dir))))
        (error ; then top the loop
         (setq dir ""))))))

(defun yuareg-jbuild-flymake-init ()
  (yuareg-jbuild-create-lint-script)
  (let ((fname (flymake-init-create-temp-buffer-copy
                'yuareg-jbuild-flymake-create-temp))
        (root (or (yuareg-jbuild--root buffer-file-name) "")))
    (list yuareg-jbuild-program (list fname root))))

(defvar yuareg-jbuild--allowed-file-name-masks
  '("\\(?:\\`\\|/\\)jbuild\\'" yuareg-jbuild-flymake-init
                               yuareg-jbuild-flymake-cleanup)
  "Flymake entry for jbuild files.  See `flymake-allowed-file-name-masks'.")

(defvar yuareg-jbuild--err-line-patterns
  ;; Beware that the path from the root will be reported by jbuild
  ;; but flymake requires it to match the file name.
  '(("File \"[^\"]*\\(jbuild\\)\", line \\([0-9]+\\), \
characters \\([0-9]+\\)-\\([0-9]+\\): +\\([^\n]*\\)$"
     1 2 3 5))
  "Value of `flymake-err-line-patterns' for jbuild files.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                          Skeletons
;; See Info node "Autotype".

(define-skeleton yuareg-jbuild-insert-version-form
  "Insert the jbuild version."
  nil
  "(jbuild_version 1" _ ")" > ?\n)

(define-skeleton yuareg-jbuild-insert-library-form
  "Insert a library stanza."
  nil
  "(library" > \n
  "((name        " _ ")" > \n
  "(public_name " _ ")" > \n
  "(libraries  (" _ "))" > \n
  "(synopsis \"" _ "\")))" > ?\n)

(define-skeleton yuareg-jbuild-insert-executable-form
  "Insert an executable stanza."
  nil
  "(executable" > \n
  "((name        " _ ")" > \n
  "(public_name " _ ")" > \n
  "(modules    (" _ "))" > \n
  "(libraries  (" _ "))))" > ?\n)

(define-skeleton yuareg-jbuild-insert-executables-form
  "Insert an executables stanza."
  nil
  "(executables" > \n
  "((names        (" _ "))" > \n
  "(public_names (" _ "))" > \n
  "(libraries    (" _ "))))" > ?\n)

(define-skeleton yuareg-jbuild-insert-rule-form
  "Insert a rule stanza."
  nil
  "(rule" > \n
  "((targets (" _ "))" > \n
  "(deps    (" _ "))" > \n
  "(action  (" _ "))))" > ?\n)

(define-skeleton yuareg-jbuild-insert-ocamllex-form
  "Insert an ocamllex stanza."
  nil
  "(ocamllex (" _ "))" > ?\n)

(define-skeleton yuareg-jbuild-insert-ocamlyacc-form
  "Insert an ocamlyacc stanza."
  nil
  "(ocamlyacc (" _ "))" > ?\n)

(define-skeleton yuareg-jbuild-insert-menhir-form
  "Insert a menhir stanza."
  nil
  "(menhir" > \n
  "((modules (" _ "))))" > ?\n)

(define-skeleton yuareg-jbuild-insert-alias-form
  "Insert an alias stanza."
  nil
  "(alias" > \n
  "((name " _ ")" > \n
  "(deps (" _ "))))" > ?\n)

(define-skeleton yuareg-jbuild-insert-install-form
  "Insert an install stanza."
  nil
  "(install" > \n
  "((section " _ ")" > \n
  "(files (" _ "))))" > ?\n)

(define-skeleton yuareg-jbuild-insert-copyfiles-form
  "Insert a copy_files stanza."
  nil
  "(copy_files " _ ")" > ?\n)

(define-skeleton yuareg-jbuild-insert-documentation-form
  "Insert a documentation stanza."
  nil
  "(documentation" > \n
  "((package" _ ")" > \n
  "(mld_files :standard)))" > ?\n)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar yuareg-jbuild-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-c" 'compile)
    (define-key map "\C-c.v" 'yuareg-jbuild-insert-version-form)
    (define-key map "\C-c.l" 'yuareg-jbuild-insert-library-form)
    (define-key map "\C-c.e" 'yuareg-jbuild-insert-executable-form)
    (define-key map "\C-c.x" 'yuareg-jbuild-insert-executables-form)
    (define-key map "\C-c.r" 'yuareg-jbuild-insert-rule-form)
    (define-key map "\C-c.p" 'yuareg-jbuild-insert-ocamllex-form)
    (define-key map "\C-c.y" 'yuareg-jbuild-insert-ocamlyacc-form)
    (define-key map "\C-c.m" 'yuareg-jbuild-insert-menhir-form)
    (define-key map "\C-c.a" 'yuareg-jbuild-insert-alias-form)
    (define-key map "\C-c.i" 'yuareg-jbuild-insert-install-form)
    (define-key map "\C-c.c" 'yuareg-jbuild-insert-copyfiles-form)
    (define-key map "\C-c.d" 'yuareg-jbuild-insert-documentation-form)
    map)
  "Keymap used in Tuareg-jbuild mode.")

(defun yuareg-jbuild-build-menu ()
  (easy-menu-define
    yuareg-jbuild-mode-menu  (list yuareg-jbuild-mode-map)
    "Tuareg-jbuild mode menu."
    '("Jbuild"
      ("Stanzas"
       ["version" yuareg-jbuild-insert-version-form t]
       ["library" yuareg-jbuild-insert-library-form t]
       ["executable" yuareg-jbuild-insert-executable-form t]
       ["executables" yuareg-jbuild-insert-executables-form t]
       ["rule" yuareg-jbuild-insert-rule-form t]
       ["ocamllex" yuareg-jbuild-insert-ocamllex-form t]
       ["ocamlyacc" yuareg-jbuild-insert-ocamlyacc-form t]
       ["menhir" yuareg-jbuild-insert-menhir-form t]
       ["alias" yuareg-jbuild-insert-alias-form t]
       ["install" yuareg-jbuild-insert-install-form t]
       ["copy_files" yuareg-jbuild-insert-copyfiles-form t]
       )))
  (easy-menu-add yuareg-jbuild-mode-menu))


;;;###autoload
(define-derived-mode yuareg-jbuild-mode prog-mode "Tuareg-jbuild"
  "Major mode to edit jbuild files."
  (setq-local font-lock-defaults '(yuareg-jbuild-font-lock-keywords))
  (setq-local comment-start ";")
  (setq-local comment-end "")
  (setq indent-tabs-mode nil)
  ;(setq-local syntax-propertize-function #'yuareg-jbuild-syntax-propertize)
  (setq-local require-final-newline mode-require-final-newline)
  (push yuareg-jbuild--allowed-file-name-masks flymake-allowed-file-name-masks)
  (smie-setup yuareg-jbuild-smie-grammar #'yuareg-jbuild-smie-rules)
  (setq-local flymake-err-line-patterns yuareg-jbuild--err-line-patterns)
  (when (and yuareg-jbuild-flymake buffer-file-name)
    (flymake-mode t))
  (yuareg-jbuild-build-menu)
  (run-mode-hooks 'yuareg-jbuild-mode-hook))


;;;###autoload
(add-to-list 'auto-mode-alist
             '("\\(?:\\`\\|/\\)jbuild\\(?:\\.inc\\)?\\'" . yuareg-jbuild-mode))


(provide 'yuareg-jbuild-mode)
