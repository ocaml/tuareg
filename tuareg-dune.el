;;; tuareg-dune.el --- Mode for editing dune files   -*- coding: utf-8 -*-

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

(defvar tuareg-dune-mode-hook nil
  "Hooks for the `tuareg-dune-mode'.")

(defvar tuareg-dune-flymake nil
  "If t, check your dune file with flymake.")

(defvar tuareg-dune-temporary-file-directory
  (expand-file-name "Tuareg-dune" temporary-file-directory)
  "Directory where to duplicate the files for flymake.")

(defvar tuareg-dune-program
  (expand-file-name "dune-lint" tuareg-dune-temporary-file-directory)
  "Script to use to check the dune file.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                     Syntax highlighting

(defface tuareg-dune-error-face
  '((t (:foreground "yellow" :background "red" :bold t)))
  "Face for errors (e.g. obsolete constructs).")

(defvar tuareg-dune-error-face 'tuareg-dune-error-face
  "Face for errors (e.g. obsolete constructs).")

(defconst tuareg-dune-keywords-regex
  (eval-when-compile
    (concat (regexp-opt
             '("jbuild_version" "library" "executable" "executables" "rule"
               "ocamllex" "ocamlyacc" "menhir" "alias" "install"
               "copy_files" "copy_files#" "include")
             ) "\\(?:\\_>\\|[[:space:]]\\)"))
  "Keywords in dune files.")

(defconst tuareg-dune-fields-regex
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
       "share_root" "etc" "doc" "stublibs" "man" "misc")
     'symbols))
  "Field names allowed in dune files.")

(defvar tuareg-dune-builtin-regex
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
               "extends")
             t)
            "\\(?:\\_>\\|[[:space:]]\\)"))
  "Builtin sub-fields in dune")

(defvar tuareg-dune-var-kind-regex
  (eval-when-compile
    (regexp-opt
     '("path" "path-no-dep" "exe" "bin" "lib" "libexec" "lib-available"
       "version" "read" "read-lines" "read-strings")
     'words))
  "Optional prefix to variable names.")

(defvar tuareg-dune-var-regex
      (concat "\\(!?\\)\\(\\(?:" tuareg-dune-var-kind-regex
              ":\\)?\\)\\([a-zA-Z][a-zA-Z0-9_.-]*\\|[<@^]\\)"
              "\\(\\(?::[a-zA-Z][a-zA-Z0-9_.-]*\\)?\\)"))

(defmacro tuareg-dune--field-vals (field &rest vals)
  `(list (concat "(" ,field "[[:space:]]+" ,(regexp-opt vals t))
         1 font-lock-constant-face))

(setq tuareg-dune-font-lock-keywords
  `((,tuareg-dune-keywords-regex . font-lock-keyword-face)
    (,(concat "(" tuareg-dune-fields-regex) 1 font-lock-function-name-face)
    ("\\(true\\|false\\)" 1 font-lock-constant-face)
    ("(\\(select\\)[[:space:]]+[^[:space:]]+[[:space:]]+\\(from\\)\\>"
     (1 font-lock-constant-face)
     (2 font-lock-constant-face))
    ,(eval-when-compile
       (tuareg-dune--field-vals "kind" "normal" "ppx_rewriter" "ppx_deriver"))
    ,(eval-when-compile
       (tuareg-dune--field-vals "mode" "standard" "fallback" "promote"
                                "promote-until-clean"))
    (,(concat "(" tuareg-dune-builtin-regex) 1 font-lock-builtin-face)
    ("(preprocess[[:space:]]+(\\(pps\\)" 1 font-lock-builtin-face)
    (,(eval-when-compile
        (concat "(" (regexp-opt '("fallback") t)))
     1 tuareg-dune-error-face)
    (,(concat "${" tuareg-dune-var-regex "}")
     (1 tuareg-dune-error-face)
     (2 font-lock-builtin-face)
     (4 font-lock-variable-name-face)
     (5 font-lock-variable-name-face))
    (,(concat "$(" tuareg-dune-var-regex ")")
     (1 tuareg-dune-error-face)
     (2 font-lock-builtin-face)
     (4 font-lock-variable-name-face)
     (5 font-lock-variable-name-face))
    ("\\(:[a-zA-Z]+\\)\\b" 1 font-lock-builtin-face)))

(defvar tuareg-dune-mode-syntax-table
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
  "Tuareg-dune syntax table.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                             SMIE

(require 'smie)

(defvar tuareg-dune-smie-grammar
  (when (fboundp 'smie-prec2->grammar)
    (smie-prec2->grammar
     (smie-bnf->prec2 '()))))

(defun tuareg-dune-smie-rules (kind token)
  (cond
   ((eq kind :close-all) '(column . 0))
   ((and (eq kind :after) (equal token ")"))
    (save-excursion
      (goto-char (cadr (smie-indent--parent)))
      (if (looking-at-p tuareg-dune-keywords-regex)
          '(column . 0)
        1)))
   ((eq kind :before)
    (if (smie-rule-parent-p "(")
        (save-excursion
          (goto-char (cadr (smie-indent--parent)))
          (cond
           ((looking-at-p tuareg-dune-keywords-regex) 1)
           ((looking-at-p tuareg-dune-fields-regex)
            (smie-rule-parent 0))
           ((smie-rule-sibling-p) (cons 'column (current-column)))
           (t (cons 'column (current-column)))))
      '(column . 0)))
   (t 1)))

(defun verbose-tuareg-dune-smie-rules (kind token)
  (let ((value (tuareg-dune-smie-rules kind token)))
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

(defun tuareg-dune-create-lint-script ()
  "Create the lint script if it does not exist.  This is nedded as long as See https://github.com/ocaml/dune/issues/241 is not fixed."
  (unless (file-exists-p tuareg-dune-program)
    (let ((dir (file-name-directory tuareg-dune-program))
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
      (append-to-file pgm nil tuareg-dune-program)
      (set-file-modes tuareg-dune-program #o777)
      )))

(defun tuareg-dune--temp-name (absolute-path)
  "Full path of the copy of the filename in `tuareg-dune-temporary-file-directory'."
  (let ((slash-pos (string-match "/" absolute-path)))
    (file-truename (expand-file-name (substring absolute-path (1+ slash-pos))
                                     tuareg-dune-temporary-file-directory))))

(defun tuareg-dune-flymake-create-temp (filename _prefix)
  ;; based on `flymake-create-temp-with-folder-structure'.
  (unless (stringp filename)
    (error "Invalid filename"))
  (tuareg-dune--temp-name filename))

(defun tuareg-dune--opam-files (dir)
  "Return all opam files in the directory DIR."
  (let ((files nil))
    (dolist (f (directory-files-and-attributes dir t ".*\\.opam\\'"))
      (when (null (cadr f))
        (push (car f) files)))
    files))

(defun tuareg-dune--root (filename)
  "Return the root and copy the necessary context files for dune."
  ;; FIXME: the root depends on jbuild-workspace.  If none is found,
  ;; assume the commands are issued from the dir where opam files are found.
  (let* ((dir (locate-dominating-file (file-name-directory filename)
                                     #'tuareg-dune--opam-files)))
    (when dir
      (setq dir (expand-file-name dir)); In case it is ~/...
      (make-directory (tuareg-dune--temp-name dir) t)
      (dolist (f (tuareg-dune--opam-files dir))
        (copy-file f (tuareg-dune--temp-name f) t)))
    dir))

(defun tuareg-dune--delete-opam-files (dir)
  "Delete all opam files in the directory DIR."
  (dolist (f (tuareg-dune--opam-files dir))
    (flymake-safe-delete-file f)))

(defun tuareg-dune-flymake-cleanup ()
  "Attempt to delete temp dir created by `tuareg-dune-flymake-create-temp', do not fail on error."
  (let ((dir (file-name-directory flymake-temp-source-file-name))
        (temp-dir (concat (directory-file-name
                           tuareg-dune-temporary-file-directory) "/")))
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
            (tuareg-dune--delete-opam-files dir)
            (delete-directory dir)
            (setq dir (file-name-directory (directory-file-name dir))))
        (error ; then top the loop
         (setq dir ""))))))

(defun tuareg-dune-flymake-init ()
  (tuareg-dune-create-lint-script)
  (let ((fname (flymake-init-create-temp-buffer-copy
                'tuareg-dune-flymake-create-temp))
        (root (or (tuareg-dune--root buffer-file-name) "")))
    (list tuareg-dune-program (list fname root))))

(defvar tuareg-dune--allowed-file-name-masks
  '("\\(?:\\`\\|/\\)jbuild\\'" tuareg-dune-flymake-init
                               tuareg-dune-flymake-cleanup)
  "Flymake entry for dune files.  See `flymake-allowed-file-name-masks'.")

(defvar tuareg-dune--err-line-patterns
  ;; Beware that the path from the root will be reported by dune
  ;; but flymake requires it to match the file name.
  '(("File \"[^\"]*\\(jbuild\\)\", line \\([0-9]+\\), \
characters \\([0-9]+\\)-\\([0-9]+\\): +\\([^\n]*\\)$"
     1 2 3 5))
  "Value of `flymake-err-line-patterns' for dune files.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                          Skeletons
;; See Info node "Autotype".

(define-skeleton tuareg-dune-insert-version-form
  "Insert the dune version."
  nil
  "(jbuild_version 1" _ ")" > ?\n)

(define-skeleton tuareg-dune-insert-library-form
  "Insert a library stanza."
  nil
  "(library" > \n
  "((name        " _ ")" > \n
  "(public_name " _ ")" > \n
  "(libraries  (" _ "))" > \n
  "(synopsis \"" _ "\")))" > ?\n)

(define-skeleton tuareg-dune-insert-executable-form
  "Insert an executable stanza."
  nil
  "(executable" > \n
  "((name        " _ ")" > \n
  "(public_name " _ ")" > \n
  "(modules    (" _ "))" > \n
  "(libraries  (" _ "))))" > ?\n)

(define-skeleton tuareg-dune-insert-executables-form
  "Insert an executables stanza."
  nil
  "(executables" > \n
  "((names        (" _ "))" > \n
  "(public_names (" _ "))" > \n
  "(libraries    (" _ "))))" > ?\n)

(define-skeleton tuareg-dune-insert-rule-form
  "Insert a rule stanza."
  nil
  "(rule" > \n
  "((targets (" _ "))" > \n
  "(deps    (" _ "))" > \n
  "(action  (" _ "))))" > ?\n)

(define-skeleton tuareg-dune-insert-ocamllex-form
  "Insert an ocamllex stanza."
  nil
  "(ocamllex (" _ "))" > ?\n)

(define-skeleton tuareg-dune-insert-ocamlyacc-form
  "Insert an ocamlyacc stanza."
  nil
  "(ocamlyacc (" _ "))" > ?\n)

(define-skeleton tuareg-dune-insert-menhir-form
  "Insert a menhir stanza."
  nil
  "(menhir" > \n
  "((modules (" _ "))))" > ?\n)

(define-skeleton tuareg-dune-insert-alias-form
  "Insert an alias stanza."
  nil
  "(alias" > \n
  "((name " _ ")" > \n
  "(deps (" _ "))))" > ?\n)

(define-skeleton tuareg-dune-insert-install-form
  "Insert an install stanza."
  nil
  "(install" > \n
  "((section " _ ")" > \n
  "(files (" _ "))))" > ?\n)

(define-skeleton tuareg-dune-insert-copyfiles-form
  "Insert a copy_files stanza."
  nil
  "(copy_files " _ ")" > ?\n)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar tuareg-dune-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-c" 'compile)
    (define-key map "\C-c.v" 'tuareg-dune-insert-version-form)
    (define-key map "\C-c.l" 'tuareg-dune-insert-library-form)
    (define-key map "\C-c.e" 'tuareg-dune-insert-executable-form)
    (define-key map "\C-c.x" 'tuareg-dune-insert-executables-form)
    (define-key map "\C-c.r" 'tuareg-dune-insert-rule-form)
    (define-key map "\C-c.p" 'tuareg-dune-insert-ocamllex-form)
    (define-key map "\C-c.y" 'tuareg-dune-insert-ocamlyacc-form)
    (define-key map "\C-c.m" 'tuareg-dune-insert-menhir-form)
    (define-key map "\C-c.a" 'tuareg-dune-insert-alias-form)
    (define-key map "\C-c.i" 'tuareg-dune-insert-install-form)
    (define-key map "\C-c.c" 'tuareg-dune-insert-copyfiles-form)
    map)
  "Keymap used in Tuareg-dune mode.")

(defun tuareg-dune-build-menu ()
  (easy-menu-define
    tuareg-dune-mode-menu  (list tuareg-dune-mode-map)
    "Tuareg-dune mode menu."
    '("Dune/jbuild"
      ("Stanzas"
       ["version" tuareg-dune-insert-version-form t]
       ["library" tuareg-dune-insert-library-form t]
       ["executable" tuareg-dune-insert-executable-form t]
       ["executables" tuareg-dune-insert-executables-form t]
       ["rule" tuareg-dune-insert-rule-form t]
       ["ocamllex" tuareg-dune-insert-ocamllex-form t]
       ["ocamlyacc" tuareg-dune-insert-ocamlyacc-form t]
       ["menhir" tuareg-dune-insert-menhir-form t]
       ["alias" tuareg-dune-insert-alias-form t]
       ["install" tuareg-dune-insert-install-form t]
       ["copy_files" tuareg-dune-insert-copyfiles-form t]
       )))
  (easy-menu-add tuareg-dune-mode-menu))


;;;###autoload
(define-derived-mode tuareg-dune-mode prog-mode "Tuareg-dune"
  "Major mode to edit dune files."
  (setq-local font-lock-defaults '(tuareg-dune-font-lock-keywords))
  (setq-local comment-start ";")
  (setq-local comment-end "")
  (setq indent-tabs-mode nil)
  (setq-local require-final-newline mode-require-final-newline)
  (push tuareg-dune--allowed-file-name-masks flymake-allowed-file-name-masks)
  (smie-setup tuareg-dune-smie-grammar #'tuareg-dune-smie-rules)
  (setq-local flymake-err-line-patterns tuareg-dune--err-line-patterns)
  (when (and tuareg-dune-flymake buffer-file-name)
    (flymake-mode t))
  (tuareg-dune-build-menu)
  (run-mode-hooks 'tuareg-dune-mode-hook))


;;;###autoload
(add-to-list 'auto-mode-alist
             '("\\(?:\\`\\|/\\)jbuild\\(?:\\.inc\\)?\\'" . tuareg-dune-mode))


(provide 'tuareg-dune-mode)
