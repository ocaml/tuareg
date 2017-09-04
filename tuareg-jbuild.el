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

(defvar tuareg-jbuild-program
  (expand-file-name "jbuilder-lint" tuareg-jbuild-temporary-file-directory)
  "Script to use to check the jbuild file.")

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

(defun tuareg-jbuild-create-lint-script ()
  "Create the lint script if it does not exist.  This is nedded as long as See https://github.com/janestreet/jbuilder/issues/241 is not fixed."
  (unless (file-exists-p tuareg-jbuild-program)
    (let ((dir (file-name-directory tuareg-jbuild-program))
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
      (append-to-file pgm nil tuareg-jbuild-program)
      (set-file-modes tuareg-jbuild-program #o777)
      )))

(defun tuareg-jbuild--temp-name (absolute-path)
  "Full path of the copy of the filename in `tuareg-jbuild-temporary-file-directory'."
  (let ((slash-pos (string-match "/" absolute-path)))
    (file-truename (expand-file-name (substring absolute-path (1+ slash-pos))
                                     tuareg-jbuild-temporary-file-directory))))

(defun tuareg-jbuild-flymake-create-temp (filename _prefix)
  ;; based on `flymake-create-temp-with-folder-structure'.
  (unless (stringp filename)
    (error "Invalid filename"))
  (tuareg-jbuild--temp-name filename))

(defun tuareg-jbuild--opam-files (dir)
  "Return all opam files in the directory DIR."
  (let ((files nil))
    (dolist (f (directory-files-and-attributes dir t ".*\\.opam\\'"))
      (when (null (cadr f))
        (push (car f) files)))
    files))

(defun tuareg-jbuild--root (filename)
  "Return the root and copy the necessary context files for jbuilder."
  ;; FIXME: the root depends on jbuild-workspace.  If none is found,
  ;; assume the commands are issued from the dir where opam files are found.
  (let* ((dir (locate-dominating-file (file-name-directory filename)
                                     #'tuareg-jbuild--opam-files)))
    (when dir
      (setq dir (expand-file-name dir)); In case it is ~/...
      (make-directory (tuareg-jbuild--temp-name dir) t)
      (dolist (f (tuareg-jbuild--opam-files dir))
        (copy-file f (tuareg-jbuild--temp-name f) t)))
    dir))

(defun tuareg-jbuild--delete-opam-files (dir)
  "Delete all opam files in the directory DIR."
  (dolist (f (tuareg-jbuild--opam-files dir))
    (flymake-safe-delete-file f)))

(defun tuareg-jbuild-flymake-cleanup ()
  "Attempt to delete temp dir created by `tuareg-jbuild-flymake-create-temp', do not fail on error."
  (let ((dir (file-name-directory flymake-temp-source-file-name))
        (temp-dir (concat (directory-file-name
                           tuareg-jbuild-temporary-file-directory) "/")))
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
            (tuareg-jbuild--delete-opam-files dir)
            (delete-directory dir)
            (setq dir (file-name-directory (directory-file-name dir))))
        (error ; then top the loop
         (setq dir ""))))))

(defun tuareg-jbuild-flymake-init ()
  (tuareg-jbuild-create-lint-script)
  (let ((fname (flymake-init-create-temp-buffer-copy
                'tuareg-jbuild-flymake-create-temp))
        (root (or (tuareg-jbuild--root buffer-file-name) "")))
    (list tuareg-jbuild-program (list fname root))))

(defvar tuareg-jbuild--allowed-file-name-masks
  '("\\(?:\\`\\|/\\)jbuild\\'" tuareg-jbuild-flymake-init
                               tuareg-jbuild-flymake-cleanup)
  "Flymake entry for jbuild files.  See `flymake-allowed-file-name-masks'.")

(defvar tuareg-jbuild--err-line-patterns
  ;; Beware that the path from the root will be reported by jbuilder
  ;; but flymake requires it to match the file name.
  '(("File \"[^\"]*\\(jbuild\\)\", line \\([0-9]+\\), \
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
