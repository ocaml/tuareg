2.2.0 2018-
-----------

* New mode `tuareg-menhir` thanks to Stefan Monnier.
  Note that <kbd>C-c C-c</kbd> launches the compilation.
* `tuareg-jbuilder`: <kbd>C-c C-c</kbd> launches the compilation.
* Be more subtle in phrase detection.
* Syntax highlighting improvements: faster; much better highlighting
  of function, class, and method arguments (including setting the
  `font-lock-multiline` property); `[]` and `::` have the constructor
  face; first class module, `type nonrec`, `raise_notrace`, `with
  type` are handled.  Finer highlighting of infix operators.  Support
  for [binding operators][].  Moreover, font-lock now has 3 possible
  levels of fontification (see the README).
* The switch .ml ↔ .mli now uses the Emacs built-in `find-file` and
  was extended to `.eliom` ↔ `.eliomi` and `.mly` ↔ `.mli`.
* Set `beginning-of-defun-function` and `end-of-defun-function` which
  allows to go to the beginning of the current function (resp. end)
  with <kbd>C-M-home</kbd>, <kbd>C-M-a</kbd> or <kbd>ESC
  <C-home></kbd> (resp. <kbd><C-M-end></kbd>, <kbd>C-M-e</kbd>, or
  <kbd>ESC <C-end></kbd>).
* `tuareg-comment-dwim` is now bound to <kbd>C-cC-;</kbd> (fixes #149).
* Rework electric functions (fixes issues #150 and #162).
* Update the compilation regexp to detect warnings and for the
  upcoming OCaml 4.08.  This also improves the highlighting of error
  messages.
* Evaluation of phrases: evaluate the above phrase if the point is in
  or after comments immediately following the let-binding (without
  separating blank lines).
* Use a pty to communicate with the `ocaml` process (fixes #83).
* Remove `tuareg-light`, you should now use `tuareg`.
* `tuareg-opam`: syntax highlighting updates.

Note that the mode `tuareg-dune` which was in the development version
of this package is now part of [Dune](https://github.com/ocaml/dune).

[binding operators]: https://caml.inria.fr/pub/docs/manual-ocaml-4.08/manual046.html

2.1.0 2017-11-10
----------------

* Let <kbd>M-q</kbd> reformat strings (and use only SMIE).
* Do not indent an expression after `;;` (issue #106).
* New face `tuareg-font-double-colon-face` to highlight `;;`.
* For `type … and …`, left-align `and` with `type`.
* Fix indentation of some GADT type definitions.
* Use `prettify-symbols-mode` to turn `+.` into `∔`,… and add a menu
  entry to toggle it.
* Properly indent `type 'a foo = 'a bla = …` (issue #98).
* Properly indent (issue #7):

        module … with module X = Z
                  and type t := C.t

* Support `let exception E in expr` (issue #102).
* Improved highlighting of `val` and `module` in first class module
  expressions.
* Warn if a file inside a `_build` is edited and propose to switch.
* Add a custom face `tuareg-font-lock-label-face` for labels.
* Add option `tuareg-match-patterns-aligned` to allow to choose
  between the two styles:

        function          v.s.        function
        | A                           | A
          | B -> ...                  | B -> ...
        | C -> ...                    | C -> ... "

* Highlight attributes and extension nodes.
* Disable by default and improve the compilation advice—see the new
  variable `tuareg-opam-insinuate` (issue #97).
* New keybinding <kbd>C-cC-w</kbd> and function `tuareg-opam-update-env`
  to update the environment to an opam switch (offering completion).
* Improved highlighting of quoted strings `{|…|}` (issue #89).
* Move after `;;` when evaluating a phrase in the toploop (issue #96).

* ocamldebug:
  - Add support for `completion-at-point`.
  - Highlight the right location even in presence of non-ascii chars
	(issue #80).
  - Make possible to pass argument to ocamldebug (say, paths with `-I`).
  - Make possible to pass argument to the program being debugged (issue #66).
  - Warn if SMIE is disabled.

* New modes `tuareg-jbuild` and `tuareg-opam` with syntax
  highlighting, indentation, and skeletons.

2.0.10
------

* New indentation config var for SMIE: tuareg-indent-align-with-first-arg.
* Many indentation improvements.
* Fixed point jumping in ocamldebug completion (by Darius Foo).
* Improved (var: t) syntax highlighting.
* Color all predefined exceptions with font-lock-builtin-face
* Syntax highlight cppo preprocessor directives.

2.0.9
-----

* Do not activate Tuareg for .mll and .mly files.
* Toplevel prompt is readonly.
* Font-lock code completely rewritten (avoids several hangs).  New faces
  `tuareg-font-lock-module-face', `tuareg-font-lock-constructor-face',
  and `tuareg-font-lock-line-number-face'.
* Non-closed comment does not cause M-q to hang.
* New variables `caml-types-build-dirs' and `caml-types-annot-dir' for
  a more versatile specification of .annot files.  (Submitted back to
  caml-mode.)
* Fix toplevel highlighting of output and errors.
