[![NonGNU ELPA](https://elpa.nongnu.org/nongnu/tuareg.svg)](https://elpa.nongnu.org/nongnu/tuareg.html)
[![MELPA](https://melpa.org/packages/tuareg-badge.svg)](https://melpa.org/#/tuareg)
[![DebianBadge](https://badges.debian.net/badges/debian/stable/elpa-tuareg/version.svg)](https://packages.debian.org/stable/elpa-tuareg)
[![License GPL 3](https://img.shields.io/badge/license-GPL_3-green.svg)](COPYING)
[![Build Status](https://github.com/ocaml/tuareg/workflows/test/badge.svg)](https://github.com/ocaml/tuareg/actions?query=workflow%3Atest)

Tuareg: an Emacs OCaml mode
===========================

This archive contains files to help editing [OCaml](http://ocaml.org/)
code, to highlight important parts of the code, to run an OCaml
[REPL](https://en.wikipedia.org/wiki/Read%E2%80%93eval%E2%80%93print_loop)
(also called *toplevel*),
and to run the OCaml debugger within Emacs.

Package Contents
--------

- `README.md`      — This file.
- `HISTORY`        — Differences with previous versions.
- `tuareg.el`      — A major mode for editing OCaml code in Emacs.
- `ocamldebug.el`  — To run the OCaml debugger under Emacs.
- `sample.ml`      — Sample file to check the indentation engine.
- `compilation.txt` — To check the compilation regexp `tuareg--error-regexp`.

Install
-------

The easier way to install Tuareg is though
the [Emacs package system](https://www.gnu.org/software/emacs/manual/html_node/emacs/Packages.html)
with [NonGNU ELPA][] or
[MELPA][] ([configuration](https://melpa.org/#/getting-started)).

You can also install it using [OPAM][]:

    opam install tuareg

and follow the instructions given at the end of the `opam`
installation.

If you want to install from the Git checkout, just add to your
[Init File][] the line:

    (load "path-to-git-checkout-dir/tuareg-site-file")

If you want to byte compile the files, issue `make elc`.  If you do
this in Darwin, make sure that the version of Emacs displayed at the
end of `make elc` is the sole that you use (the `.elc` files may not
be compatible with other versions of Emacs installed on your system).


Usage & Configuration
---------------------

The Tuareg major mode is triggered by visiting a file with extension
`.ml`, `.mli`, and `.mlp` or manually by <kbd>M-x tuareg-mode</kbd>.
A [Menhir][] mode, `tuareg-menhir`, supports `.mly` files.  (A special
mode for `.mll` has yet to be written.)

For the convenience of users of [ocsigen][], the extensions
[`.eliom`](http://ocsigen.org/eliom/), `.eliomi` trigger `tuareg-mode`.

Start the OCaml REPL with <kbd>M-x run-ocaml</kbd>.
To evaluate a
phrase, simply type <kbd>S-⟨return⟩</kbd> (<kbd>shift</kbd> and
<kbd>return</kbd>).  You can also evaluate a
phrase in a different buffer by typing <kbd>C-c C-e</kbd> when the
cursor is on it (it
will start the OCaml REPL if needed).

Run the OCaml debugger with <kbd>M-x ocamldebug FILE</kbd>.


Tips & customization
--------------------

- You can comment/uncomment a single line with `tuareg-comment-dwim`
  which is bound to <kbd>C-cC-;</kbd>.

- By default, Tuareg will align the arguments of functions as follows:

        function_name arg1
          arg2

  This is what most OCaml programmers expect and is convenient if you
  use the following style:

        function_name (fun x ->
            do_something
          )
          arg2

  If you prefer the “lisp style” indentation in which arguments on the
  second line are aligned with the arguments on the first line as in

        function_name arg1
                      arg2

  put `(setq tuareg-indent-align-with-first-arg t)` in your [Init File][].

  In both cases, if there are no argument on the line following the
  function name, the indentation will be:

        function_name
          arg1
          arg2

- To make easier to distinguish pattern-match cases containing several
  patterns, sub-patterns are slightly indented as in

        match x with
        | A
          | B -> ...
        | C -> ...

  If you prefer all pipes to be aligned as

        match x with
        | A
        | B -> ...
        | C -> ...

  use `(setq tuareg-match-patterns-aligned t)`.

- Emacs ≥ 24.4 turned on [electric-indent-mode][] mode by default.  If
  you do not like it, call `(electric-indent-mode 0)` in
  `tuareg-mode-hook`.

  [electric-indent-mode]: https://www.gnu.org/software/emacs/manual/html_node/emacs/Indent-Convenience.html

- Tuareg respects you default commenting style.  However, in OCaml,
  commenting a region is usually done with a single multi-line comment
  and without leading stars on each line.  You can have that behavior
  in OCaml buffers by setting:

        (add-hook 'tuareg-mode-hook
                  (lambda()
                    (setq-local comment-style 'multi-line)
                    (setq-local comment-continue "   ")))

- If you turn on `show-paren-mode`, the delimiters of comments will
  also be highlighted.  If you do not like this behavior, set
  `tuareg-comment-show-paren` to `nil`.

- Syntax highlighting has 3 levels.  You can select the one you prefer
  by setting [font-lock-maximum-decoration][] from `0` to `2`.  By
  default, [font-lock-maximum-decoration][] is set to `t` which
  means that the maximum level of decoration will be used.

[font-lock-maximum-decoration]: https://www.gnu.org/software/emacs/manual/html_node/emacs/Font-Lock.html

- Fontifying all operators (as opposed to only non-standard ones) is a
  costly operation that slows down font-lock.  This is why it is
  disabled by default.  If you nonetheless want it, set
  `tuareg-highlight-all-operators` to `t` in your [Init File][]
  (before `tuareg-mode` is initialized; in particular, not in a hook
  added to `'tuareg-mode-hook`).

- You can turn on and off the rendering of certain sequences of
  characters as symbols (such as `∔` and `∧` instead of `+.`and `&&`),
  use `prettify-symbols-mode` or use the check box in the _Tuareg
  Options_ menu.  To enable it by default when you start Tuareg, add
  the following to your [Init File][]:

        (add-hook 'tuareg-mode-hook
                  (lambda()
                    (when (functionp 'prettify-symbols-mode)
                      (prettify-symbols-mode))))

  If you want more symbols to be prettified (such as `->` being
  displayed as `→`) at the expense of modifying the indentation in
  incompatible ways with those not using that option, add `(setq
  tuareg-prettify-symbols-full t)` to your [Init File][].

- By default, constructors are highlighted with the default face
  because having too many colors is distracting.  If you wish to
  customize the appearance of constructors, add to your [Init File][]
  the following code adapted to your tastes.

        (face-spec-set
         'tuareg-font-lock-constructor-face
         '((((class color) (background light)) (:foreground "SaddleBrown"))
           (((class color) (background dark)) (:foreground "burlywood1"))))

- To have a list of definitions in the buffer, use [imenu][].  It is
  available by right clicking in the buffer.  You can also launch the
  `speedbar` and click on file to have a list of definitions.

[imenu]: https://www.gnu.org/software/emacs/manual/html_node/emacs/Imenu.html

- If you wish to have a nice 🐫 as the mode name, add

        (add-hook 'tuareg-mode-hook
                  (lambda() (setq tuareg-mode-name "🐫")))

  to your [Init File][].

Thanks to the work of Stefan Monnier, a new indentation engine based on
[SMIE](https://www.gnu.org/software/emacs/manual/html_node/elisp/SMIE.html)
was written.  This changes the indentation somewhat w.r.t. the
previous versions of `tuareg`.  If the indentation does not correspond
to what you expect, please submit a
[motivated issue](https://github.com/ocaml/tuareg/issues/).


The standard Emacs customization tool can be used to configure Tuareg
options.  It is available from the Options menu and Tuareg's Customize
sub-menu.  Note that, at the moment, both customization options
pertaining to the SMIE indentation mode and the old one are present.

You may also customize the appearance of OCaml code by twiddling the
variables listed at the start of tuareg.el (preferably using
`tuareg-mode-hook`, you should not patch the file directly).
You should then add to your configuration file something like:

    (add-hook 'tuareg-mode-hook
      (lambda () ... ; your customization code ))

For example:

    (add-hook 'tuareg-mode-hook
              ;; Turn on auto-fill minor mode.
              #'auto-fill-mode)

See [dot-emacs.el](dot-emacs.el) for some examples.

[Init File]: https://www.gnu.org/software/emacs/manual/html_node/emacs/Init-File.html

Additional packages
-------------------

### Merlin

It is recommended to install [Merlin][] which is available in
[OPAM][].  Tuareg will automatically detect it and use some of its
features (e.g. for *imenu*).  Merlin offers auto-completion, the
possibility to query the type with <kbd>C-cC-t</kbd>, to find the
location of an identifier with <kbd>C-cC-l</kbd>, to go to the next
(resp. previous) phrase with <kbd>C-cC-n</kbd>
(resp. <kbd>C-cC-p</kbd>),...  Highly recommended.

### Caml mode

[caml-mode][] (available in [NonGNU ELPA][] and [MELPA][]) is used to
display types (using
the obsolete `*.annot` files), open a module for documentation,...

[Menhir]: http://gallium.inria.fr/~fpottier/menhir/
[ocsigen]: http://ocsigen.org/
[Merlin]: https://github.com/ocaml/merlin
[OPAM]: http://opam.ocaml.org/
[caml-mode]: https://github.com/ocaml/caml-mode
[NonGNU ELPA]: https://elpa.nongnu.org/
[MELPA]: https://melpa.org/


Reporting
---------

The official Tuareg home page is located at:
<https://github.com/ocaml/tuareg>.

Bug reports & patches: use the tracker:
<https://github.com/ocaml/tuareg/issues>.

Thanks
------

Ian Zimmerman for the previous mode, compilation interface and
debugger enhancement.

Jacques Garrigue enhanced Zimmerman's mode along with an adaptation
to OCaml (and Labl) syntax. Although this work was performed
independently, his useful test file and comments were of great help.

Michel Quercia for excellent suggestions, patches, and helpful
emacs-lisp contributions (full, ready-to-work implementations, I
should say), especially for Tuareg interactive mode, and browser
capacities.

Denis Barthou, Pierre Boulet, Jean-Christophe Filliatre and Rémi
Vanicat for intensive testing, useful suggestions, and help.

Ralf Treinen for maintaining the Debian GNU/Linux package.

Every people who sent me bug reports, suggestions, comments and
patches. Nothing would have improved since version 0.9.2 without
their help. Special thanks to Eli Barzilay, Josh Berdine, Christian
Boos, Carsten Clasohm, Yann Coscoy, Prakash Countcham, Alvarado
Cuihtlauac, Erwan David, Gilles Défourneaux, Philippe Esperet,
Gilles Falcon, Tim Freeman, Alain Frisch, Christian Lindig, Claude
Marché, Charles Martin, Dave Mason, Stefan Monnier, Toby Moth,
Jean-Yves Moyen, Alex Ott, Christopher Quinn, Ohad Rodeh, Rauli
Ruohonen, Hendrik Tews, Christophe Troestler, Joseph Sudish, Mattias
Waldau and John Whitley.

Tuareg mode have been maintained by Albert Cohen until version 1.45.

Jane Street took over maintenance based on Albert Cohen's version 1.46
(later retracted by him), and released its first version as 2.0.

License
-------

Tuareg is distributed under the GNU General Public License, version 3 or later.
