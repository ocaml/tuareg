VERSION = $(shell grep ';; Version:' tuareg.el \
	| sed 's/;; Version: *\([0-9.]*\).*/\1/')
DESCRIPTION = $(shell grep ';;; tuareg.el ---' tuareg.el \
	| sed 's/[^-]*--- *\([^.]*\).*/\1/')
REQUIREMENTS = $(shell grep ';; Package-Requires:' tuareg.el \
	| sed 's/;; Package-Requires: *\(.*\)/\1/')
DIST_NAME = tuareg-$(VERSION)
TARBALL = $(DIST_NAME).tar.gz
OPAM_FILE = packages/tuareg/tuareg.$(VERSION)/opam

SOURCES = tuareg.el ocamldebug.el tuareg-opam.el \
  tuareg-menhir.el tuareg-compat.el
ELS = $(SOURCES) tuareg-site-file.el
ELC = $(ELS:.el=.elc)

INSTALL_FILES = $(ELS) $(ELC)
INSTALL_DIR ?= $(shell opam var share)/emacs/site-lisp

DIST_FILES += $(ELS) Makefile README.md tuareg.install

EMACSFORMACOSX = /Applications/Emacs.app/Contents/MacOS/Emacs
EMACSMACPORTS = /Applications/MacPorts/Emacs.app/Contents/MacOS/Emacs
AQUAMACS = /Applications/Aquamacs.app/Contents/MacOS/Aquamacs
ifeq ($(wildcard $(EMACSFORMACOSX)),$(EMACSFORMACOSX))
EMACS ?= $(EMACSFORMACOSX)
else
ifeq ($(wildcard $(EMACSMACPORTS)),$(EMACSMACPORTS))
EMACS ?= $(EMACSMACPORTS)
else
ifeq ($(wildcard $(AQUAMACS)),$(AQUAMACS))
EMACS ?= $(AQUAMACS)
endif
endif
endif
EMACS ?= emacs

RM ?= rm -f
CP ?= cp -f
LN = ln
DIFF = diff -u -B

INSTALL_RM_R = $(RM) -r
INSTALL_MKDIR = mkdir -p
INSTALL_CP = cp

all elc : $(ELC) tuareg-site-file.el

elc-werror: WERROR=--eval '(setq byte-compile-error-on-warn t)'
elc-werror: elc

%.elc : %.el
	$(EMACS) --batch -L . --no-init-file $(WERROR) -f batch-byte-compile $<

install : $(INSTALL_FILES)
	$(INSTALL_MKDIR) $(INSTALL_DIR)
	$(INSTALL_CP) $(INSTALL_FILES) $(INSTALL_DIR)/
	$(POST_INSTALL_HOOK)

uninstall :
	-test -d $(INSTALL_DIR) && \
	  $(INSTALL_RM_R) $(addprefix $(INSTALL_DIR)/, $(INSTALL_FILES))

.PHONY: refresh
refresh:

.PHONY: check
check:
	$(EMACS) -batch -Q -L . -l tuareg-tests -f ert-run-tests-batch-and-exit

%.test: % $(ELC) refresh
	@echo ====Indent $*====
	-$(RM) $@
	$(EMACS) --batch -q --no-site-file $(ENABLE_SMIE) \
	  --load tuareg-site-file.el $< \
	  --eval '(setq indent-tabs-mode nil)' \
	  --eval '(defun ask-user-about-lock (file opponent) nil)' \
	  --eval '(indent-region (point-min) (point-max) nil)' \
	  --eval '(indent-region (point-min) (point-max) nil)' \
	  --eval '(write-region (point-min) (point-max) "$@")'
	$(DIFF) $< $@ || true

indent-test: indent-test.ml.test

tuareg-site-file.el: $(SOURCES)
	(echo ";;; $@ --- Automatically extracted autoloads.  -*- lexical-binding: t; -*-";\
	 echo ";;; Code:";\
	 echo "(add-to-list 'load-path";\
	 echo "             (or (file-name-directory load-file-name) (car load-path)))";\
	 echo "") >$@
	$(EMACS) --batch --eval '(if (>= emacs-major-version 28) (make-directory-autoloads "." "'`pwd`'/$@") (setq generated-autoload-file "'`pwd`'/$@") (batch-update-autoloads))' "."

tuareg.install:
	echo "share_root: [" > $@
	for f in $(ELS); do \
	  echo "  \"$$f\" {\"emacs/site-lisp/$$f\"}" >> $@; \
	  echo "  \"?$${f}c\" {\"emacs/site-lisp/$${f}c\"}" >> $@; \
	done
	echo "]" >> $@

dist distrib: $(TARBALL)

$(TARBALL): $(DIST_FILES)
	mkdir -p $(DIST_NAME)
	for f in $(DIST_FILES); do $(LN) $$f $(DIST_NAME); done
	echo '(define-package "tuareg" "$(VERSION)" "$(DESCRIPTION)" ' "'"'$(REQUIREMENTS))' > $(DIST_NAME)/tuareg-pkg.el
	tar acvf $@ $(DIST_NAME)
	$(RM) -r $(DIST_NAME)

submit: $(TARBALL)
	@if [ ! -d packages/ ]; then \
	  echo "Make a symbolic link packages → OPAM repository/packages"; \
	  exit 1; \
	fi
	$(INSTALL_MKDIR) $(dir $(OPAM_FILE))
	$(CP) -a tuareg.opam $(OPAM_FILE)
	echo "url {" >> $(OPAM_FILE)
	echo "  src: \"https://github.com/ocaml/tuareg/releases/download/$(VERSION)/$(TARBALL)\"" >> $(OPAM_FILE)
	echo "  checksum: \"`md5sum $(TARBALL) | cut -d ' ' -f 1`\"" \
	  >> $(OPAM_FILE)
	echo "}" >> $(OPAM_FILE)

clean :
	$(RM) $(ELC) "$(DIST_NAME).tar.gz" "$(DIST_NAME).tar"
	$(RM) -r tuareg.$(VERSION)

.PHONY : all elc clean install uninstall check distrib dist submit
