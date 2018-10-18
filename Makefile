VERSION = $(shell grep ';; Version:' yuareg.el \
	| sed 's/;; Version: *\([0-9.]*\).*/\1/')
DESCRIPTION = $(shell grep ';;; yuareg.el ---' yuareg.el \
	| sed 's/[^-]*--- *\([^.]*\).*/\1/')
REQUIREMENTS = $(shell grep ';; Package-Requires:' yuareg.el \
	| sed 's/;; Package-Requires: *\(.*\)/\1/')
DIST_NAME = yuareg-$(VERSION)
TARBALL = $(DIST_NAME).tar.gz
OPAM_FILE = packages/yuareg/yuareg.$(VERSION)/opam

SOURCES = yuareg.el ocamldebug.el yuareg-opam.el yuareg-jbuild.el \
  yuareg-menhir.el
ELS = $(SOURCES) yuareg-site-file.el
ELC = $(ELS:.el=.elc)

TESTS = $(wildcard test/*.ml)
TEST_GENERATED = $(foreach x, $(TESTS), $(x).generated.test)

INSTALL_FILES = $(ELS) $(ELC)
DIST_FILES += $(ELS) Makefile README.md yuareg.install

EMACSFORMACOSX = /Applications/Emacs.app/Contents/MacOS/Emacs
EMACSMACPORTS = /Applications/MacPorts/Emacs.app/Contents/MacOS/Emacs
AQUAMACS = $(shell test -d /Applications \
	&& find /Applications -type f | grep 'Aquamacs$$')
ifeq ($(wildcard $(EMACSFORMACOSX)),$(EMACSFORMACOSX))
EMACS ?= $(EMACSFORMACOSX)
else
ifeq ($(wildcard $(EMACSMACPORTS)),$(EMACSMACPORTS))
EMACS ?= $(EMACSMACPORTS)
else
ifneq ($(strip $(AQUAMACS)),)
ifeq ($(wildcard $(AQUAMACS)),$(AQUAMACS))
EMACS ?= $(AQUAMACS)
endif
endif
endif
endif
EMACS ?= emacs

#ENABLE_SMIE = --eval '(setq yuareg-use-smie t)'
RM ?= rm -f
CP ?= cp -f
LN = ln
DIFF = diff -u -B

INSTALL_MKDIR = mkdir -p

all elc : $(ELC) yuareg-site-file.el

install : $(INSTALL_FILES)
	$(EMACS) --batch --eval "(package-initialize)" --eval "(package-install-file \"`pwd`\")"

uninstall :
	$(EMACS) --batch --script script/uninstall.el $(DIST_NAME)

.PHONY: refresh test
refresh:

check : sample.ml.test

test: indent-test

indent-test: $(TEST_GENERATED)

yuareg-site-file.el: $(SOURCES)
	(echo ";;; $@ --- Automatically extracted autoloads.";\
	 echo ";;; Code:";\
	 echo "(add-to-list 'load-path";\
	 echo "             (or (file-name-directory load-file-name) (car load-path)))";\
	 echo "") >$@
	$(EMACS) --batch --eval '(setq generated-autoload-file "'`pwd`'/$@")' -f batch-update-autoloads "."

dist distrib: $(TARBALL)

$(TARBALL): $(DIST_FILES)
	mkdir -p $(DIST_NAME)
	for f in $(DIST_FILES); do $(LN) $$f $(DIST_NAME); done
	echo '(define-package "yuareg" "$(VERSION)" "$(DESCRIPTION)" ' "'"'$(REQUIREMENTS))' > $(DIST_NAME)/yuareg-pkg.el
	tar acvf $@ $(DIST_NAME)
	$(RM) -r $(DIST_NAME)

submit: $(TARBALL)
	@if [ ! -d packages/ ]; then \
	  echo "Make a symbolic link packages → OPAM repository/packages"; \
	  exit 1; \
	fi
	$(INSTALL_MKDIR) $(dir $(OPAM_FILE))
	$(CP) -a yuareg.opam $(OPAM_FILE)
	echo "url {" >> $(OPAM_FILE)
	echo "  src: \"https://github.com/yutopp/yuareg/releases/download/$(VERSION)/$(TARBALL)\"" >> $(OPAM_FILE)
	echo "  checksum: \"`md5sum $(TARBALL) | cut -d ' ' -f 1`\"" \
	  >> $(OPAM_FILE)
	echo "}" >> $(OPAM_FILE)

clean :
	$(RM) $(ELC) "$(DIST_NAME).tar.gz" "$(DIST_NAME).tar"
	$(RM) -r yuareg.$(VERSION)
	$(RM) test/*.generated.test

.PHONY : all elc clean install uninstall check distrib dist submit

%.elc : %.el
	$(EMACS) --batch -L . --no-init-file -f batch-byte-compile $<
	@echo "Files byte-compiled using $(EMACS)"

%.generated.test: % $(ELC) refresh
	@echo ====Indent $*====
	touch $@
	$(EMACS) --batch -q --no-site-file $(ENABLE_SMIE) \
	  --load yuareg-site-file.el $< \
	  --eval '(setq indent-tabs-mode nil)' \
	  --eval '(defun ask-user-about-lock (file opponent) nil)' \
	  --eval '(indent-region (point-min) (point-max) nil)' \
	  --eval '(indent-region (point-min) (point-max) nil)' \
	  --eval '(write-region (point-min) (point-max) "$(notdir $@)")'
	$(DIFF) $< $@
