EMACS = emacs
NOINIT = -q --no-site-file
# when testing, we want all settings to be non-0
TEST_INIT = -eval '(setq tuareg-in-indent 2)'
BATCH = -batch $(NOINIT) --load tuareg.elc
RM = rm -rf
CP = cp -f
LN = ln
CMP = cmp
# CMP = diff -u
DIFF_W = diff -uw
DEST = /usr/share/emacs/site-lisp/tuareg

DIST_FILES = COPYING HISTORY README sample.ml
ELS = camldebug.el tuareg.el
ELC = $(ELS:.el=.elc)

INSTALL_RM_R = $(RM)
INSTALL_MKDIR = mkdir
INSTALL_CP = $(CP)

all : elc

elc : $(ELC)

%.elc : %.el
        $(EMACS) -batch $(NOINIT) -f batch-byte-compile $<

camldebug.elc : camldebug.el tuareg.elc

VERSION_FILE = version

ifneq ($(realpath .hg),)
POST_INSTALL_HOOK = $(RM) $(VERSION_FILE)
MAKE_VERSION_FILE = hg id -i | fgrep -v '+' >/dev/null || \
        (echo 'uncommitted changes' >&2; exit 1); \
        hg id -i --debug > $(VERSION_FILE)
else
ifneq ($(realpath .svn),)
POST_INSTALL_HOOK = $(RM) $(VERSION_FILE)
MAKE_VERSION_FILE = svn info | grep Revision: | sed 's/Revision: //' > $(VERSION_FILE)
else
ifneq ($(realpath .bzr),)
POST_INSTALL_HOOK = $(RM) $(VERSION_FILE)
MAKE_VERSION_FILE = bzr log -l -1 | grep revno: > $(VERSION_FILE)
else
ifneq ($(realpath $(VERSION_FILE)),)
POST_INSTALL_HOOK =
MAKE_VERSION_FILE = @echo "Using \"$(VERSION_FILE)\" in the distribution."
else
POST_INSTALL_HOOK =
MAKE_VERSION_FILE = @(echo "missing \"$(VERSION_FILE)\" in the distribution?" >&2; exit 1)
endif
endif
endif
endif

$(VERSION_FILE) : force
        $(MAKE_VERSION_FILE)

install : $(ELC) $(VERSION_FILE)
        fgrep `cat $(VERSION_FILE)` tuareg.elc >/dev/null 2>&1 || \
         ($(RM) tuareg.elc; $(MAKE) tuareg.elc)
        $(INSTALL_RM_R) ${DEST}
        $(INSTALL_MKDIR) ${DEST}
        for f in $(ELS) $(ELC) $(VERSION_FILE); do $(INSTALL_CP) $$f $(DEST)/$$f; done
        $(POST_INSTALL_HOOK)

# have to indent twice because comments are indented to the _following_ code
REINDENT = --file test.ml --eval '(with-current-buffer "test.ml" (tuareg-mode) (indent-region (point-min) (point-max)) (indent-region (point-min) (point-max)) (save-buffer))' --kill

MANGLE = sed -e 's/^\(  *[a-z].*[^\"]\)$$/ \1/'

EXTRA_CHECK_COMMANDS =

check : $(ELC) sample.ml
        @echo ====sample.ml====
        $(MANGLE) sample.ml > test.ml
        $(EMACS) $(BATCH) $(TEST_INIT) $(REINDENT)
        $(CMP) sample.ml test.ml
        $(EXTRA_CHECK_COMMANDS)
        $(RM) test.ml test.ml~

DIST_NAME = tuareg-$(shell grep 'Tuareg Version' tuareg.el | sed 's/.*Tuareg Version \([^ ]*\) .*/\1/')
DIST_FILES += $(ELS) $(VERSION_FILE) Makefile
$(DIST_NAME).tgz $(DIST_NAME).zip : $(DIST_FILES)
        $(RM) $(DIST_NAME) $(DIST_NAME).tgz $(DIST_NAME).zip; mkdir $(DIST_NAME)
        for f in $(DIST_FILES); do $(LN) $$f $(DIST_NAME); done
        tar cvfz $(DIST_NAME).tgz $(DIST_NAME)
        zip -9vr $(DIST_NAME).zip $(DIST_NAME)
        $(RM) $(DIST_NAME)
        $(POST_INSTALL_HOOK)

distrib : $(DIST_NAME).tgz
dist: distrib

clean :
        $(RM) $(ELC) test.ml test.ml~ $(DIST_NAME).tgz $(DIST_NAME).zip
        $(POST_INSTALL_HOOK)

.PHONY : all elc clean install force check distrib dist
