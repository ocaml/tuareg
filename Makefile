EMACS = emacs
BATCH = -batch -q --no-site-file --load tuareg.elc
RM = rm -rf
CP = cp -f
LN = ln
CMP = cmp
# CMP = diff -u
DIFF_W = diff -uw
DEST = /usr/share/emacs/site-lisp/tuareg

DIST_FILES = COPYING HISTORY README sample.ml
ELR = append-tuareg camldebug custom-tuareg ocamlspot tuareg
ifeq ("`$(EMACS) --version |grep 'GNU Emacs'`", "")
ELR += sym-lock
else
DIST_FILES += sym-lock.el
endif
ELS = $(addsuffix .el, $(ELR))
ELC = $(addsuffix .elc, $(ELR))

INSTALL_RM_R = $(RM)
INSTALL_MKDIR = mkdir
INSTALL_CP = $(CP)

all : elc

elc : $(ELC)

%.elc : %.el
	$(EMACS) -batch -q --no-site-file -f batch-byte-compile $<

camldebug.elc : camldebug.el tuareg.elc

clean :
	$(RM) $(ELC) test.ml test.ml~ $(DIST_NAME).tgz $(DIST_NAME).zip

HG_VERSION_FILE = version
POST_INSTALL_HOOK =

install : $(ELC) $(HG_VERSION_FILE)
	fgrep `cat $(HG_VERSION_FILE)` tuareg.elc >/dev/null 2>&1 || \
	 ($(RM) tuareg.elc; $(MAKE) tuareg.elc)
	$(INSTALL_RM_R) ${DEST}
	$(INSTALL_MKDIR) ${DEST}
	for f in $(ELS) $(ELC) $(HG_VERSION_FILE); do $(INSTALL_CP) $$f $(DEST)/$$f; done
	$(POST_INSTALL_HOOK)

# have to indent twice because comments are indented to the _following_ code
REINDENT = --file test.ml --eval '(with-current-buffer "test.ml" (tuareg-mode) (indent-region (point-min) (point-max)) (indent-region (point-min) (point-max)) (save-buffer))' --kill

MANGLE = sed -e 's/^\(  *[a-z].*[^\"]\)$$/ \1/'

EXTRA_CHECK_COMMANDS =

check : $(ELC) sample.ml
	@echo ====sample.ml====
	$(MANGLE) sample.ml > test.ml
	$(EMACS) $(BATCH) $(REINDENT)
	$(CMP) sample.ml test.ml
	$(EXTRA_CHECK_COMMANDS)
	$(RM) test.ml test.ml~

DIST_NAME = tuareg-$(shell grep 'Tuareg Version' tuareg.el | sed 's/.*Tuareg Version \([^ ]*\) .*/\1/')
DIST_FILES += $(ELS) $(HG_VERSION_FILE) Makefile
$(DIST_NAME).tgz $(DIST_NAME).zip : $(DIST_FILES)
	$(RM) $(DIST_NAME) $(DIST_NAME).tgz $(DIST_NAME).zip; mkdir $(DIST_NAME)
	for f in $(DIST_FILES); do $(LN) $$f $(DIST_NAME); done
	tar cvfz $(DIST_NAME).tgz $(DIST_NAME)
	zip -9vr $(DIST_NAME).zip $(DIST_NAME)
	$(RM) $(DIST_NAME) $(HG_VERSION_FILE)

distrib : $(DIST_NAME).tgz

.PHONY : all elc clean install force check distrib
