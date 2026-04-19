# This is not a full-featured Makefile and it is not intended to be used
# to install aurel package to your system.  Its only purpose is to
# byte-compile "aurel.el" (using "make") to make sure that there are no
# compilation warnings.  Moreover, it will not work on your system
# because of non-standard directories with dependencies.

EMACS = emacs

TOP := $(dir $(lastword $(MAKEFILE_LIST)))

BUI_DIR = $(HOME)/config/emacs/packages/bui
ELPA_DIR = $(HOME)/config/emacs/data/elpa

LOAD_PATH = -L $(TOP) -L $(BUI_DIR)
LOAD_PATH += $(shell \
  find $(ELPA_DIR) -mindepth 1 -maxdepth 1 -type d | \
  xargs -I === echo "-L ===")

EMACS_BATCH = $(EMACS) -batch -Q $(LOAD_PATH)

ELS =						\
  aurel.el

ELCS = $(ELS:.el=.elc)

all: $(ELCS)

%.elc: %.el
	@printf "Compiling $<\n"
	@$(EMACS_BATCH) -f batch-byte-compile $<

clean:
	@printf "Removing *.elc...\n"
	@$(RM) $(ELCS)
