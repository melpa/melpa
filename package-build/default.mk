TOP := $(dir $(lastword $(MAKEFILE_LIST)))

PKG = package-build

ELS   = package-recipe.el
ELS  += package-build-badges.el
ELS  += package-build.el
ELS  += package-recipe-mode.el
ELCS  = $(ELS:.el=.elc)

DEPS  = compat

VERSION ?= $(shell test -e $(TOP).git && git describe --tags --abbrev=0 | cut -c2-)

EMACS      ?= emacs
EMACS_ARGS ?=

LOAD_PATH  ?= $(addprefix -L $(TOP)../,$(DEPS))
LOAD_PATH  += -L $(TOP)

BATCH       = $(EMACS) -Q --batch $(EMACS_ARGS) $(LOAD_PATH)
