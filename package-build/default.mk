# LOAD_PATH has to work from "./" and "./test/".
TOP := $(dir $(realpath $(lastword $(MAKEFILE_LIST))))

PKG = package-build

ELS   = package-recipe.el
ELS  += package-build-badges.el
ELS  += package-build.el
ELS  += package-recipe-mode.el
ELCS  = $(ELS:.el=.elc)

DEPS  = compat

LOAD_PATH ?= $(addprefix -L $(TOP)../,$(DEPS))
LOAD_PATH += -L .

VERSION ?= $(shell test -e $(TOP).git && git describe --tags --abbrev=0 | cut -c2-)

EMACS       ?= emacs
EMACS_ARGS  ?=
EMACS_Q_ARG ?= -Q
EMACS_BATCH ?= $(EMACS) $(EMACS_Q_ARG) --batch $(EMACS_ARGS) $(LOAD_PATH)
