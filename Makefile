## Settings

TOP := $(abspath $(dir $(lastword $(MAKEFILE_LIST))))

-include ./config.mk

# Users should usually prefer this over other *_CONFIG variables.
# We recommend that the value is set in the included "config.mk".
USER_CONFIG ?= "()"

# Only intended for "docker/builder/run.sh" and similar scripts.
# That is also why we add extra quoting when setting EVAL below,
# instead of here.  Not doing it like that would complicate the
# quoting needed in scripts.
BUILD_CONFIG ?= ()

SLEEP ?= 0

SHELL := bash

ifdef EMACS_COMMAND
EMACS := $(EMACS_COMMAND)
else
EMACS ?= emacs
endif

RCPDIR  := recipes
WORKDIR := working
SANDBOX := sandbox

ifndef MELPA_CHANNEL
PKGDIR  := packages
HTMLDIR := html
CHANNEL_CONFIG := "()"

else ifeq ($(MELPA_CHANNEL), unstable)
PKGDIR  := packages
HTMLDIR := html
CHANNEL_CONFIG := "(progn\
  (setq package-build-stable nil)\
  (setq package-build-all-publishable t)\
  (setq package-build-snapshot-version-functions\
        '(package-build-timestamp-version))\
  (setq package-build-badge-data '(\"melpa\" \"\#922793\")))"

else ifeq ($(MELPA_CHANNEL), stable)
PKGDIR  := packages-stable
HTMLDIR := html-stable
CHANNEL_CONFIG := "(progn\
  (setq package-build-stable t)\
  (setq package-build-all-publishable nil)\
  (setq package-build-release-version-functions\
        '(package-build-tag-version))\
  (setq package-build-badge-data '(\"melpa stable\" \"\#3e999f\")))"

else
$(error Unknown MELPA_CHANNEL: $(MELPA_CHANNEL))
endif

# You probably don't want to change this.
LOCATION_CONFIG ?= "(progn\
  (setq package-build--melpa-base \"$(TOP)/\")\
  (setq package-build-working-dir \"$(TOP)/$(WORKDIR)/\")\
  (setq package-build-archive-dir \"$(TOP)/$(PKGDIR)/\")\
  (setq package-build-recipes-dir \"$(TOP)/$(RCPDIR)/\"))"

LOAD_PATH ?= $(TOP)/package-build

EVAL := $(EMACS) --no-site-file --batch \
$(addprefix -L ,$(LOAD_PATH)) \
--eval $(CHANNEL_CONFIG) \
--eval $(LOCATION_CONFIG) \
--eval "$(BUILD_CONFIG)" \
--eval $(USER_CONFIG) \
--load package-build.el \
--eval

TIMEOUT := $(shell which timeout && echo "-k 60 600")

.PHONY: clean build summarise json html sandbox
.FORCE:

all: build summarise

summarise: archive-contents json html

## Build

build: $(RCPDIR)/*

$(RCPDIR)/%: .FORCE
	@echo " • Building package $(@F) ..."
	@exec 2>&1; exec &> >(tee $(PKGDIR)/$(@F).log); \
	  $(TIMEOUT) $(EVAL) "(package-build-archive \"$(@F)\")"
	@test $(SLEEP) -gt 0 && echo " Sleeping $(SLEEP) seconds ..." \
	  && sleep $(SLEEP) || true
	@echo

## Metadata

archive-contents: .FORCE
	@$(EVAL) "(package-build-dump-archive-contents)"

json: .FORCE
	@echo " • Building json indexes ..."
	@$(EVAL) "(package-build-archive-alist-as-json \"$(HTMLDIR)/archive.json\")"
	@$(EVAL) "(package-build-recipe-alist-as-json \"$(HTMLDIR)/recipes.json\")"

html: json
	@echo " • Building html index ..."
	$(MAKE) -C $(HTMLDIR)

$(RCPDIR)/.dirstamp: .FORCE
	@[[ ! -e $@ || "$$(find $(@D) -newer $@ -print -quit)" != "" ]] \
	&& touch $@ || exit 0

## Cleanup rules

clean-working:
	@echo " • Removing package sources ..."
	@git clean -dffX $(WORKDIR)/.

clean-packages:
	@echo " • Removing $(PKGDIR)/* ..."
	@git clean -dffX $(PKGDIR)/.

clean-json:
	@echo " • Removing $(HTMLDIR)/*.json ..."
	@-rm -vf $(HTMLDIR)/archive.json $(HTMLDIR)/recipes.json

clean-sandbox:
	@echo " • Removing sandbox files ..."
	@if [ -d "$(SANDBOX)" ]; then \
	  rm -rfv "$(SANDBOX)/elpa"; \
	  rmdir "$(SANDBOX)"; \
	fi

clean: .FORCE
	MELPA_CHANNEL=unstable make clean-packages clean-json clean-sandbox
	MELPA_CHANNEL=stable   make clean-packages clean-json clean-sandbox

## Update package-build

PACKAGE_BUILD_REPO ?= "https://github.com/melpa/package-build"

pull-package-build:
	git fetch $(PACKAGE_BUILD_REPO)
	git -c "commit.gpgSign=true" subtree merge \
	-m "Merge Package-Build $$(git describe FETCH_HEAD)" \
	--squash -P package-build FETCH_HEAD

## Docker support

get-pkgdir: .FORCE
	@echo $(PKGDIR)

## Sandbox

sandbox: .FORCE
	@echo " • Building sandbox ..."
	@mkdir -p $(SANDBOX)
	@$(EVAL) "(progn\
  (package-build-dump-archive-contents)\
  (setq user-emacs-directory (file-truename \"$(SANDBOX)\"))\
  (setq package-user-dir (locate-user-emacs-file \"elpa\"))\
  (add-to-list 'package-archives '(\"gnu\" . \"https://elpa.gnu.org/packages/\") t)\
  (add-to-list 'package-archives '(\"melpa\" . \"https://melpa.org/packages/\") t)\
  (add-to-list 'package-archives '(\"sandbox\" . \"$(TOP)/$(PKGDIR)/\") t)\
  (package-refresh-contents)\
  (package-initialize)\
  (setq sandbox-install-package \"$(INSTALL)\")\
  (unless (equal sandbox-install-package \"\")\
    (package-install (intern sandbox-install-package)))\
  (when (get-buffer \"*Compile-Log*\")\
    (display-buffer \"*Compile-Log*\")))"

# Local Variables:
# outline-regexp: "#\\(#+\\)"
# eval: (outline-minor-mode)
# End:
