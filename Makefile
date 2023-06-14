## Settings

TOP := $(abspath $(dir $(lastword $(MAKEFILE_LIST))))

-include ./config.mk

# Users should usually prefer this over other *_CONFIG variables.
# We recommend that the value is set in the included "config.mk".
USER_CONFIG ?= '()'

# Only intended for "docker/builder/run.sh" and similar scripts.
# That is also why we add extra quoting when setting EVAL below,
# instead of here.  Not doing it like that would complicate the
# quoting needed in scripts.
BUILD_CONFIG ?= ()

SLEEP ?= 0

SHELL         := bash
EMACS_COMMAND ?= emacs

PKGDIR  := packages
RCPDIR  := recipes
HTMLDIR := html
WORKDIR := working
SANDBOX := sandbox
STABLE  ?= nil
ifneq ($(STABLE), nil)
PKGDIR  := packages-stable
HTMLDIR := html-stable
endif

# You probably don't want to change this.
LOCATION_CONFIG ?= '(progn\
  (setq package-build--melpa-base "$(TOP)/")\
  (setq package-build-working-dir "$(TOP)/$(WORKDIR)/")\
  (setq package-build-archive-dir "$(TOP)/$(PKGDIR)/")\
  (setq package-build-recipes-dir "$(TOP)/$(RCPDIR)/"))'

LOAD_PATH ?= $(TOP)/package-build

EVAL := $(EMACS_COMMAND) --no-site-file --batch \
$(addprefix -L ,$(LOAD_PATH)) \
--eval $(LOCATION_CONFIG) \
--eval "$(BUILD_CONFIG)" \
--eval $(USER_CONFIG) \
--load package-build.el \
--eval

TIMEOUT := $(shell which timeout && echo "-k 60 600")

.PHONY: clean build json html sandbox
.FORCE:

all: build archive-contents json html

## Build

build: $(RCPDIR)/*

$(RCPDIR)/%: .FORCE
	@echo " • Building package $(@F) ..."
	@exec 2>&1; exec &> >(tee $(PKGDIR)/$(@F).log); \
	  $(TIMEOUT) $(EVAL) "(package-build-archive \"$(@F)\")" \
	  && echo " ✓ Success:" \
	  && ls -lsh $(PKGDIR)/$(@F)-[0-9]*
	@test $(SLEEP) -gt 0 && echo " Sleeping $(SLEEP) seconds ..." \
	  && sleep $(SLEEP) || true
	@echo

## Metadata

archive-contents: .FORCE
	@$(EVAL) '(package-build-dump-archive-contents)'

json: .FORCE
	@echo " • Building json indexes ..."
	@$(EVAL) '(package-build-archive-alist-as-json "$(HTMLDIR)/archive.json")'
	@$(EVAL) '(package-build-recipe-alist-as-json "$(HTMLDIR)/recipes.json")'

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
	@if [ -d '$(SANDBOX)' ]; then \
	  rm -rfv '$(SANDBOX)/elpa'; \
	  rmdir '$(SANDBOX)'; \
	fi

clean: .FORCE
	STABLE = nil make clean-packages clean-json clean-sandbox
	STABLE = t   make clean-packages clean-json clean-sandbox

## Update package-build

pull-package-build:
	git fetch package-build
	git -c "commit.gpgSign=true" subtree merge \
	-m "Merge Package-Build $$(git describe package-build/master)" \
	--squash -P package-build package-build/master

add-package-build-remote:
	git remote add package-build "git@github.com:melpa/package-build.git"

## Sandbox

sandbox: .FORCE
	@echo " • Building sandbox ..."
	@mkdir -p $(SANDBOX)
	@$(EMACS_COMMAND) -Q \
	  --eval '(package-build-dump-archive-contents)' \
	  --eval '(setq user-emacs-directory (file-truename "$(SANDBOX)"))' \
	  --eval '(setq package-user-dir (locate-user-emacs-file "elpa"))' \
	  -l package \
	  --eval "(add-to-list 'package-archives \
	            '(\"gnu\" . \"https://elpa.gnu.org/packages/\") t)" \
	  --eval "(add-to-list 'package-archives \
	            '(\"melpa\" . \"https://melpa.org/packages/\") t)" \
	  --eval "(add-to-list 'package-archives \
	            '(\"sandbox\" . \"$(TOP)/$(PKGDIR)/\") t)" \
	  --eval "(package-refresh-contents)" \
	  --eval "(package-initialize)" \
	  --eval '(setq sandbox-install-package "$(INSTALL)")' \
	  --eval "(unless (string= \"\" sandbox-install-package) \
	            (package-install (intern sandbox-install-package)))" \
	  --eval "(when (get-buffer \"*Compile-Log*\") \
	            (display-buffer \"*Compile-Log*\"))"

# Local Variables:
# outline-regexp: "#\\(#+\\)"
# eval: (outline-minor-mode)
# End:
