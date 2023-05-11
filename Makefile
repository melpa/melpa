## Settings

TOP := $(abspath $(dir $(lastword $(MAKEFILE_LIST))))

-include ./config.mk

SHELL         := bash
EMACS_COMMAND ?= emacs

PKGDIR  := packages
RCPDIR  := recipes
HTMLDIR := html
WORKDIR := working
SLEEP   ?= 0
SANDBOX := sandbox
STABLE  ?= nil
ifneq ($(STABLE), nil)
PKGDIR  := packages-stable
HTMLDIR := html-stable
endif

LISP_CONFIG ?= '(progn\
  (setq package-build--melpa-base "$(TOP)/")\
  (setq package-build-working-dir "$(TOP)/$(WORKDIR)/")\
  (setq package-build-archive-dir "$(TOP)/$(PKGDIR)/")\
  (setq package-build-recipes-dir "$(TOP)/$(RCPDIR)/")\
  (setq package-build-stable $(STABLE))\
  (setq package-build-write-melpa-badge-images t)\
  (setq package-build-timeout-secs \
        (and (string= "linux" (symbol-name system-type)) 600)))'

LOAD_PATH ?= $(TOP)/package-build

EVAL := $(EMACS_COMMAND) --no-site-file --batch \
$(addprefix -L ,$(LOAD_PATH)) \
--eval $(LISP_CONFIG) \
--load package-build.el \
--eval

TIMEOUT := $(shell which timeout && echo "-k 60 600")

## General rules

.PHONY: clean build json html sandbox
.FORCE:

all: packages archive-contents json html

## Cleanup rules

clean-working:
	@echo " • Removing package sources ..."
	@git clean -dffX $(WORKDIR)/.

clean-packages:
	@echo " • Removing packages ..."
	@git clean -dffX $(PKGDIR)/.

clean-json:
	@echo " • Removing json files ..."
	@-rm -vf $(HTMLDIR)/archive.json $(HTMLDIR)/recipes.json

clean-sandbox:
	@echo " • Removing sandbox files ..."
	@if [ -d '$(SANDBOX)' ]; then \
	  rm -rfv '$(SANDBOX)/elpa'; \
	  rmdir '$(SANDBOX)'; \
	fi

clean: clean-working clean-packages clean-json clean-sandbox

packages: $(RCPDIR)/*

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

## Recipe rules

$(RCPDIR)/%: .FORCE
	@echo " • Building package $(@F) ..."
	@exec 2>&1; exec &> >(tee $(PKGDIR)/$(@F).log); \
	  $(TIMEOUT) $(EVAL) "(package-build-archive \"$(@F)\")" \
	  && echo " ✓ Success:" \
	  && ls -lsh $(PKGDIR)/$(@F)-[0-9]*
	@test $(SLEEP) -gt 0 && echo " Sleeping $(SLEEP) seconds ..." \
	  && sleep $(SLEEP) || true
	@echo

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
