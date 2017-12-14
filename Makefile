TOP := $(dir $(lastword $(MAKEFILE_LIST)))

SHELL   := bash
PKGDIR  := ./packages
RCPDIR  := ./recipes
HTMLDIR := ./html
WORKDIR := ./working
WEBROOT := $$HOME/www
EMACS_COMMAND   ?= emacs
SLEEP   ?= 0
SANDBOX := ./sandbox
STABLE ?= nil

EVAL := $(EMACS_COMMAND)

## Check for needing to initialize CL-LIB from ELPA
NEED_CL-LIB := $(shell $(EMACS_COMMAND) --no-site-file --batch --eval '(prin1 (version< emacs-version "24.3"))')
ifeq ($(NEED_CL-LIB), t)
	EMACS_COMMAND := $(EMACS_COMMAND) --eval "(package-initialize)"
endif

EVAL := $(EMACS_COMMAND) --no-site-file --batch -L $(TOP)/package-build -l package-build.el --eval

TIMEOUT := $(shell which timeout && echo "-k 60 600")

all: packages packages/archive-contents json index

## General rules
html: index
index: json
	@echo " • Building html index ..."
	$(MAKE) -C $(HTMLDIR)


## Cleanup rules
clean-working:
	@echo " • Removing package sources ..."
	git clean -dffX $(WORKDIR)/.

clean-packages:
	@echo " • Removing packages ..."
	git clean -dffX $(PKGDIR)/.

clean-json:
	@echo " • Removing json files ..."
	-rm -vf html/archive.json html/recipes.json

clean-sandbox:
	@echo " • Removing sandbox files ..."
	if [ -d '$(SANDBOX)' ]; then \
		rm -rfv '$(SANDBOX)/elpa'; \
		rmdir '$(SANDBOX)'; \
	fi

sync:
	rsync -avz --delete $(PKGDIR)/ $(WEBROOT)/packages
	rsync -avz --safe-links --delete $(HTMLDIR)/* $(WEBROOT)/
	chmod -R go+rx $(WEBROOT)/packages/*


pull-package-build:
	git subtree pull --squash -P package-build package-build master

clean: clean-working clean-packages clean-json clean-sandbox

packages: $(RCPDIR)/*

packages/archive-contents: .FORCE
	@echo " • Updating $@ ..."
	$(EVAL) '(package-build-dump-archive-contents)'

cleanup:
	$(EVAL) '(let ((package-build-stable $(STABLE)) (package-build-archive-dir (expand-file-name "$(PKGDIR)/" package-build--melpa-base))) (package-build-cleanup))'

## Json rules
html/archive.json: $(PKGDIR)/archive-contents
	@echo " • Building $@ ..."
	$(EVAL) '(let ((package-build-stable $(STABLE)) (package-build-archive-dir (expand-file-name "$(PKGDIR)/" package-build--melpa-base))) (package-build-archive-alist-as-json "html/archive.json"))'

html/recipes.json: $(RCPDIR)/.dirstamp
	@echo " • Building $@ ..."
	$(EVAL) '(let ((package-build-stable $(STABLE)) (package-build-archive-dir (expand-file-name "$(PKGDIR)/" package-build--melpa-base))) (package-build-recipe-alist-as-json "html/recipes.json"))'

json: html/archive.json html/recipes.json

$(RCPDIR)/.dirstamp: .FORCE
	@[[ ! -e $@ || "$$(find $(@D) -newer $@ -print -quit)" != "" ]] \
	&& touch $@ || exit 0


## Recipe rules
$(RCPDIR)/%: .FORCE
	@echo " • Building recipe $(@F) ..."

	- $(TIMEOUT) $(EVAL) "(let ((package-build-stable $(STABLE)) (package-build-write-melpa-badge-images t) (package-build-archive-dir (expand-file-name \"$(PKGDIR)\" package-build--melpa-base))) (package-build-archive \"$(@F)\"))"

	@echo " ✓ Wrote $$(ls -lsh $(PKGDIR)/$(@F)-*) "
	@echo " Sleeping for $(SLEEP) ..."
	sleep $(SLEEP)
	@echo


## Sandbox
sandbox: packages/archive-contents
	@echo " • Building sandbox ..."
	mkdir -p $(SANDBOX)
	$(EMACS_COMMAND) -Q \
		--eval '(setq user-emacs-directory "$(SANDBOX)")' \
		-l package \
		--eval "(add-to-list 'package-archives '(\"gnu\" . \"http://elpa.gnu.org/packages/\") t)" \
		--eval "(add-to-list 'package-archives '(\"melpa\" . \"https://melpa.org/packages/\") t)" \
		--eval "(add-to-list 'package-archives '(\"sandbox\" . \"$(shell pwd)/$(PKGDIR)/\") t)" \
		--eval "(package-refresh-contents)" \
		--eval "(package-initialize)" \
		--eval '(setq sandbox-install-package "$(INSTALL)")' \
		--eval "(unless (string= \"\" sandbox-install-package) (package-install (intern sandbox-install-package)))"

.PHONY: clean build index html json sandbox
.FORCE:
