SHELL   := /bin/bash
PKGDIR  := ./packages
RCPDIR  := ./recipes
HTMLDIR := ./html
WORKDIR := ./working
EMACS   := emacs

EVAL := $(EMACS)

ifdef STABLE
STABLE := t
PKGDIR := ./packages-stable
else
STABLE := nil
endif

## Check for needing to initialize CL-LIB from ELPA
NEED_CL-LIB := $(shell $(EMACS) --no-site-file --batch --eval '(prin1 (version< emacs-version "24.3"))')
ifeq ($(NEED_CL-LIB), t)
	EVAL := $(EVAL) --eval "(package-initialize)"
endif

EVAL := $(EVAL) --no-site-file --batch -l package-build.el --eval


all: build json index


## General rules
build:
	@echo " • Building $$(ls -1 $(RCPDIR) | wc -l) recipes ..."
	$(EVAL) "(let ((package-build-stable $(STABLE))) (package-build-all))"

html: index
index: archive.json recipes.json
	@echo " • Building html index ..."
	$(MAKE) -C $(HTMLDIR)


## Cleanup rules
clean-working:
	@echo " • Removing package sources ..."
	rm -rf $(WORKDIR)/*

clean-packages:
	@echo " • Removing packages ..."
	rm -rfv $(PKGDIR)/*

clean-json:
	@echo " • Removing json files ..."
	-rm -vf archive.json recipes.json

clean: clean-working clean-packages clean-json


## Json rules
archive.json: packages/archive-contents
	@echo " • Building $@ ..."
	$(EVAL) "(let ((package-build-stable $(STABLE))) (package-build-archive-alist-as-json \"archive.json\"))"

recipes.json: $(RCPDIR)/.dirstamp
	@echo " • Building $@ ..."
	$(EVAL) "(let ((package-build-stable $(STABLE))) (package-build-recipe-alist-as-json \"recipes.json\"))"

json: archive.json recipes.json

$(RCPDIR)/.dirstamp: .FORCE
	@[[ ! -e $@ || "$$(find $(@D) -newer $@ -print -quit)" != "" ]] \
	&& touch $@ || exit 0


## Recipe rules
$(RCPDIR)/%: .FORCE
	@echo " • Building recipe $(@F) ..."

	-rm -vf $(PKGDIR)/$(@F)-*
	$(EVAL) "(let ((package-build-stable $(STABLE))) (package-build-archive '$(@F)))"

	@echo " ✓ Wrote $$(ls -lsh $(PKGDIR)/$(@F)-*) "
	@echo


.PHONY: clean build index html json
.FORCE:
