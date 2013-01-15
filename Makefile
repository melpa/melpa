SHELL   := /bin/bash
PKGDIR  := ./packages
RCPDIR  := ./recipes
HTMLDIR := ./html
WORKDIR := ./working
EMACS   := emacs

EVAL := $(EMACS) --no-site-file --batch -l package-build.el --eval


all: build json index


## General rules
build:
	@echo " • Building $$(ls -1 $(RCPDIR) | wc -l) recipes ..."
	$(EVAL) "(package-build-all)"

html: index
index: archive.json
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
	$(EVAL) '(package-build-archive-alist-as-json "archive.json")'

recipes.json: $(RCPDIR)/.dirstamp
	@echo " • Building $@ ..."
	$(EVAL) '(package-build-alist-as-json "recipes.json")'

json: archive.json recipes.json

$(RCPDIR)/.dirstamp: .FORCE
	@[[ ! -e $@ || "$$(find $(@D) -newer $@ -print -quit)" != "" ]] \
	&& touch $@ || exit 0


## Recipe rules
$(RCPDIR)/%: .FORCE
	@echo " • Building recipe $(@F) ..."
	-rm -vf $(PKGDIR)/$(@F)-*
	$(EVAL) "(package-build-archive '$(@F))"

	@echo " ✓ Wrote $$(ls -lsh $(PKGDIR)/$(@F)-*) "
	@echo


.PHONY: clean build index html json
.FORCE:
