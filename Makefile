SHELL   := /bin/bash
PKGDIR  := ./packages
HTMLDIR := ./html
WORKDIR := ./working

.PHONY: clean build index html json
.FORCE:

all: build json index

build:
	emacs --batch -l package-build.el --eval "(package-build-all)"

html:
	$(MAKE) -C $(HTMLDIR)

index: html

clean-working:
	rm -rf $(WORKDIR)/*

clean-packages:
	rm -rfv $(PKGDIR)/*

clean-json:
	-rm -vf archive.json recipes.json

clean: clean-working clean-packages clean-json

archive.json: packages/archive-contents
	emacs --batch --no-site-file -l package-build.el --eval \
		'(package-build-archive-alist-as-json "archive.json")'

recipes.json: recipes/.dirstamp
	emacs --batch --no-site-file -l package-build.el --eval \
  '(package-build-alist-as-json "recipes.json")'

recipes/.dirstamp: .FORCE
	@[[ ! -e $@ || "$$(find $(@D) -newer $@ -print -quit)" != "" ]] && touch $@ || exit 0

json: archive.json recipes.json

recipes/%: .FORCE
	-rm -vf $(PKGDIR)/$(notdir $@)-*
	@echo
	emacs --batch --no-site-file -l package-build.el --eval \
  "(package-build-archive '$(notdir $@))"
