SHELL   := /bin/bash
PKGDIR  := ./packages
HTMLDIR := ./html
WORKDIR := ./working

.PHONY: clean build index html
.FORCE:

all: build index

build:
	emacs --batch -l package-build.el --eval "(package-build-all)"

html:
	$(MAKE) -C $(HTMLDIR)

index: html

clean-working:
	rm -rf $(WORKDIR)/*

clean-packages:
	rm -rfv $(PKGDIR)/*

clean: clean-working clean-packages

recipes/%: .FORCE
	-rm -vf $(PKGDIR)/$(notdir $@)-*
	@echo
	./buildpkg $(notdir $@)
