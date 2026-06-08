## Help

help helpall::
	$(info )
	$(info Getting Help)
	$(info ============)
	$(info make help                                 Show brief help)
	$(info make helpall                              Show extended help)
	$(info )
	$(info Building)
	$(info ========)
	$(info )
	$(info make recipes/<package>                    Build <package>)
	$(info make build-channel                        Build "$(CHANNEL)" channel)
	$(info make build-channels                       Build all channels)
	$(info make container-build                      Build all channels in container)
	$(info make container-image                      Build container image)
	$(info make CHANNEL=<channel> recipes/<package>  Build <package> on <channel>)
	$(info make CHANNEL=<channel> build-channel      Build <channel>)
	$(info )
	$(info make BUILD_PACKAGES="<p1> <p2>..." ...    Limit to <p1>, <p2> ...)
	$(info make ASYNC=t ...                          Build asynchronously)
	$(info make V=t ...                              Show commands run by make)
	$(info make NOFETCH=t ...                        Build without fetching)
	$(info make [ASYNC=t] fetch                      Fetch without building)
helpall::
	$(info make PACKAGE_BUILD_DIRECTORY=<dir> ...    Use package-build.el from <dir>)
	$(info )
	$(info make [CHANNEL=<channel>] ...              On <channel> (else "$(CHANNEL)"):)
	$(info make sign                                 Sign packages et al.)
	$(info make archive-contents                     Create archive-contents)
	$(info make html                                 Create index.html)
	$(info make json                                 Create TODO)
help helpall::
	$(info )
	$(info Testing)
	$(info =======)
	$(info make INSTALL=<package> sandbox            Install <package> in sandbox)
	$(info )
	$(info Cleaning)
	$(info ========)
	$(info make clean                                Remove all generated files)
	$(info )
	@:

## Config

PACKAGE_BUILD_DIRECTORY := $(abspath $(dir $(lastword $(MAKEFILE_LIST))))

ifdef GITHUB_WORKSPACE
  TOP := $(GITHUB_WORKSPACE)
else
  TOP ?= /elpa
endif

MAKE += TOP=$(TOP)

-include $(TOP)/config.mk

CONFIG ?= "()"

ELPA_NAME ?= MyELPA
ELPA_URL  ?= https://owner.github.io/name/
REPO_URL  ?= https://github.com/owner/name/
WIKI_URL  ?= https://github.com/owner/name/wiki/

NOFETCH ?= nil
NOBUILD ?= nil
ASYNC   ?= nil

BUILD_PACKAGES ?=
BUILD_TARGETS  ?= archive-contents json sign

TIMEOUT := $(shell which timeout && echo "-k 60 600")
PAUSE   ?= 0
NOERROR ?= true

OPENPGP_CMD ?= gpg --yes --no-tty --detach-sign --local-user
OPENPGP_KEY ?=

CHANNELS ?= snapshots releases
CHANNEL  ?= snapshots

ifdef DOCKER_CHANNEL
  CHANNEL := $(DOCKER_CHANNEL)
endif

ifeq ($(CHANNEL), snapshots)
  CHANNEL_CONFIG := "(progn\
  (setq package-build-releases nil)\
  (setq package-build-badge-data '(\"snapshots\" \"\#30a14e\")))"

else ifeq ($(CHANNEL), releases)
  CHANNEL_CONFIG := "(progn\
  (setq package-build-releases t)\
  (setq package-build-badge-data '(\"releases\" \"\#9be9a8\")))"

else
  $(error Unknown CHANNEL: $(CHANNEL))
endif

PUBDIR ?= archives
PKGDIR ?= $(PUBDIR:=/)$(CHANNEL)
RCPDIR ?= recipes
SRCDIR ?= sources
PATH_CONFIG ?= '(progn\
  (setq package-build-directory "$(TOP)")\
  (setq package-build-archive-dir "$(TOP)/$(PKGDIR)")\
  (setq package-build-recipes-dir "$(TOP)/$(RCPDIR)")\
  (setq package-build-working-dir "$(TOP)/$(SRCDIR)"))'

EMACS       ?= emacs
EMACS_ARGS  ?=
EMACS_Q_ARG ?= -Q
EMACS_BATCH ?= $(EMACS) $(EMACS_Q_ARG) --batch $(EMACS_ARGS)\
  -L $(PACKAGE_BUILD_DIRECTORY)
EMACS_EVAL   = $(EMACS_BATCH)\
  --eval $(CHANNEL_CONFIG)\
  --eval $(PATH_CONFIG)\
  --eval $(CONFIG)\
  --eval "(setq package-build--inhibit-fetch $(NOFETCH))"\
  --eval "(setq package-build--inhibit-build $(NOBUILD))"\
  --load package-build.el\
  --eval

SHELL := bash

M ?= @echo " • "

ifdef V
  Q=
  MAKE += V=$(V)
else
  Q=@
  MAKEFLAGS += --no-print-directory
endif

MAKE += --file $(PACKAGE_BUILD_DIRECTORY)/package-build.mk

.FORCE:
.PHONY: help fetch build-channels build-channel build \
  archive-contents sign json html clean

## Build

all: build-channels

fetch:
	$(Q)$(MAKE) NOBUILD=t build-channel

build-channels:
	$(Q)for channel in $(CHANNELS); do\
	  NOFETCH=$${NOFETCH=$(NOFETCH)} CHANNEL=$$channel $(MAKE) build-channel\
	  && NOFETCH=t;\
	done

build-channel:
	@echo
	$(M)"Building channel $(CHANNEL)..."
ifneq ($(ASYNC), nil)
	$(Q)$(MAKE) -k -j 8 build || $(NOERROR)
else
	$(Q)$(MAKE) build || $(NOERROR)
endif
	$(Q)$(MAKE) $(BUILD_TARGETS)

ifdef BUILD_PACKAGES
build: $(addprefix $(RCPDIR)/,$(BUILD_PACKAGES))
else
build: $(RCPDIR)/*
endif

$(RCPDIR)/%: .FORCE
	$(Q)mkdir -p $(PKGDIR)
	$(Q)exec 2>&1; exec &> >(tee $(PKGDIR)/$(@F).log); \
	  $(TIMEOUT) $(EMACS_EVAL) "(package-build-archive \"$(@F)\")"
	$(Q)test $(PAUSE) -gt 0 && sleep $(PAUSE) || true

archive-contents: .FORCE
	$(M)"Building archive-contents..."
	$(Q)$(EMACS_EVAL) "(package-build-dump-archive-contents)"

ifdef OPENPGP_KEY
signing ?= $(patsubst %, %.sig, $(wildcard\
	$(PKGDIR)/*.tar\
	$(PKGDIR)/archive-contents\
	$(PKGDIR)/elpa-packages.eld))
sign: $(signing)
else
sign: ;
	@echo -e "\nNo signing key configured!"
endif

%.sig: %
	$(Q)$(OPENPGP_CMD) $(OPENPGP_KEY) $<

json: .FORCE
	$(M)"Building json indices..."
	$(Q)$(EMACS_EVAL) "(package-build-archive-alist-as-json \"$(PKGDIR)/archive.json\")"
	$(Q)$(EMACS_EVAL) "(package-build-recipe-alist-as-json \"$(PKGDIR)/recipes.json\")"

START_URL ?= https://codeberg.org/tarsius/myelpa/wiki/

page: .FORCE
	$(M)"Building page..."
	$(Q)ELPA_NAME=$(ELPA_NAME) ELPA_URL=$(ELPA_URL) \
	REPO_URL=$(REPO_URL) WIKI_URL=$(WIKI_URL) \
	START_URL=$(START_URL) $(EMACS_EVAL) \
	'(package-build--format-webpage "index.html" "$(or $(PUBDIR),$(CHANNEL))")'

## Container

container-build:
	$(Q)docker run \
	--user $$(id --user):$$(id --group) \
	--mount type=bind,src=$$PWD,target=/elpa \
	--mount type=bind,src=$(PACKAGE_BUILD_DIRECTORY),target=/builder \
	--workdir /elpa \
	package-build build-channels

container-image:
	$(Q)docker build -t package-build $(PACKAGE_BUILD_DIRECTORY)

action-setup:
ifdef GITHUB_WORKSPACE
	$(Q)git config --global --add safe.directory '$(GITHUB_WORKSPACE)/sources/*'
else
	@:
endif

## Cleanup

clean:
	$(Q)git clean --quiet --force -x $(or $(PUBDIR),$(CHANNELS))

## Sandbox

SANDBOX ?= sandbox

sandbox: .FORCE
	$(M)"Building sandbox..."
	$(Q)mkdir -p $(SANDBOX)
	$(Q)$(EMACS_EVAL) "(progn\
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
