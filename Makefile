## Help

.DEFAULT_GOAL := all

help helpall::
	$(info )
	$(info Getting Help)
	$(info ============)
	$(info make help                 Show brief help)
	$(info make helpall              Show extended help)
	$(info )
	$(info Building)
	$(info ========)
	$(info )
	$(info Use "MELPA_CHANNEL=<channel> make <target>")
	$(info .    to build like MELPA channel <channel> does.)
	$(info .    <channel> is one of "stable" or "unstable".)
	$(info or use "make <target>")
	$(info .    to build using package-build.el’s default)
	$(info .    settings (which is like channel "unstable").)
	$(info )
helpall::
	$(info Use "PACKAGE_BUILD_REPO=<dir> make <target>")
	$(info .    to use an out-of-tree package-build.el.)
	$(info )
help helpall::
	$(info make recipes/<package>    Build <package>)
	$(info make [-k] [-j 8] build    Build all packages)
	$(info make all                  Build everything)
helpall::
	$(info make -k -j 8 build; make summarise)
	$(info .                         Build everything faster)
	$(info make summarise            Build all package and indices)
	$(info make archive-contents     Build main package index)
	$(info make json                 Build json package index)
	$(info make html                 Build html package index)
	$(info make sign                 Sign packages and main indices)
help helpall::
	$(info )
	$(info Cleaning)
	$(info ========)
	$(info make clean                Empty output directories of all channels)
	$(info .                         Also clean indices but not cloned repos)
	$(info make clean-packages       Empty current channel’s output directory)
helpall::
	$(info make clean-json           Clean current channel’s json index)
	$(info make clean-sandbox        Clean sandbox)
	$(info make clean-working        [DANGER] Remove all cloned repositories)
help helpall::
	$(info )
helpall::
	$(info Maintenance)
	$(info ===========)
	$(info make pull-package-build   Merge new package-build.el version)
	$(info make docker-build-run     Build everything like melpa.org does)
	$(info make docker-build-fetch   Fetch upstream repositories)
	$(info make docker-build-shell   Run interactive shell in the container)
	$(info make docker-build-rebuild Re-build the build container)
help helpall::
	@printf "\n"

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

# Channel build/targeted by other make targets.
# When empty, use "package-build.el"'s default channel settings.
MELPA_CHANNEL ?=

# Channels build by "docker-build-run" target.
# To build all channels use "unstable:stable:snapshot:release".
# To fetch without building use "", which the "docker-build-fetch"
# target does.
DOCKER_BUILD_CHANNELS ?= unstable:stable

# To instruct "docker-build-run" target to build package without
# first pulling them first use non-emtpy INHIBIT_PACKAGE_PULL.

# Seconds to sleep after building a single package.
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
  (setq package-build-build-function\
        'package-build--build-multi-file-package)\
  (setq package-build-snapshot-version-functions\
        '(package-build-timestamp-version))\
  (setq package-build-badge-data '(\"melpa\" \"\#922793\")))"

else ifeq ($(MELPA_CHANNEL), stable)
PKGDIR  := packages-stable
HTMLDIR := html-stable
CHANNEL_CONFIG := "(progn\
  (setq package-build-stable t)\
  (setq package-build-all-publishable nil)\
  (setq package-build-build-function\
        'package-build--build-multi-file-package)\
  (setq package-build-release-version-functions\
        '(package-build-tag-version))\
  (setq package-build-badge-data '(\"melpa stable\" \"\#3e999f\")))"

else ifeq ($(MELPA_CHANNEL), snapshot)
# This is an experimental channel, which may
# eventually replace the "unstable" channel.
PKGDIR  := packages-snapshot
HTMLDIR := html-snapshot
CHANNEL_CONFIG := "(progn\
  (setq package-build-stable nil)\
  (setq package-build-all-publishable t)\
  (setq package-build-snapshot-version-functions\
        '(package-build-release+count-version))\
  (setq package-build-release-version-functions\
        '(package-build-tag-version\
          package-build-header-version))\
  (setq package-build-badge-data '(\"snapshot\" \"\#30a14e\")))"

else ifeq ($(MELPA_CHANNEL), release)
# This is an experimental channel, which may
# eventually replace the "stable" channel.
PKGDIR  := packages-release
HTMLDIR := html-release
CHANNEL_CONFIG := "(progn\
  (setq package-build-stable t)\
  (setq package-build-all-publishable t)\
  (setq package-build-snapshot-version-functions\
        '(package-build-release+count-version))\
  (setq package-build-release-version-functions\
        '(package-build-tag-version\
          package-build-header-version\
          package-build-fallback-count-version))\
  (setq package-build-badge-data '(\"release\" \"\#9be9a8\")))"

else
$(error Unknown MELPA_CHANNEL: $(MELPA_CHANNEL))
endif

# You probably don't want to change this.
LOCATION_CONFIG ?= "(progn\
  (setq package-build--melpa-base \"$(TOP)/\")\
  (setq package-build-working-dir \"$(TOP)/$(WORKDIR)/\")\
  (setq package-build-archive-dir \"$(TOP)/$(PKGDIR)/\")\
  (setq package-build-recipes-dir \"$(TOP)/$(RCPDIR)/\"))"

ifeq ($(INSIDE_DOCKER), true)
# When building on the server, this is the vendored copy.
# When building locally, PACKAGE_BUILD_REPO is mounted here.
LOAD_PATH := $(TOP)/package-build
else ifdef PACKAGE_BUILD_REPO
LOAD_PATH := $(PACKAGE_BUILD_REPO)
else
LOAD_PATH := $(TOP)/package-build
endif

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

## Sign

ifdef OPENPGP_KEY
signatures := $(patsubst %, %.sig, $(wildcard \
	$(PKGDIR)/*.tar \
	$(PKGDIR)/archive-contents \
	$(PKGDIR)/elpa-packages.eld))
sign: $(signatures)
else
sign: ;
	@echo "No signing key configured"
endif

%.sig: %
	gpg --yes --no-tty --detach-sign --local-user ${OPENPGP_KEY} $<

## Metadata

archive-contents: .FORCE
	@echo " • Building archive-contents ..."
	@$(EVAL) "(package-build-dump-archive-contents)"

json: .FORCE
	@echo " • Building json indexes ..."
	@$(EVAL) "(package-build-archive-alist-as-json \"$(HTMLDIR)/archive.json\")"
	@$(EVAL) "(package-build-recipe-alist-as-json \"$(HTMLDIR)/recipes.json\")"

html: .FORCE
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
	MELPA_CHANNEL=snapshot make clean-packages clean-json clean-sandbox
	MELPA_CHANNEL=release  make clean-packages clean-json clean-sandbox

## Update package-build

PACKAGE_BUILD_REPO ?= "https://github.com/melpa/package-build"

pull-package-build:
	git fetch $(PACKAGE_BUILD_REPO)
	git -c "commit.gpgSign=true" subtree merge \
	-m "Merge Package-Build $$(git describe --always FETCH_HEAD)" \
	--squash -P package-build FETCH_HEAD

## Docker

DOCKER_RUN_ARGS = -it \
 --mount type=bind,src=$$PWD,target=/mnt/store/melpa \
 --mount type=bind,src=$(LOAD_PATH),target=/mnt/store/melpa/package-build \
 --env INHIBIT_MELPA_PULL=t \
 --env DOCKER_BUILD_PAUSE=0

docker-build-run:
	docker run $(DOCKER_RUN_ARGS) \
	--env INHIBIT_PACKAGE_PULL=$(INHIBIT_PACKAGE_PULL) \
	--env DOCKER_BUILD_CHANNELS=$(DOCKER_BUILD_CHANNELS) \
	melpa_builder

docker-build-fetch:
	docker run $(DOCKER_RUN_ARGS) \
	--env INHIBIT_PACKAGE_PULL="" \
	--env DOCKER_BUILD_CHANNELS="" \
	melpa_builder

docker-build-shell:
	docker run $(DOCKER_RUN_ARGS) \
	--env INHIBIT_PACKAGE_PULL=$(INHIBIT_PACKAGE_PULL) \
	--env DOCKER_BUILD_CHANNELS=$(DOCKER_BUILD_CHANNELS) \
	melpa_builder bash

docker-build-rebuild:
	docker build \
	--build-arg UID=$$(id --user) \
	--build-arg GID=$$(id --group) \
	-t melpa_builder docker/builder-ng

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
