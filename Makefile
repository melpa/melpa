TOP := $(abspath $(dir $(lastword $(MAKEFILE_LIST))))
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
	$(info make clean-packages                       Remove all generated packages)
helpall::
	$(info make clean-indices                        Remove all generated indices)
	$(info make remove-sandbox                       Remove all package test installations)
	$(info make remove-repositories                  Remove all cloned package repositories)
	$(info )
	$(info Building with Docker)
	$(info ====================)
	$(info make docker-build                         Build everything like melpa.org does)
	$(info make docker-fetch                         Fetch upstream repositories)
	$(info make docker-shell                         Run interactive shell in the container)
	$(info make docker-image                         Re-build the build container)
	$(info )
	$(info Maintenance)
	$(info ===========)
	$(info make pull-package-build                   Merge new package-build.el version)
help helpall::
	$(info )
	@:

## Config

-include ./config.mk

ifeq ($(INSIDE_DOCKER), true)
  PACKAGE_BUILD_DIRECTORY := $(TOP)/package-build
else
  PACKAGE_BUILD_DIRECTORY ?= $(TOP)/package-build
endif

CONFIG ?= "()"

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

CHANNELS ?= unstable stable snapshots releases
CHANNEL  ?= unstable

ifdef DOCKER_CHANNEL
  CHANNEL := $(DOCKER_CHANNEL)
endif

ifeq ($(CHANNEL), unstable)
  PKGDIR  := packages
  HTMLDIR := html
  CHANNEL_CONFIG := "(progn\
  (setq package-build-stable nil)\
  (setq package-build-build-function 'package-build--build-multi-file-package)\
  (setq package-build-snapshot-version-functions '(package-build-timestamp-version))\
  (setq package-build-badge-data '(\"melpa\" \"\#922793\")))"

else ifeq ($(CHANNEL), stable)
  PKGDIR  := packages-stable
  HTMLDIR := html-stable
  CHANNEL_CONFIG := "(progn\
  (setq package-build-stable t)\
  (setq package-build-all-publishable nil)\
  (setq package-build-build-function 'package-build--build-multi-file-package)\
  (setq package-build-release-version-functions '(package-build-tag-version))\
  (setq package-build-badge-data '(\"melpa stable\" \"\#3e999f\")))"

else ifeq ($(CHANNEL), snapshots)
  # This is an experimental channel, which may
  # eventually replace the "unstable" channel.
  PKGDIR  := packages-snapshots
  HTMLDIR := html-snapshots
  CHANNEL_CONFIG := "(progn\
  (setq package-build-stable nil)\
  (setq package-build-badge-data '(\"snapshots\" \"\#30a14e\")))"

else ifeq ($(CHANNEL), releases)
  # This is an experimental channel, which may
  # eventually replace the "stable" channel.
  PKGDIR  := packages-releases
  HTMLDIR := html-releases
  CHANNEL_CONFIG := "(progn\
  (setq package-build-stable t)\
  (setq package-build-badge-data '(\"releases\" \"\#9be9a8\")))"

else
  $(error Unknown CHANNEL: $(CHANNEL))
endif

PKGDIR ?= $(CHANNEL)
RCPDIR ?= recipes
SRCDIR ?= working
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
  --eval "$(DOCKER_BUILD_CONFIG)"\
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
	$(Q)$(EMACS_EVAL) "(package-build-archive-alist-as-json \"$(HTMLDIR)/archive.json\")"
	$(Q)$(EMACS_EVAL) "(package-build-recipe-alist-as-json \"$(HTMLDIR)/recipes.json\")"

html: .FORCE
	$(M)"Building html index..."
	$(Q)$(MAKE) -C $(HTMLDIR)

## Cleanup

HTMLDIRS = html html-stable html-snapshots html-releases
PKGDIRS  = packages packages-stable packages-snapshots packages-releases

INDICES  = $(addsuffix /archive.json,$(HTMLDIRS))
INDICES += $(addsuffix /recipes.json,$(HTMLDIRS))
INDICES += $(addsuffix /updates.rss,$(HTMLDIRS))
INDICES += $(addsuffix /archive-contents,$(PKGDIRS))
INDICES += $(addsuffix /elpa-packages.eld,$(PKGDIRS))
# Only created by docker targets:
INDICES += $(addsuffix /errors.log,$(PKGDIRS))
INDICES += $(addsuffix /errors-previous.log,$(PKGDIRS))
# Directory hardcoded in "run.sh" and symlinked for channels.
INDICES += html/build-status.json

clean:
	$(M)"Removing indices..."
	$(M)"Removing packages..."
	$(Q)git clean --quiet --force -x $(HTMLDIRS) $(PKGDIRS)

clean-packages:
	$(M)"Removing packages..."
	$(Q)git clean --quiet --force -x $(HTMLDIRS) $(PKGDIRS) \
	$(addprefix -e /,$(INDICES))

clean-indices:
	$(M)"Removing indices..."
	$(Q)rm -f $(sort $(INDICES))

remove-sandbox:
	$(M)"Removing $(SANDBOX)..."
	$(Q)rm -rf $(SANDBOX)

remove-repositories:
	$(M)"Removing $(WORKDIR)..."
	$(Q)rm -rf $(WORKDIR)

## Update package-build

PACKAGE_BUILD_REPO ?= "https://github.com/melpa/package-build"

pull-package-build:
	$(Q)git fetch $(PACKAGE_BUILD_REPO)
	$(Q)git -c "commit.gpgSign=true" subtree \
	$(shell test -e package-build && echo merge || echo add) \
	-m "Merge Package-Build $$(git describe --always FETCH_HEAD)" \
	--squash -P package-build FETCH_HEAD

## Docker

# Channels build by the "docker-build-run" target.
# To build all channels use "unstable stable snapshots releases".
# To fetch without building use "", which the "docker-build-fetch"
# target does.  (Keep in sync with "docker/builder/run.sh".)
DOCKER_CHANNELS ?= unstable stable

# Only intended for "docker/builder/run.sh" and similar scripts.  That
# is also why we add extra quoting when setting EMACS_EVAL, instead of
# here.  Not doing it like that would complicate the quoting needed in
# scripts.
DOCKER_BUILD_CONFIG ?= ()

DOCKER_INHIBIT_PACKAGE_PULL ?= nil

DOCKER_RUN_ARGS = \
 --user $$(id --user):$$(id --group) \
 --mount type=bind,src=$$PWD,target=/mnt/store/melpa \
 --mount type=bind,src=$(PACKAGE_BUILD_DIRECTORY),target=/mnt/store/melpa/package-build \
 --env INHIBIT_MELPA_PULL=t \
 --env BUILD_PAUSE=0

docker-build:
	@docker run $(DOCKER_RUN_ARGS) \
	--env INHIBIT_PACKAGE_PULL=$(DOCKER_INHIBIT_PACKAGE_PULL) \
	--env DOCKER_CHANNELS="$(DOCKER_CHANNELS)" \
	melpa_builder

docker-fetch:
	@docker run $(DOCKER_RUN_ARGS) \
	--env INHIBIT_PACKAGE_PULL="" \
	--env DOCKER_CHANNELS="" \
	melpa_builder

docker-shell:
	@docker run $(DOCKER_RUN_ARGS) \
	--env INHIBIT_PACKAGE_PULL=$(DOCKER_INHIBIT_PACKAGE_PULL) \
	--env DOCKER_CHANNELS="$(DOCKER_CHANNELS)" \
	melpa_builder bash

docker-image:
	@docker build -t melpa_builder docker/builder

get-pkgdir: .FORCE
	@echo $(PKGDIR)

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
