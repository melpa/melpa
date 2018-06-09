-include config.mk

PKG = package-build

ELS   = $(PKG).el
ELS  += package-build-badges.el
ELS  += package-recipe.el
ELS  += package-recipe-mode.el
ELCS  = $(ELS:.el=.elc)

DEPS  =

EMACS      ?= emacs
EMACS_ARGS ?=

LOAD_PATH  ?= $(addprefix -L ../,$(DEPS))
LOAD_PATH  += -L .

all: lisp

help:
	$(info make [all|lisp]   - generate byte-code and autoloads)
	$(info make clean        - remove generated files)
	@printf "\n"

lisp: $(ELCS) loaddefs

loaddefs: $(PKG)-autoloads.el

%.elc: %.el
	@printf "Compiling $<\n"
	@$(EMACS) -Q --batch \
	--load eieio \
	--eval "(put 'object-class-fast 'byte-obsolete-info nil)" \
	--load eieio-compat \
	--eval "(put 'defmethod 'byte-obsolete-info nil)" \
	--eval "(put 'defgeneric 'byte-obsolete-info nil)" \
	$(EMACS_ARGS) $(LOAD_PATH) -f batch-byte-compile $<

CLEAN  = $(ELCS) $(PKG)-autoloads.el

clean:
	@printf "Cleaning...\n"
	@rm -rf $(CLEAN)

define LOADDEFS_TMPL
;;; $(PKG)-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name \
(or (file-name-directory #$$) (car load-path))))

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; $(PKG)-autoloads.el ends here
endef
export LOADDEFS_TMPL
#'

$(PKG)-autoloads.el: $(ELS)
	@printf "Generating $@\n"
	@printf "%s" "$$LOADDEFS_TMPL" > $@
	@$(EMACS) -Q --batch --eval "(progn\
	(setq make-backup-files nil)\
	(setq vc-handled-backends nil)\
	(setq default-directory (file-truename default-directory))\
	(setq generated-autoload-file (expand-file-name \"$@\"))\
	(setq find-file-visit-truename t)\
	(update-directory-autoloads default-directory))"
