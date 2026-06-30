-include config.mk
include default.mk

.PHONY: test

all: lisp

help:
	$(info make all     -- Build lisp)
	$(info make lisp    -- Build lisp)
	$(info make redo    -- Build lisp from scratch)
	$(info make test    -- Run tests)
	$(info make demo    -- Demo version generation)
	$(info make clean   -- Remove built files)
	@printf "\n"

redo: clean $(ELCS) autoloads check-declare
	@$(MAKE) -C test lisp

lisp: $(ELCS) autoloads check-declare
	@$(MAKE) -C test lisp

autoloads: $(PKG)-autoloads.el

%.elc: %.el
	@printf "Compiling $<\n"
	@$(EMACS_BATCH) --funcall batch-byte-compile $<

check-declare:
	@printf " Checking function declarations\n"
	@$(EMACS_BATCH) --eval "(check-declare-directory default-directory)"

test:
	@$(MAKE) -C test test

demo:
	@$(MAKE) -C test demo

CLEAN = $(ELCS) $(PKG)-autoloads.el test/$(PKG)-tests.elc

clean:
	@printf " Cleaning...\n"
	@rm -rf $(CLEAN)

$(PKG)-autoloads.el: $(ELS)
	@printf " Creating $@\n"
	@$(EMACS_BATCH) --load autoload --eval "\
(let* ((file (expand-file-name \"$@\"))\
       (generated-autoload-file file)\
       (coding-system-for-write 'utf-8-emacs-unix)\
       (backup-inhibited t)\
       (version-control 'never)\
       (inhibit-message t))\
  (write-region (autoload-rubric file \"package\" t) nil file)\
  (update-directory-autoloads default-directory))" \
	2>&1 | sed "/^Package autoload is deprecated$$/d"
