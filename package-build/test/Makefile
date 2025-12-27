-include ../config.mk
include ../default.mk

LOAD_PATH += -L ../

test: lisp
	@export MELPA_BASE=$$(mktemp -d) && \
	$(EMACS_BATCH) --eval "(progn\
	(setq package-build--melpa-base \"$$MELPA_BASE\")\
	(load-file \"$(TOP)test/package-build-tests.el\")\
	(ert-run-tests-batch-and-exit))" && \
	rm -rf "$$MELPA_BASE"

demo: lisp
	@export MELPA_BASE=$$(mktemp -d) && \
	PB_TEST_VERBOSE=true $(EMACS_BATCH) --eval "(progn\
	(setq package-build--melpa-base \"$$MELPA_BASE\")\
	(load-file \"$(TOP)test/package-build-tests.el\")\
	(ert-run-tests-batch-and-exit))" && \
	rm -rf "$$MELPA_BASE"

lisp: $(addprefix ../,$(ELCS)) $(PKG)-tests.elc

%.elc: %.el
	@printf "Compiling $<\n"
	@$(EMACS_BATCH) --funcall batch-byte-compile $<
