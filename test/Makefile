-include ../config.mk
include ../default.mk

test:
	@export MELPA_BASE=$$(mktemp -d) && \
	$(BATCH) --eval "(progn\
	(setq package-build--melpa-base \"$$MELPA_BASE\")\
	(load-file \"$(TOP)test/package-build-tests.el\")\
	(ert-run-tests-batch-and-exit))" && \
	rm -rf "$$MELPA_BASE"

demo:
	@export MELPA_BASE=$$(mktemp -d) && \
	PB_TEST_VERBOSE=true $(BATCH) --eval "(progn\
	(setq package-build--melpa-base \"$$MELPA_BASE\")\
	(load-file \"$(TOP)test/package-build-tests.el\")\
	(ert-run-tests-batch-and-exit))" && \
	rm -rf "$$MELPA_BASE"

lisp: package-build-tests.elc

%.elc: %.el
	@printf "Compiling $<\n"
	@$(BATCH) --eval "(progn\
	(when (file-exists-p \"$@\")\
	  (delete-file \"$@\")))" \
	-f batch-byte-compile $<
