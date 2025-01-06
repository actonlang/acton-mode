EMACS ?= emacs
BATCH := $(EMACS) -Q -batch -L .

.PHONY: all clean test

all: clean
	$(BATCH) -f batch-byte-compile *.el

clean:
	rm -f *.elc

test: all
	$(BATCH) -l acton-mode-tests.el -f ert-run-tests-batch-and-exit
