EMACS ?= emacs
SRC_DIR := $(dir $(abspath $(lastword $(MAKEFILE_LIST))))
PROFILE_DIR ?= $(patsubst %/elpaca/sources/emacs-slack/,%,$(SRC_DIR))
ELPACA_BUILDS := $(PROFILE_DIR)/elpaca/builds
EXTRAS_DIR := $(HOME)/My Drive/dotfiles/emacs/extras

LOAD_PATH := -L $(SRC_DIR) -L $(SRC_DIR)test
LOAD_PATH += $(patsubst %,-L %,$(wildcard $(ELPACA_BUILDS)/*/))
LOAD_PATH += -L "$(EXTRAS_DIR)"

BATCH := $(EMACS) --batch -Q $(LOAD_PATH)

.PHONY: test test-upstream test-suite test-buffer compile clean

test: compile test-upstream test-suite test-buffer

compile:
	$(BATCH) --eval '(batch-byte-compile)' *.el

test-upstream:
	$(BATCH) -l emacs-slack -l test/run-test.el

test-suite:
	$(BATCH) -l emacs-slack -l test/test-suite.el \
	  --eval '(ert-run-tests-batch-and-exit)'

test-buffer:
	$(BATCH) -l emacs-slack -l test/test-buffer-rendering.el \
	  --eval '(ert-run-tests-batch-and-exit)'

clean:
	rm -f *.elc
