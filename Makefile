# Makefile

.PHONY: help all test test-as-is lint compile clean run-emacs

verbose ?= 

help:
	$(info )
	$(info - make            # Show this help)
	$(info - make help       # Show this help)
	$(info - make all        # Run tests, lint and compile)
	$(info - make test       # Run tests)
	$(info - make test-as-is # Run tests and use as-is instead of packaged)
	$(info - make lint       # Lint the package)
	$(info - make compile    # Compiles the files to check for errors/warnings and delete elc after)
	$(info - make clean      # Clean everything)
	$(info - make run-emacs  # Run Emacs with just your project installed)
	$(info )

all: test compile lint

test:
	@printf '\n\e[1;34m%-10s\e[0m\n\n' '>> TEST'
	@eldev --packaged --debug $(verbose) --time test

test-as-is:
	@printf '\n\e[1;34m%-10s\e[0m\n\n' '>> TEST'
	@eldev --as-is --debug $(verbose) --time test

lint:
	@printf '\n\e[1;34m%-10s\e[0m\n\n' '>> LINT'
	@eldev --debug $(verbose) --time lint

compile:
	@printf '\n\e[1;34m%-10s\e[0m\n\n' '>> COMPILE'
	@eldev --debug $(verbose) --time compile --warnings-as-errors
	@eldev clean .elc > /dev/null

clean:
	@printf '\n\e[1;34m%-10s\e[0m\n\n' '>> CLEAN'
	@eldev clean

run-emacs:
	@printf '\n\e[1;34m%-10s\e[0m\n\n' '>> RUN EMACS'
	@eldev emacs
