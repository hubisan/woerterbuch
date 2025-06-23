# Makefile

EMACS ?= emacs
EASK ?= eask

.PHONY: help all clean package install compile test test-local docker-test lint emacs

help:
	$(info )
	$(info - make            # Show this help)
	$(info - make all        # Run clean, package, install, compile, lint and test)
	$(info - make clean      # Clean everything)
	$(info - make package    # Build package artifact)
	$(info - make install    # Install the package)
	$(info - make compile    # Compiles the files to check for errors/warnings)
	$(info - make test       # Run tests with buttercup)
	$(info - make test-local # Test locally (compile, test, lint))
	$(info - make lint       # Clean autoloads and run linters)
	$(info - make emacs      # Run Emacs with package and dependencies installed)
	$(info )
	@echo > /dev/null

all: clean package install compile lint test

clean:
	@printf '\n\e[1;34m%-10s\e[0m\n\n' '>> CLEAN ALL'
	@$(EASK) clean all

package:
	@printf '\n\e[1;34m%-10s\e[0m\n\n' '>> PACKAGING'
	@$(EASK) package

install:
	@printf '\n\e[1;34m%-10s\e[0m\n\n' '>> INSTALL'
	@$(EASK) install

compile:
	@printf '\n\e[1;34m%-10s\e[0m\n\n' '>> COMPILE'
	@$(EASK) recompile

test:
	@printf '\n\e[1;34m%-10s\e[0m\n\n' '>> TEST'
	@$(EASK) install-deps --dev
	@$(EASK) test buttercup

test-local: compile test lint

lint:
	@printf '\n\e[1;34m%-10s\e[0m\n\n' '>> LINT'
	@$(EASK) clean autoloads --verbose 0
	@printf '\e[1;34m%-10s\e[0m\n\n' '>>> package-lint'
	@$(EASK) lint package --verbose 0
	@printf '\e[1;34m%-10s\e[0m\n\n' '>>> checkdoc'
	@$(EASK) lint checkdoc --verbose 0
	@printf '\e[1;34m%-10s\e[0m\n' '>>> indent-lint'
	@$(EASK) lint indent --verbose 0
	@printf '\e[1;34m%-10s\e[0m\n\n' '>>> relint'
	@$(EASK) lint regexps --verbose 0

docker-test:
ifndef emacs
	$(error emacs is undefined. Use 'make docker-test emacs=<version>')
endif
	@printf '\n\e[1;34m%-10s\e[0m\n\n' '>> COMPILE'
	$(EASK) docker $(emacs) recompile
	@printf '\n\e[1;34m%-10s\e[0m\n\n' '>> TEST'
	$(EASK) docker $(emacs) install-deps --dev
	$(EASK) docker $(emacs) test buttercup
	$(EASK) docker $(emacs) clean autoloads
	@printf '\n\e[1;34m%-10s\e[0m\n\n' '>> LINT'
	$(EASK) docker $(emacs) lint package --verbose 0
	$(EASK) docker $(emacs) lint elint --verbose 0
	$(EASK) docker $(emacs) lint checkdoc --verbose 0
	$(EASK) docker $(emacs) lint indent --verbose 0
	$(EASK) docker $(emacs) lint regexps --verbose 0

emacs:
	@printf '\n\e[1;34m%-10s\e[0m\n\n' '>> RUN EMACS'
	$(EASK) install
	$(EASK) emacs &
