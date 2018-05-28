stackbin      := $(shell stack path --local-install-root)/bin
stacklocalbin := $(shell stack path --local-bin)
projectbin    := $(shell pwd)/bin
build:
	@echo "Building interpreters..."
	@stack build
	@cp -r $(stackbin) .
	@echo "Interpreters builded to $(projectbin)."

install:
	@echo "Installing interpreters..."
	@stack install
	@echo "Installed to $(stacklocalbin)."

clean:
	@echo "Cleaning interpreters..."
	@stack clean --full
	@rm -rf .stack-work bin
	@echo "Interpreters cleaned."

test:
	@stack test

.PHONY: build clean test
