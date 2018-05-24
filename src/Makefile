stackbin := $(shell stack path --local-install-root)/bin
stacklocalbin := $(shell stack path --local-bin)
build:
	@echo "Building interpreters..."
	@stack build
	@cp -r $(stackbin) .

install:
	@echo "Installing interpreters..."
	@stack install
	@echo "Installed to $(stacklocalbin)."

clean:
	@echo "Cleaning interpreters..."
	@stack clean --full
	@rm -rf .stack-work bin

test:
	@stack test

.PHONY: build clean test
