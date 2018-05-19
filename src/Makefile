stackbin := $(shell stack path --local-install-root)/bin
build:
	@echo "Building interpreters..."
	@stack build
	@cp -r $(stackbin) .

clean:
	@echo "Cleaning interpreters..."
	@stack clean --full
	@rm -rf .stack-work bin

test:
	@stack test

.PHONY: build clean test
