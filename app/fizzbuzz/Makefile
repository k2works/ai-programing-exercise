# Makefile for FizzBuzz Clojure project

.PHONY: help test lint format coverage check clean deps

# Default target
.DEFAULT_GOAL := help

help: ## Show this help message
	@echo 'Usage: make [target]'
	@echo ''
	@echo 'Targets:'
	@awk 'BEGIN {FS = ":.*?## "} /^[a-zA-Z_-]+:.*?## / {printf "  %-15s %s\n", $$1, $$2}' $(MAKEFILE_LIST)

deps: ## Install dependencies
	lein deps

test: ## Run tests
	lein test

lint: ## Run static code analysis
	lein lint

format: ## Format code
	lein format

coverage: ## Run code coverage analysis
	lein coverage

check: ## Run all checks (lint, test, coverage)
	lein check

clean: ## Clean build artifacts
	lein clean
	rm -rf target/

watch: ## Run tests in watch mode (requires entr)
	find src test -name "*.clj" | entr -r make test

repl: ## Start REPL
	lein repl

guard: ## Start continuous testing (similar to Ruby Guard)
	@echo "Starting continuous testing..."
	@echo "Watching for file changes..."
	@$(MAKE) watch
