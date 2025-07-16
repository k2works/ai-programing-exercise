.PHONY: help test coverage cs-check cs-fix quality phpmd cpd dev watch

# Default target
help: ## Show this help message
	@echo 'Usage: make [target]'
	@echo ''
	@echo 'Targets:'
	@awk 'BEGIN {FS = ":.*?## "} /^[a-zA-Z_-]+:.*?## / {printf "\033[36m%-15s\033[0m %s\n", $$1, $$2}' $(MAKEFILE_LIST)

# Test commands
test: ## Run tests
	composer test

coverage: ## Run tests with coverage
	composer test:coverage

# Code quality commands  
cs-check: ## Check code style
	composer cs:check

cs-fix: ## Fix code style
	composer cs:fix

phpmd: ## Run PHP Mess Detector
	composer phpmd

cpd: ## Run PHP Copy/Paste Detector
	composer cpd

# Combined commands
quality: ## Run all quality checks
	composer quality

dev: ## Run development workflow (fix style + coverage)
	composer dev

# Watch mode (requires entr or inotifywait)
watch: ## Watch files and run tests automatically
	@echo "Watching for file changes... (requires 'entr' or 'inotifywait')"
	@if command -v entr >/dev/null 2>&1; then \
		find src tests -name "*.php" | entr -c make test; \
	elif command -v inotifywait >/dev/null 2>&1; then \
		while inotifywait -e modify,create,delete -r src tests; do make test; done; \
	else \
		echo "Please install 'entr' or 'inotifywait' for watch functionality"; \
		exit 1; \
	fi

# Install dependencies
install: ## Install dependencies
	composer install

update: ## Update dependencies
	composer update

# Clean up
clean: ## Clean up generated files
	rm -rf coverage/
	rm -f .php-cs-fixer.cache
