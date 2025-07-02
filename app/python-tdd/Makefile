.PHONY: help install install-dev test test-watch coverage lint format type-check clean all

# Default target
.DEFAULT_GOAL := help

help: ## Show this help message
	@echo 'Usage: make [target]'
	@echo ''
	@echo 'Targets:'
	@awk 'BEGIN {FS = ":.*?## "} /^[a-zA-Z_-]+:.*?## / {printf "  %-20s %s\n", $$1, $$2}' $(MAKEFILE_LIST)

install: ## Install production dependencies
	pip install -r requirements.txt

install-dev: ## Install development dependencies
	pip install -r requirements-dev.txt

test: ## Run tests
	pytest

test-watch: ## Run tests in watch mode
	ptw --runner "pytest --cov=src --cov-report=term-missing"

coverage: ## Generate coverage report
	pytest --cov=src --cov-report=html --cov-report=term

lint: ## Run linting (pylint and flake8)
	pylint src tests
	flake8 src tests

format: ## Format code with black
	black src tests

format-check: ## Check if code is formatted correctly
	black --check src tests

type-check: ## Run type checking with mypy
	mypy src

clean: ## Clean up generated files
	rm -rf .pytest_cache
	rm -rf htmlcov
	rm -rf .coverage
	rm -rf __pycache__
	find . -type d -name "__pycache__" -delete
	find . -type f -name "*.pyc" -delete

all: format lint type-check test ## Run all checks and tests

# Auto setup for development
setup: install-dev ## Set up development environment
	@echo "Development environment setup complete!"
	@echo "Run 'make test-watch' to start development with automatic testing"
