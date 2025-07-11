.PHONY: help install install-dev test test-watch coverage lint format type-check clean all

# Default target
.DEFAULT_GOAL := test-watch

help: ## Show this help message
	@echo 'Usage: make [target]'
	@echo ''
	@echo 'Targets:'
	@awk 'BEGIN {FS = ":.*?## "} /^[a-zA-Z_-]+:.*?## / {printf "  %-20s %s\n", $$1, $$2}' $(MAKEFILE_LIST)

test: ## Run tests
	pytest

lint: ## Run linting (pylint and flake8)
	pylint src tests
	flake8 src tests

format: ## Format code with black
	black src tests

type-check: ## Run type checking with mypy
	mypy src

coverage: ## Generate coverage report
	pytest --cov=src --cov-report=html --cov-report=term

test-watch: ## Run tests in watch mode
	ptw --runner "pytest --cov=src --cov-report=term-missing"

all: format lint type-check test ## Run all checks and tests
