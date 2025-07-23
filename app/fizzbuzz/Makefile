.PHONY: compile test clean dialyzer lint all check

# Default target
all: compile test

# Compile the project
compile:
	@echo "Compiling project..."
	@rebar3 compile

# Run tests
test:
	@echo "Running tests..."
	@rebar3 eunit

# Run tests with coverage
cover:
	@echo "Running tests with coverage..."
	@rebar3 eunit --cover

# Run dialyzer
dialyzer:
	@echo "Running Dialyzer..."
	@rebar3 dialyzer

# Run linter
lint:
	@echo "Running linter..."
	@rebar3 lint

# Run all checks
check: compile test dialyzer lint
	@echo "All checks completed."

# Clean build artifacts
clean:
	@echo "Cleaning..."
	@rebar3 clean

# Development target - runs tests when files change
dev:
	@echo "Starting development mode..."
	@echo "Run 'make test' to run tests manually"

# Generate documentation
docs:
	@echo "Generating documentation..."
	@rebar3 edoc

# Shell for interactive development
shell:
	@echo "Starting Erlang shell..."
	@rebar3 shell