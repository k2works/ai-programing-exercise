#!/bin/bash

# Simple file watcher for Erlang development
# This script watches for changes in src/ and test/ directories
# and runs tests when files are modified

echo "Starting file watcher for FizzBuzz project..."
echo "Watching src/ and test/ directories for changes..."
echo "Press Ctrl+C to stop"

# Function to run tests
run_tests() {
    echo "File changed, running tests..."
    make test
    echo "Tests completed. Watching for more changes..."
}

# Initial test run
echo "Running initial tests..."
make test
echo "Initial tests completed. Starting watch mode..."

# Use inotifywait if available, otherwise fall back to a simple polling loop
if command -v inotifywait &> /dev/null; then
    # Use inotifywait for efficient file watching
    while true; do
        inotifywait -r -e modify,create,delete src/ test/ 2>/dev/null
        run_tests
    done
else
    # Fallback: simple polling mechanism
    echo "inotifywait not found, using simple polling mode"
    prev_checksum=""
    while true; do
        # Create checksum of all .erl files
        current_checksum=$(find src/ test/ -name "*.erl" -exec md5sum {} \; 2>/dev/null | md5sum)
        
        if [ "$current_checksum" != "$prev_checksum" ] && [ "$prev_checksum" != "" ]; then
            run_tests
        fi
        
        prev_checksum="$current_checksum"
        sleep 2
    done
fi