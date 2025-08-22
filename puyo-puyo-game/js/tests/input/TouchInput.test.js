/**
 * Unit tests for Touch Input functionality in InputHandler
 * Tests touch gesture recognition, tap zones, swipe detection, and mobile-specific features
 */

import { InputHandler } from '../../input/InputHandler.js';

// Simple test framework for browser environment
class TestRunner {
    constructor() {
        this.tests = [];
        this.results = [];
    }

    test(name, testFn) {
        this.tests.push({ name, testFn });
    }

    async run() {
        console.log('Running Touch Input tests...');
        
        for (const { name, testFn } of this.tests) {
            try {
                await testFn();
                this.results.push({ name, status: 'PASS', error: null });
                console.log(`✓ ${name}`);
            } catch (error) {
                this.results.push({ name, status: 'FAIL', error: error.message });
                console.error(`✗ ${name}: ${error.message}`);
            }
        }
        
        this.printSummary();
        return this.results;
    }

    printSummary() {
        const passed = this.results.filter(r => r.status === 'PASS').length;
        const failed = this.results.filter(r => r.status === 'FAIL').length;
        
        console.log(`\nTest Summary: ${passed} passed, ${failed} failed`);
        
        if (failed > 0) {
            console.log('\nFailed tests:');
            this.results.filter(r => r.status === 'FAIL').forEach(r => {
                console.log(`  - ${r.name}: ${r.error}`);
            });
        }
    }
}

// Helper function for assertions
function assert(condition, message) {
    if (!condition) {
        throw new Error(message || 'Assertion failed');
    }
}

function assertEqual(actual, expected, message) {
    if (actual !== expected) {
        throw new Error(message || `Expected ${expected}, but got ${actual}`);
    }
}

function assertDeepEqual(actual, expected, message) {
    if (JSON.stringify(actual) !== JSON.stringify(expected)) {
        throw new Error(message || `Expected ${JSON.stringify(expected)}, but got ${JSON.stringify(actual)}`);
    }
}

// Mock GameEngine for testing
class MockGameEngine {
    constructor() {
        this.lastInput = null;
        this.inputHistory = [];
    }

    handleInput(action) {
        this.lastInput = action;
        this.inputHistory.push(action);
    }

    getLastInput() {
        return this.lastInput;
    }

    getInputHistory() {
        return this.inputHistory;
    }

    clearHistory() {
        this.lastInput = null;
        this.inputHistory = [];
    }
}

// Mock Canvas element for testing
function createMockCanvas(width = 300, height = 600) {
    return {
        id: 'game-canvas',
        addEventListener: function(event, handler) {
            this[`_${event}`] = handler;
        },
        getBoundingClientRect: function() {
            return {
                left: 0,
                top: 0,
                width: width,
                height: heig