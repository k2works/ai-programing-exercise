/**
 * Unit tests for Puyo class
 * Tests color validation, position management, state transitions, and other core functionality
 */

import { Puyo } from '../../models/Puyo.js';

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
        console.log('Running Puyo tests...');
        
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

function assertThrows(fn, expectedMessage, message) {
    try {
        fn();
        throw new Error(message || 'Expected function to throw an error');
    } catch (error) {
        if (expectedMessage && !error.message.includes(expectedMessage)) {
            throw new Error(`Expected error message to contain "${expectedMessage}", but got "${error.message}"`);
        }
    }
}

// Create test runner instance
const testRunner = new TestRunner();

// Constructor and validation tests
testRunner.test('should create puyo with valid color and default position', () => {
    const puyo = new Puyo('red');
    
    assertEqual(puyo.color, 'red');
    assertEqual(puyo.x, 0);
    assertEqual(puyo.y, 0);
    assertEqual(puyo.state, Puyo.STATES.FALLING);
    assertEqual(puyo.animationFrame, 0);
});

testRunner.test('should create puyo with valid color and specified position', () => {
    const puyo = new Puyo('blue', 3, 5);
    
    assertEqual(puyo.color, 'blue');
    assertEqual(puyo.x, 3);
    assertEqual(puyo.y, 5);
    assertEqual(puyo.state, Puyo.STATES.FALLING);
});

testRunner.test('should throw error for invalid color', () => {
    assertThrows(() => new Puyo('invalid'), 'Invalid color');
    assertThrows(() => new Puyo(''), 'Color must be a non-empty string');
    assertThrows(() => new Puyo(null), 'Color must be a non-empty string');
    assertThrows(() => new Puyo(123), 'Color must be a non-empty string');
});

testRunner.test('should throw error for invalid position', () => {
    assertThrows(() => new Puyo('red', -1, 0), 'Position coordinates must be non-negative');
    assertThrows(() => new Puyo('red', 0, -1), 'Position coordinates must be non-negative');
    assertThrows(() => new Puyo('red', 1.5, 0), 'Position coordinates must be integers');
    assertThrows(() => new Puyo('red', 0, 2.7), 'Position coordinates must be integers');
    assertThrows(() => new Puyo('red', 'x', 0), 'Position coordinates must be numbers');
    assertThrows(() => new Puyo('red', 0, 'y'), 'Position coordinates must be numbers');
});

testRunner.test('should validate all valid colors', () => {
    const validColors = ['red', 'blue', 'green', 'yellow', 'purple'];
    
    validColors.forEach(color => {
        const puyo = new Puyo(color);
        assertEqual(puyo.color, color);
    });
});

// Position management tests
testRunner.test('should set position correctly', () => {
    const puyo = new Puyo('green');
    
    puyo.setPosition(4, 7);
    assertEqual(puyo.x, 4);
    assertEqual(puyo.y, 7);
});

testRunner.test('should get position as object', () => {
    const puyo = new Puyo('yellow', 2, 3);
    const position = puyo.getPosition();
    
    assertEqual(position.x, 2);
    assertEqual(position.y, 3);
});

testRunner.test('should throw error when setting invalid position', () => {
    const puyo = new Puyo('purple');
    
    assertThrows(() => puyo.setPosition(-1, 0), 'Position coordinates must be non-negative');
    assertThrows(() => puyo.setPosition(0, -1), 'Position coordinates must be non-negative');
    assertThrows(() => puyo.setPosition(1.5, 0), 'Position coordinates must be integers');
});

// State management tests
testRunner.test('should set and get state correctly', () => {
    const puyo = new Puyo('red');
    
    assertEqual(puyo.getState(), Puyo.STATES.FALLING);
    
    puyo.setState(Puyo.STATES.FIXED);
    assertEqual(puyo.getState(), Puyo.STATES.FIXED);
    
    puyo.setState(Puyo.STATES.CLEARING);
    assertEqual(puyo.getState(), Puyo.STATES.CLEARING);
});

testRunner.test('should throw error for invalid state', () => {
    const puyo = new Puyo('blue');
    
    assertThrows(() => puyo.setState('invalid'), 'Invalid state');
    assertThrows(() => puyo.setState(''), 'Invalid state');
    assertThrows(() => puyo.setState(null), 'Invalid state');
});

testRunner.test('should check state correctly with helper methods', () => {
    const puyo = new Puyo('green');
    
    // Initially falling
    assert(puyo.isFalling());
    assert(!puyo.isFixed());
    assert(!puyo.isClearing());
    
    // Set to fixed
    puyo.setFixed();
    assert(!puyo.isFalling());
    assert(puyo.isFixed());
    assert(!puyo.isClearing());
    
    // Set to clearing
    puyo.setClearing();
    assert(!puyo.isFalling());
    assert(!puyo.isFixed());
    assert(puyo.isClearing());
    
    // Set back to falling
    puyo.setFalling();
    assert(puyo.isFalling());
    assert(!puyo.isFixed());
    assert(!puyo.isClearing());
});

// Animation frame tests
testRunner.test('should set and get animation frame', () => {
    const puyo = new Puyo('yellow');
    
    assertEqual(puyo.getAnimationFrame(), 0);
    
    puyo.setAnimationFrame(5);
    assertEqual(puyo.getAnimationFrame(), 5);
    
    puyo.setAnimationFrame(10);
    assertEqual(puyo.getAnimationFrame(), 10);
});

testRunner.test('should throw error for invalid animation frame', () => {
    const puyo = new Puyo('purple');
    
    assertThrows(() => puyo.setAnimationFrame(-1), 'Animation frame must be a non-negative number');
    assertThrows(() => puyo.setAnimationFrame('invalid'), 'Animation frame must be a non-negative number');
    assertThrows(() => puyo.setAnimationFrame(null), 'Animation frame must be a non-negative number');
});

// Clone and equality tests
testRunner.test('should clone puyo correctly', () => {
    const original = new Puyo('red', 2, 3);
    original.setFixed();
    original.setAnimationFrame(7);
    
    const cloned = original.clone();
    
    assertEqual(cloned.color, original.color);
    assertEqual(cloned.x, original.x);
    assertEqual(cloned.y, original.y);
    assertEqual(cloned.state, original.state);
    assertEqual(cloned.animationFrame, original.animationFrame);
    
    // Ensure they are different instances
    assert(cloned !== original);
});

testRunner.test('should check equality correctly', () => {
    const puyo1 = new Puyo('blue', 1, 2);
    const puyo2 = new Puyo('blue', 1, 2);
    const puyo3 = new Puyo('red', 1, 2);
    const puyo4 = new Puyo('blue', 2, 2);
    
    assert(puyo1.equals(puyo2));
    assert(!puyo1.equals(puyo3));
    assert(!puyo1.equals(puyo4));
    assert(!puyo1.equals(null));
    assert(!puyo1.equals('not a puyo'));
});

// String representation tests
testRunner.test('should generate correct string representation', () => {
    const puyo = new Puyo('green', 4, 5);
    puyo.setFixed();
    
    const str = puyo.toString();
    assert(str.includes('green'));
    assert(str.includes('4'));
    assert(str.includes('5'));
    assert(str.includes('fixed'));
});

// JSON serialization tests
testRunner.test('should serialize to JSON correctly', () => {
    const puyo = new Puyo('purple', 3, 4);
    puyo.setClearing();
    puyo.setAnimationFrame(8);
    
    const json = puyo.toJSON();
    
    assertEqual(json.color, 'purple');
    assertEqual(json.x, 3);
    assertEqual(json.y, 4);
    assertEqual(json.state, 'clearing');
    assertEqual(json.animationFrame, 8);
});

testRunner.test('should deserialize from JSON correctly', () => {
    const json = {
        color: 'yellow',
        x: 5,
        y: 6,
        state: 'fixed',
        animationFrame: 12
    };
    
    const puyo = Puyo.fromJSON(json);
    
    assertEqual(puyo.color, 'yellow');
    assertEqual(puyo.x, 5);
    assertEqual(puyo.y, 6);
    assertEqual(puyo.state, 'fixed');
    assertEqual(puyo.animationFrame, 12);
});

testRunner.test('should handle JSON without animation frame', () => {
    const json = {
        color: 'red',
        x: 1,
        y: 2,
        state: 'falling'
    };
    
    const puyo = Puyo.fromJSON(json);
    assertEqual(puyo.animationFrame, 0);
});

// Constants tests
testRunner.test('should have correct color constants', () => {
    const expectedColors = ['red', 'blue', 'green', 'yellow', 'purple'];
    
    assertEqual(Puyo.COLORS.length, expectedColors.length);
    expectedColors.forEach(color => {
        assert(Puyo.COLORS.includes(color));
    });
});

testRunner.test('should have correct state constants', () => {
    assertEqual(Puyo.STATES.FALLING, 'falling');
    assertEqual(Puyo.STATES.FIXED, 'fixed');
    assertEqual(Puyo.STATES.CLEARING, 'clearing');
});

// Export the test runner for use in test HTML
export { testRunner as PuyoTestRunner };

// Auto-run tests if this module is loaded directly
if (typeof window !== 'undefined') {
    // Browser environment - make test runner available globally
    window.PuyoTestRunner = testRunner;
}