/**
 * Unit tests for PuyoPair class
 * Tests pair creation, rotation logic, movement, and position management
 */

import { PuyoPair } from '../../models/PuyoPair.js';
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
        console.log('Running PuyoPair tests...');
        
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

// Helper functions for assertions
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
testRunner.test('should create puyo pair with valid puyos', () => {
    const puyo1 = new Puyo('red');
    const puyo2 = new Puyo('blue');
    const pair = new PuyoPair(puyo1, puyo2);
    
    assertEqual(pair.puyo1, puyo1);
    assertEqual(pair.puyo2, puyo2);
    assertEqual(pair.rotation, 0);
    assertEqual(pair.x, 2);
    assertEqual(pair.y, 1);
    assertEqual(pair.fallSpeed, 1);
    assertEqual(pair.fastDrop, false);
});

testRunner.test('should throw error for invalid puyo parameters', () => {
    const puyo = new Puyo('red');
    
    assertThrows(() => new PuyoPair(null, puyo), 'puyo1 must be an instance of Puyo');
    assertThrows(() => new PuyoPair(puyo, null), 'puyo2 must be an instance of Puyo');
    assertThrows(() => new PuyoPair('not a puyo', puyo), 'puyo1 must be an instance of Puyo');
    assertThrows(() => new PuyoPair(puyo, 'not a puyo'), 'puyo2 must be an instance of Puyo');
});

testRunner.test('should update puyo positions on creation', () => {
    const puyo1 = new Puyo('red');
    const puyo2 = new Puyo('blue');
    const pair = new PuyoPair(puyo1, puyo2);
    
    // At rotation 0, puyo1 should be at (2,1) and puyo2 at (2,0)
    assertEqual(pair.puyo1.x, 2);
    assertEqual(pair.puyo1.y, 1);
    assertEqual(pair.puyo2.x, 2);
    assertEqual(pair.puyo2.y, 0);
});

// Position management tests
testRunner.test('should set and get position correctly', () => {
    const puyo1 = new Puyo('green');
    const puyo2 = new Puyo('yellow');
    const pair = new PuyoPair(puyo1, puyo2);
    
    pair.setPosition(3, 5);
    
    assertEqual(pair.x, 3);
    assertEqual(pair.y, 5);
    
    const position = pair.getPosition();
    assertEqual(position.x, 3);
    assertEqual(position.y, 5);
});

testRunner.test('should update puyo positions when pair position changes', () => {
    const puyo1 = new Puyo('purple');
    const puyo2 = new Puyo('red');
    const pair = new PuyoPair(puyo1, puyo2);
    
    pair.setPosition(4, 6);
    
    // At rotation 0, puyo1 should be at (4,6) and puyo2 at (4,5)
    assertEqual(pair.puyo1.x, 4);
    assertEqual(pair.puyo1.y, 6);
    assertEqual(pair.puyo2.x, 4);
    assertEqual(pair.puyo2.y, 5);
});

testRunner.test('should throw error for invalid position', () => {
    const puyo1 = new Puyo('blue');
    const puyo2 = new Puyo('green');
    const pair = new PuyoPair(puyo1, puyo2);
    
    assertThrows(() => pair.setPosition('x', 0), 'Position coordinates must be numbers');
    assertThrows(() => pair.setPosition(0, 'y'), 'Position coordinates must be numbers');
    assertThrows(() => pair.setPosition(1.5, 0), 'Position coordinates must be integers');
    assertThrows(() => pair.setPosition(0, 2.7), 'Position coordinates must be integers');
});

// Rotation tests
testRunner.test('should rotate clockwise correctly', () => {
    const puyo1 = new Puyo('red');
    const puyo2 = new Puyo('blue');
    const pair = new PuyoPair(puyo1, puyo2);
    pair.setPosition(3, 3); // Set to a position where all rotations are valid
    
    // Initial rotation (0°): puyo2 above puyo1
    assertEqual(pair.getRotation(), 0);
    assertEqual(pair.getRotationDegrees(), 0);
    assertEqual(pair.puyo1.x, 3);
    assertEqual(pair.puyo1.y, 3);
    assertEqual(pair.puyo2.x, 3);
    assertEqual(pair.puyo2.y, 2);
    
    // Rotate to 90°: puyo2 to the right of puyo1
    pair.rotate();
    assertEqual(pair.getRotation(), 1);
    assertEqual(pair.getRotationDegrees(), 90);
    assertEqual(pair.puyo1.x, 3);
    assertEqual(pair.puyo1.y, 3);
    assertEqual(pair.puyo2.x, 4);
    assertEqual(pair.puyo2.y, 3);
    
    // Rotate to 180°: puyo2 below puyo1
    pair.rotate();
    assertEqual(pair.getRotation(), 2);
    assertEqual(pair.getRotationDegrees(), 180);
    assertEqual(pair.puyo1.x, 3);
    assertEqual(pair.puyo1.y, 3);
    assertEqual(pair.puyo2.x, 3);
    assertEqual(pair.puyo2.y, 4);
    
    // Rotate to 270°: puyo2 to the left of puyo1
    pair.rotate();
    assertEqual(pair.getRotation(), 3);
    assertEqual(pair.getRotationDegrees(), 270);
    assertEqual(pair.puyo1.x, 3);
    assertEqual(pair.puyo1.y, 3);
    assertEqual(pair.puyo2.x, 2);
    assertEqual(pair.puyo2.y, 3);
    
    // Rotate back to 0°
    pair.rotate();
    assertEqual(pair.getRotation(), 0);
    assertEqual(pair.getRotationDegrees(), 0);
});

testRunner.test('should rotate counter-clockwise correctly', () => {
    const puyo1 = new Puyo('green');
    const puyo2 = new Puyo('yellow');
    const pair = new PuyoPair(puyo1, puyo2);
    pair.setPosition(3, 3);
    
    // Start at 0°, rotate counter-clockwise to 270°
    pair.rotateCounterClockwise();
    assertEqual(pair.getRotation(), 3);
    assertEqual(pair.getRotationDegrees(), 270);
    
    // Continue counter-clockwise to 180°
    pair.rotateCounterClockwise();
    assertEqual(pair.getRotation(), 2);
    assertEqual(pair.getRotationDegrees(), 180);
});

testRunner.test('should set specific rotation correctly', () => {
    const puyo1 = new Puyo('purple');
    const puyo2 = new Puyo('red');
    const pair = new PuyoPair(puyo1, puyo2);
    pair.setPosition(3, 3);
    
    pair.setRotation(2);
    assertEqual(pair.getRotation(), 2);
    assertEqual(pair.getRotationDegrees(), 180);
    
    pair.setRotation(0);
    assertEqual(pair.getRotation(), 0);
    assertEqual(pair.getRotationDegrees(), 0);
});

testRunner.test('should throw error for invalid rotation', () => {
    const puyo1 = new Puyo('blue');
    const puyo2 = new Puyo('green');
    const pair = new PuyoPair(puyo1, puyo2);
    
    assertThrows(() => pair.setRotation(-1), 'Rotation must be an integer between 0 and 3');
    assertThrows(() => pair.setRotation(4), 'Rotation must be an integer between 0 and 3');
    assertThrows(() => pair.setRotation(1.5), 'Rotation must be an integer between 0 and 3');
    assertThrows(() => pair.setRotation('invalid'), 'Rotation must be an integer between 0 and 3');
});

// Movement tests
testRunner.test('should move in all directions correctly', () => {
    const puyo1 = new Puyo('red');
    const puyo2 = new Puyo('blue');
    const pair = new PuyoPair(puyo1, puyo2);
    pair.setPosition(3, 3);
    
    // Move left
    pair.move('left');
    assertEqual(pair.x, 2);
    assertEqual(pair.y, 3);
    
    // Move right
    pair.move('right');
    assertEqual(pair.x, 3);
    assertEqual(pair.y, 3);
    
    // Move down
    pair.move('down');
    assertEqual(pair.x, 3);
    assertEqual(pair.y, 4);
});

testRunner.test('should throw error for invalid move direction', () => {
    const puyo1 = new Puyo('green');
    const puyo2 = new Puyo('yellow');
    const pair = new PuyoPair(puyo1, puyo2);
    
    assertThrows(() => pair.move('up'), 'Invalid direction');
    assertThrows(() => pair.move('invalid'), 'Invalid direction');
    assertThrows(() => pair.move(''), 'Invalid direction');
});

// Fall speed tests
testRunner.test('should set and get fall speed correctly', () => {
    const puyo1 = new Puyo('purple');
    const puyo2 = new Puyo('red');
    const pair = new PuyoPair(puyo1, puyo2);
    
    assertEqual(pair.getFallSpeed(), 1);
    
    pair.setFallSpeed(2.5);
    assertEqual(pair.getFallSpeed(), 2.5);
});

testRunner.test('should throw error for invalid fall speed', () => {
    const puyo1 = new Puyo('blue');
    const puyo2 = new Puyo('green');
    const pair = new PuyoPair(puyo1, puyo2);
    
    assertThrows(() => pair.setFallSpeed(0), 'Fall speed must be a positive number');
    assertThrows(() => pair.setFallSpeed(-1), 'Fall speed must be a positive number');
    assertThrows(() => pair.setFallSpeed('invalid'), 'Fall speed must be a positive number');
});

// Fast drop tests
testRunner.test('should handle fast drop correctly', () => {
    const puyo1 = new Puyo('red');
    const puyo2 = new Puyo('blue');
    const pair = new PuyoPair(puyo1, puyo2);
    
    assertEqual(pair.isFastDropEnabled(), false);
    assertEqual(pair.getEffectiveFallSpeed(), 1);
    
    pair.enableFastDrop();
    assertEqual(pair.isFastDropEnabled(), true);
    assertEqual(pair.getEffectiveFallSpeed(), 10); // Default multiplier
    assertEqual(pair.getEffectiveFallSpeed(5), 5); // Custom multiplier
    
    pair.disableFastDrop();
    assertEqual(pair.isFastDropEnabled(), false);
    assertEqual(pair.getEffectiveFallSpeed(), 1);
});

// State management tests
testRunner.test('should manage puyo states correctly', () => {
    const puyo1 = new Puyo('green');
    const puyo2 = new Puyo('yellow');
    const pair = new PuyoPair(puyo1, puyo2);
    
    // Initially both should be falling
    assert(pair.isFalling());
    assert(!pair.isFixed());
    
    // Set to fixed
    pair.setFixed();
    assert(!pair.isFalling());
    assert(pair.isFixed());
    assert(pair.puyo1.isFixed());
    assert(pair.puyo2.isFixed());
    
    // Set back to falling
    pair.setFalling();
    assert(pair.isFalling());
    assert(!pair.isFixed());
    assert(pair.puyo1.isFalling());
    assert(pair.puyo2.isFalling());
});

// Position calculation tests
testRunner.test('should get puyo positions correctly', () => {
    const puyo1 = new Puyo('purple');
    const puyo2 = new Puyo('red');
    const pair = new PuyoPair(puyo1, puyo2);
    pair.setPosition(4, 5);
    
    const positions = pair.getPuyoPositions();
    
    assertEqual(positions.puyo1.x, 4);
    assertEqual(positions.puyo1.y, 5);
    assertEqual(positions.puyo2.x, 4);
    assertEqual(positions.puyo2.y, 4); // Above puyo1 at rotation 0
});

testRunner.test('should calculate potential positions correctly', () => {
    const puyo1 = new Puyo('blue');
    const puyo2 = new Puyo('green');
    const pair = new PuyoPair(puyo1, puyo2);
    pair.setPosition(3, 3);
    
    // Test potential move left
    const leftPositions = pair.getPotentialPositions('left');
    assertEqual(leftPositions.puyo1.x, 2);
    assertEqual(leftPositions.puyo1.y, 3);
    assertEqual(leftPositions.puyo2.x, 2);
    assertEqual(leftPositions.puyo2.y, 2);
    
    // Test potential move right
    const rightPositions = pair.getPotentialPositions('right');
    assertEqual(rightPositions.puyo1.x, 4);
    assertEqual(rightPositions.puyo1.y, 3);
    
    // Test potential move down
    const downPositions = pair.getPotentialPositions('down');
    assertEqual(downPositions.puyo1.x, 3);
    assertEqual(downPositions.puyo1.y, 4);
    assertEqual(downPositions.puyo2.x, 3);
    assertEqual(downPositions.puyo2.y, 3);
});

testRunner.test('should calculate potential rotation positions correctly', () => {
    const puyo1 = new Puyo('red');
    const puyo2 = new Puyo('blue');
    const pair = new PuyoPair(puyo1, puyo2);
    pair.setPosition(3, 3);
    
    // At rotation 0, after rotation should be at 90°
    const rotationPositions = pair.getPotentialRotationPositions();
    assertEqual(rotationPositions.puyo1.x, 3);
    assertEqual(rotationPositions.puyo1.y, 3);
    assertEqual(rotationPositions.puyo2.x, 4); // To the right
    assertEqual(rotationPositions.puyo2.y, 3);
    
    // Original pair should not be affected
    assertEqual(pair.getRotation(), 0);
});

// Clone and equality tests
testRunner.test('should clone pair correctly', () => {
    const puyo1 = new Puyo('green');
    const puyo2 = new Puyo('yellow');
    const original = new PuyoPair(puyo1, puyo2);
    original.setPosition(4, 5);
    original.setRotation(2);
    original.setFallSpeed(3);
    original.enableFastDrop();
    
    const cloned = original.clone();
    
    assertEqual(cloned.x, original.x);
    assertEqual(cloned.y, original.y);
    assertEqual(cloned.rotation, original.rotation);
    assertEqual(cloned.fallSpeed, original.fallSpeed);
    assertEqual(cloned.fastDrop, original.fastDrop);
    assertEqual(cloned.puyo1.color, original.puyo1.color);
    assertEqual(cloned.puyo2.color, original.puyo2.color);
    
    // Ensure they are different instances
    assert(cloned !== original);
    assert(cloned.puyo1 !== original.puyo1);
    assert(cloned.puyo2 !== original.puyo2);
});

testRunner.test('should check equality correctly', () => {
    const puyo1a = new Puyo('red', 2, 1);
    const puyo2a = new Puyo('blue', 2, 0);
    const pair1 = new PuyoPair(puyo1a, puyo2a);
    
    const puyo1b = new Puyo('red', 2, 1);
    const puyo2b = new Puyo('blue', 2, 0);
    const pair2 = new PuyoPair(puyo1b, puyo2b);
    
    const puyo1c = new Puyo('green', 2, 1);
    const puyo2c = new Puyo('blue', 2, 0);
    const pair3 = new PuyoPair(puyo1c, puyo2c);
    
    assert(pair1.equals(pair2));
    assert(!pair1.equals(pair3));
    assert(!pair1.equals(null));
    assert(!pair1.equals('not a pair'));
});

// String representation tests
testRunner.test('should generate correct string representation', () => {
    const puyo1 = new Puyo('purple');
    const puyo2 = new Puyo('red');
    const pair = new PuyoPair(puyo1, puyo2);
    pair.setPosition(3, 4);
    pair.setRotation(1);
    
    const str = pair.toString();
    assert(str.includes('purple'));
    assert(str.includes('red'));
    assert(str.includes('3'));
    assert(str.includes('4'));
    assert(str.includes('90'));
});

// JSON serialization tests
testRunner.test('should serialize to JSON correctly', () => {
    const puyo1 = new Puyo('blue');
    const puyo2 = new Puyo('green');
    const pair = new PuyoPair(puyo1, puyo2);
    pair.setPosition(5, 6);
    pair.setRotation(3);
    pair.setFallSpeed(2.5);
    pair.enableFastDrop();
    
    const json = pair.toJSON();
    
    assertEqual(json.x, 5);
    assertEqual(json.y, 6);
    assertEqual(json.rotation, 3);
    assertEqual(json.fallSpeed, 2.5);
    assertEqual(json.fastDrop, true);
    assertEqual(json.puyo1.color, 'blue');
    assertEqual(json.puyo2.color, 'green');
});

testRunner.test('should deserialize from JSON correctly', () => {
    const json = {
        puyo1: { color: 'yellow', x: 5, y: 6, state: 'falling', animationFrame: 0 },
        puyo2: { color: 'purple', x: 5, y: 5, state: 'falling', animationFrame: 0 },
        x: 5,
        y: 6,
        rotation: 2,
        fallSpeed: 3,
        fastDrop: true
    };
    
    const pair = PuyoPair.fromJSON(json);
    
    assertEqual(pair.x, 5);
    assertEqual(pair.y, 6);
    assertEqual(pair.rotation, 2);
    assertEqual(pair.fallSpeed, 3);
    assertEqual(pair.fastDrop, true);
    assertEqual(pair.puyo1.color, 'yellow');
    assertEqual(pair.puyo2.color, 'purple');
});

// Random pair creation tests
testRunner.test('should create random pair correctly', () => {
    const pair = PuyoPair.createRandom(4, 7);
    
    assertEqual(pair.x, 4);
    assertEqual(pair.y, 7);
    assertEqual(pair.rotation, 0);
    assert(Puyo.COLORS.includes(pair.puyo1.color));
    assert(Puyo.COLORS.includes(pair.puyo2.color));
});

testRunner.test('should create random pair with default position', () => {
    const pair = PuyoPair.createRandom();
    
    assertEqual(pair.x, 2);
    assertEqual(pair.y, 1);
});

// Constants tests
testRunner.test('should have correct rotation constants', () => {
    assertEqual(PuyoPair.ROTATIONS.DEGREE_0, 0);
    assertEqual(PuyoPair.ROTATIONS.DEGREE_90, 1);
    assertEqual(PuyoPair.ROTATIONS.DEGREE_180, 2);
    assertEqual(PuyoPair.ROTATIONS.DEGREE_270, 3);
});

testRunner.test('should have correct default fall speed', () => {
    assertEqual(PuyoPair.DEFAULT_FALL_SPEED, 1);
});

// Export the test runner for use in test HTML
export { testRunner as PuyoPairTestRunner };

// Auto-run tests if this module is loaded directly
if (typeof window !== 'undefined') {
    // Browser environment - make test runner available globally
    window.PuyoPairTestRunner = testRunner;
}