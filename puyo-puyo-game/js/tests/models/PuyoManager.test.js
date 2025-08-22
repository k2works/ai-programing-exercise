/**
 * Unit tests for PuyoManager class
 * Tests puyo lifecycle management, spawning, movement, and placement
 */

import { PuyoManager } from '../../models/PuyoManager.js';
import { FieldManager } from '../../models/FieldManager.js';
import { Puyo } from '../../models/Puyo.js';
import { PuyoPair } from '../../models/PuyoPair.js';

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
        console.log('Running PuyoManager tests...');
        
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

function assertNotEqual(actual, expected, message) {
    if (actual === expected) {
        throw new Error(message || `Expected not to equal ${expected}, but got ${actual}`);
    }
}

function assertInstanceOf(actual, expectedClass, message) {
    if (!(actual instanceof expectedClass)) {
        throw new Error(message || `Expected instance of ${expectedClass.name}, but got ${actual?.constructor?.name || typeof actual}`);
    }
}

function assertThrows(fn, message) {
    try {
        fn();
        throw new Error(message || 'Expected function to throw, but it did not');
    } catch (error) {
        if (error.message === (message || 'Expected function to throw, but it did not')) {
            throw error;
        }
        // Function threw as expected
    }
}

// Test runner instance
const runner = new TestRunner();

// Constructor tests
runner.test('should create PuyoManager with field manager', () => {
    const fieldManager = new FieldManager();
    const puyoManager = new PuyoManager(fieldManager);
    
    assertInstanceOf(puyoManager, PuyoManager);
    assertEqual(puyoManager.fieldManager, fieldManager);
});

runner.test('should throw error without field manager', () => {
    assertThrows(() => new PuyoManager(), 'FieldManager is required');
});

runner.test('should initialize with current and next pairs', () => {
    const fieldManager = new FieldManager();
    const puyoManager = new PuyoManager(fieldManager);
    
    assertInstanceOf(puyoManager.getCurrentPair(), PuyoPair);
    assertInstanceOf(puyoManager.getNextPair(), PuyoPair);
});

// createRandomPuyoPair tests
runner.test('should create puyo pair with random colors', () => {
    const fieldManager = new FieldManager();
    const puyoManager = new PuyoManager(fieldManager);
    const pair = puyoManager.createRandomPuyoPair();
    
    assertInstanceOf(pair, PuyoPair);
    assert(Puyo.COLORS.includes(pair.puyo1.color), `Expected valid color, got ${pair.puyo1.color}`);
    assert(Puyo.COLORS.includes(pair.puyo2.color), `Expected valid color, got ${pair.puyo2.color}`);
});

runner.test('should create pair at specified position', () => {
    const fieldManager = new FieldManager();
    const puyoManager = new PuyoManager(fieldManager);
    const pair = puyoManager.createRandomPuyoPair(3, 2);
    
    assertEqual(pair.x, 3);
    assertEqual(pair.y, 2);
});

runner.test('should create pair at default position', () => {
    const fieldManager = new FieldManager();
    const puyoManager = new PuyoManager(fieldManager);
    const pair = puyoManager.createRandomPuyoPair();
    
    assertEqual(pair.x, 2);
    assertEqual(pair.y, 1);
});

// spawnPuyoPair tests
runner.test('should move next pair to current and generate new next', () => {
    const fieldManager = new FieldManager();
    const puyoManager = new PuyoManager(fieldManager);
    const originalNext = puyoManager.getNextPair();
    const spawned = puyoManager.spawnPuyoPair();
    
    assertEqual(spawned, originalNext);
    assertEqual(puyoManager.getCurrentPair(), originalNext);
    assertNotEqual(puyoManager.getNextPair(), originalNext);
    assertInstanceOf(puyoManager.getNextPair(), PuyoPair);
});

runner.test('should reset spawned pair position', () => {
    const fieldManager = new FieldManager();
    const puyoManager = new PuyoManager(fieldManager);
    const spawned = puyoManager.spawnPuyoPair();
    
    assertEqual(spawned.x, 2);
    assertEqual(spawned.y, 1);
});

runner.test('should set spawned pair to falling state', () => {
    const fieldManager = new FieldManager();
    const puyoManager = new PuyoManager(fieldManager);
    const spawned = puyoManager.spawnPuyoPair();
    
    assert(spawned.isFalling(), 'Expected spawned pair to be falling');
});

// getCurrentPair and getNextPair tests
runner.test('should return current and next pairs', () => {
    const fieldManager = new FieldManager();
    const puyoManager = new PuyoManager(fieldManager);
    const current = puyoManager.getCurrentPair();
    const next = puyoManager.getNextPair();
    
    assertInstanceOf(current, PuyoPair);
    assertInstanceOf(next, PuyoPair);
    assertNotEqual(current, next);
});

// hasCurrentPair tests
runner.test('should return true when current pair exists', () => {
    const fieldManager = new FieldManager();
    const puyoManager = new PuyoManager(fieldManager);
    
    assertEqual(puyoManager.hasCurrentPair(), true);
});

runner.test('should return false when no current pair', () => {
    const fieldManager = new FieldManager();
    const puyoManager = new PuyoManager(fieldManager);
    puyoManager.currentPair = null;
    
    assertEqual(puyoManager.hasCurrentPair(), false);
});

// movePair tests
runner.test('should move pair left when valid', () => {
    const fieldManager = new FieldManager();
    const puyoManager = new PuyoManager(fieldManager);
    const initialX = puyoManager.getCurrentPair().x;
    const result = puyoManager.movePair('left');
    
    assertEqual(result, true);
    assertEqual(puyoManager.getCurrentPair().x, initialX - 1);
});

runner.test('should move pair right when valid', () => {
    const fieldManager = new FieldManager();
    const puyoManager = new PuyoManager(fieldManager);
    const initialX = puyoManager.getCurrentPair().x;
    const result = puyoManager.movePair('right');
    
    assertEqual(result, true);
    assertEqual(puyoManager.getCurrentPair().x, initialX + 1);
});

runner.test('should move pair down when valid', () => {
    const fieldManager = new FieldManager();
    const puyoManager = new PuyoManager(fieldManager);
    const initialY = puyoManager.getCurrentPair().y;
    const result = puyoManager.movePair('down');
    
    assertEqual(result, true);
    assertEqual(puyoManager.getCurrentPair().y, initialY + 1);
});

runner.test('should not move pair when invalid', () => {
    const fieldManager = new FieldManager();
    const puyoManager = new PuyoManager(fieldManager);
    // Move to left edge
    puyoManager.getCurrentPair().setPosition(0, 1);
    const result = puyoManager.movePair('left');
    
    assertEqual(result, false);
    assertEqual(puyoManager.getCurrentPair().x, 0);
});

runner.test('should return false when no current pair', () => {
    const fieldManager = new FieldManager();
    const puyoManager = new PuyoManager(fieldManager);
    puyoManager.currentPair = null;
    const result = puyoManager.movePair('left');
    
    assertEqual(result, false);
});

runner.test('should not move into occupied space', () => {
    const fieldManager = new FieldManager();
    const puyoManager = new PuyoManager(fieldManager);
    // Place a puyo in the field
    const blockingPuyo = new Puyo('red', 1, 1);
    fieldManager.setCell(1, 1, blockingPuyo);
    
    // Try to move current pair into that space
    puyoManager.getCurrentPair().setPosition(2, 1);
    const result = puyoManager.movePair('left');
    
    assertEqual(result, false);
    assertEqual(puyoManager.getCurrentPair().x, 2);
});

// rotatePair tests
runner.test('should rotate pair when valid', () => {
    const fieldManager = new FieldManager();
    const puyoManager = new PuyoManager(fieldManager);
    const initialRotation = puyoManager.getCurrentPair().getRotation();
    const result = puyoManager.rotatePair();
    
    assertEqual(result, true);
    assertEqual(puyoManager.getCurrentPair().getRotation(), (initialRotation + 1) % 4);
});

runner.test('should not rotate when invalid', () => {
    const fieldManager = new FieldManager();
    const puyoManager = new PuyoManager(fieldManager);
    // Fill field around current pair to prevent rotation
    const currentPair = puyoManager.getCurrentPair();
    currentPair.setPosition(2, 2);
    
    // Block rotation positions
    fieldManager.setCell(3, 2, new Puyo('red')); // Right
    fieldManager.setCell(1, 2, new Puyo('blue')); // Left
    fieldManager.setCell(2, 1, new Puyo('green')); // Up
    fieldManager.setCell(2, 3, new Puyo('yellow')); // Down
    
    const initialRotation = currentPair.getRotation();
    const result = puyoManager.rotatePair();
    
    assertEqual(result, false);
    assertEqual(currentPair.getRotation(), initialRotation);
});

runner.test('should perform wall kick when rotation blocked', () => {
    const fieldManager = new FieldManager();
    const puyoManager = new PuyoManager(fieldManager);
    // Position pair at right edge where normal rotation would fail
    const currentPair = puyoManager.getCurrentPair();
    currentPair.setPosition(5, 2);
    currentPair.setRotation(0); // Secondary puyo above
    
    // Rotation to right (90°) would put secondary puyo at x=6 (invalid)
    // Wall kick should move pair left and then rotate
    const result = puyoManager.rotatePair();
    
    assertEqual(result, true);
    assertEqual(currentPair.x, 4); // Moved left for wall kick
    assertEqual(currentPair.getRotation(), 1); // Rotated
});

runner.test('should return false when no current pair', () => {
    const fieldManager = new FieldManager();
    const puyoManager = new PuyoManager(fieldManager);
    puyoManager.currentPair = null;
    const result = puyoManager.rotatePair();
    
    assertEqual(result, false);
});

// dropPair and stopDrop tests
runner.test('should enable fast drop', () => {
    const fieldManager = new FieldManager();
    const puyoManager = new PuyoManager(fieldManager);
    const result = puyoManager.dropPair();
    
    assertEqual(result, true);
    assertEqual(puyoManager.getCurrentPair().isFastDropEnabled(), true);
});

runner.test('should disable fast drop', () => {
    const fieldManager = new FieldManager();
    const puyoManager = new PuyoManager(fieldManager);
    puyoManager.dropPair();
    const result = puyoManager.stopDrop();
    
    assertEqual(result, true);
    assertEqual(puyoManager.getCurrentPair().isFastDropEnabled(), false);
});

runner.test('should return false when no current pair', () => {
    const fieldManager = new FieldManager();
    const puyoManager = new PuyoManager(fieldManager);
    puyoManager.currentPair = null;
    
    assertEqual(puyoManager.dropPair(), false);
    assertEqual(puyoManager.stopDrop(), false);
});

// canPairFall tests
runner.test('should return true when pair can fall', () => {
    const fieldManager = new FieldManager();
    const puyoManager = new PuyoManager(fieldManager);
    
    assertEqual(puyoManager.canPairFall(), true);
});

runner.test('should return false when pair cannot fall', () => {
    const fieldManager = new FieldManager();
    const puyoManager = new PuyoManager(fieldManager);
    // Move pair to bottom
    const currentPair = puyoManager.getCurrentPair();
    currentPair.setPosition(2, 11);
    
    assertEqual(puyoManager.canPairFall(), false);
});

runner.test('should return false when no current pair', () => {
    const fieldManager = new FieldManager();
    const puyoManager = new PuyoManager(fieldManager);
    puyoManager.currentPair = null;
    
    assertEqual(puyoManager.canPairFall(), false);
});

runner.test('should return false when blocked by other puyos', () => {
    const fieldManager = new FieldManager();
    const puyoManager = new PuyoManager(fieldManager);
    // Place blocking puyo
    fieldManager.setCell(2, 3, new Puyo('red'));
    
    // Position current pair above blocking puyo
    puyoManager.getCurrentPair().setPosition(2, 2);
    
    assertEqual(puyoManager.canPairFall(), false);
});

// fallPair tests
runner.test('should make pair fall one step', () => {
    const fieldManager = new FieldManager();
    const puyoManager = new PuyoManager(fieldManager);
    const initialY = puyoManager.getCurrentPair().y;
    const result = puyoManager.fallPair();
    
    assertEqual(result, true);
    assertEqual(puyoManager.getCurrentPair().y, initialY + 1);
});

runner.test('should return false when pair cannot fall', () => {
    const fieldManager = new FieldManager();
    const puyoManager = new PuyoManager(fieldManager);
    // Move pair to bottom
    puyoManager.getCurrentPair().setPosition(2, 11);
    const result = puyoManager.fallPair();
    
    assertEqual(result, false);
});

// fixPairToField tests
runner.test('should fix pair to field and spawn next', () => {
    const fieldManager = new FieldManager();
    const puyoManager = new PuyoManager(fieldManager);
    const currentPair = puyoManager.getCurrentPair();
    const nextPair = puyoManager.getNextPair();
    
    // Position pair at bottom
    currentPair.setPosition(2, 11);
    
    const result = puyoManager.fixPairToField();
    
    assertEqual(result, true);
    assertEqual(puyoManager.getCurrentPair(), nextPair);
    assertNotEqual(puyoManager.getCurrentPair(), currentPair);
    
    // Check that puyos were placed in field
    assertEqual(fieldManager.getCell(2, 11), currentPair.puyo1);
    assertEqual(fieldManager.getCell(2, 10), currentPair.puyo2); // Secondary puyo above
});

runner.test('should set puyos to fixed state', () => {
    const fieldManager = new FieldManager();
    const puyoManager = new PuyoManager(fieldManager);
    const currentPair = puyoManager.getCurrentPair();
    currentPair.setPosition(2, 11);
    
    puyoManager.fixPairToField();
    
    assertEqual(currentPair.puyo1.isFixed(), true);
    assertEqual(currentPair.puyo2.isFixed(), true);
});

runner.test('should return false when no current pair', () => {
    const fieldManager = new FieldManager();
    const puyoManager = new PuyoManager(fieldManager);
    puyoManager.currentPair = null;
    const result = puyoManager.fixPairToField();
    
    assertEqual(result, false);
});

// isValidMove tests
runner.test('should return true for valid positions', () => {
    const fieldManager = new FieldManager();
    const puyoManager = new PuyoManager(fieldManager);
    const positions = {
        puyo1: { x: 2, y: 2 },
        puyo2: { x: 2, y: 1 }
    };
    
    assertEqual(puyoManager.isValidMove(positions), true);
});

runner.test('should return false for out of bounds positions', () => {
    const fieldManager = new FieldManager();
    const puyoManager = new PuyoManager(fieldManager);
    const positions = {
        puyo1: { x: -1, y: 2 },
        puyo2: { x: 2, y: 1 }
    };
    
    assertEqual(puyoManager.isValidMove(positions), false);
});

runner.test('should return false for occupied positions', () => {
    const fieldManager = new FieldManager();
    const puyoManager = new PuyoManager(fieldManager);
    fieldManager.setCell(2, 2, new Puyo('red'));
    
    const positions = {
        puyo1: { x: 2, y: 2 },
        puyo2: { x: 2, y: 1 }
    };
    
    assertEqual(puyoManager.isValidMove(positions), false);
});

// isGameOver tests
runner.test('should return false when spawn position is clear', () => {
    const fieldManager = new FieldManager();
    const puyoManager = new PuyoManager(fieldManager);
    
    assertEqual(puyoManager.isGameOver(), false);
});

runner.test('should return true when spawn position is blocked', () => {
    const fieldManager = new FieldManager();
    const puyoManager = new PuyoManager(fieldManager);
    // Block spawn position
    fieldManager.setCell(2, 1, new Puyo('red'));
    
    assertEqual(puyoManager.isGameOver(), true);
});

runner.test('should return false when no next pair', () => {
    const fieldManager = new FieldManager();
    const puyoManager = new PuyoManager(fieldManager);
    puyoManager.nextPair = null;
    
    assertEqual(puyoManager.isGameOver(), false);
});

// reset tests
runner.test('should reset manager to initial state', () => {
    const fieldManager = new FieldManager();
    const puyoManager = new PuyoManager(fieldManager);
    // Modify state
    puyoManager.currentPair = null;
    puyoManager.nextPair = null;
    
    puyoManager.reset();
    
    assertInstanceOf(puyoManager.getCurrentPair(), PuyoPair);
    assertInstanceOf(puyoManager.getNextPair(), PuyoPair);
});

// getState tests
runner.test('should return manager state information', () => {
    const fieldManager = new FieldManager();
    const puyoManager = new PuyoManager(fieldManager);
    const state = puyoManager.getState();
    
    assert(state.hasOwnProperty('hasCurrentPair'), 'State should have hasCurrentPair property');
    assert(state.hasOwnProperty('currentPairPosition'), 'State should have currentPairPosition property');
    assert(state.hasOwnProperty('currentPairRotation'), 'State should have currentPairRotation property');
    assert(state.hasOwnProperty('currentPairColors'), 'State should have currentPairColors property');
    assert(state.hasOwnProperty('nextPairColors'), 'State should have nextPairColors property');
    assert(state.hasOwnProperty('canFall'), 'State should have canFall property');
    assert(state.hasOwnProperty('isGameOver'), 'State should have isGameOver property');
    
    assertEqual(state.hasCurrentPair, true);
    assertEqual(state.currentPairPosition.x, 2);
    assertEqual(state.currentPairPosition.y, 1);
    assertEqual(state.currentPairRotation, 0);
});

runner.test('should handle null current pair', () => {
    const fieldManager = new FieldManager();
    const puyoManager = new PuyoManager(fieldManager);
    puyoManager.currentPair = null;
    const state = puyoManager.getState();
    
    assertEqual(state.hasCurrentPair, false);
    assertEqual(state.currentPairPosition, null);
    assertEqual(state.currentPairRotation, null);
    assertEqual(state.currentPairColors, null);
});

// JSON serialization tests
runner.test('should convert to JSON', () => {
    const fieldManager = new FieldManager();
    const puyoManager = new PuyoManager(fieldManager);
    const json = puyoManager.toJSON();
    
    assert(json.hasOwnProperty('currentPair'), 'JSON should have currentPair property');
    assert(json.hasOwnProperty('nextPair'), 'JSON should have nextPair property');
    assert(json.currentPair !== null, 'currentPair should not be null');
    assert(json.nextPair !== null, 'nextPair should not be null');
});

runner.test('should create from JSON', () => {
    const fieldManager = new FieldManager();
    const puyoManager = new PuyoManager(fieldManager);
    const json = puyoManager.toJSON();
    const newManager = PuyoManager.fromJSON(json, fieldManager);
    
    assertInstanceOf(newManager, PuyoManager);
    assertInstanceOf(newManager.getCurrentPair(), PuyoPair);
    assertInstanceOf(newManager.getNextPair(), PuyoPair);
});

runner.test('should handle null pairs in JSON', () => {
    const fieldManager = new FieldManager();
    const json = {
        currentPair: null,
        nextPair: null
    };
    
    const newManager = PuyoManager.fromJSON(json, fieldManager);
    
    assertEqual(newManager.getCurrentPair(), null);
    assertEqual(newManager.getNextPair(), null);
});

// Export the test runner
export const PuyoManagerTestRunner = runner;