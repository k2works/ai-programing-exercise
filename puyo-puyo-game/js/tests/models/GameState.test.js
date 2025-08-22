/**
 * Unit tests for GameState class
 * Tests game state management, field operations, score tracking, and state transitions
 */

import { GameState } from '../../models/GameState.js';
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
        console.log('Running GameState tests...');
        
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

// Constructor and initialization tests
testRunner.test('should initialize with default values', () => {
    const gameState = new GameState();
    
    assertEqual(gameState.getScore(), 0);
    assertEqual(gameState.getLevel(), 1);
    assertEqual(gameState.getStatus(), GameState.STATUS.PLAYING);
    assertEqual(gameState.getChainCount(), 0);
    assertEqual(gameState.getLastClearTime(), 0);
    assertEqual(gameState.getCurrentPair(), null);
    assertEqual(gameState.getNextPair(), null);
    assertEqual(gameState.getLinesCleared(), 0);
    assertEqual(gameState.getTotalPuyosCleared(), 0);
    assertEqual(gameState.getHighestChain(), 0);
    assertEqual(gameState.getZenkeshiCount(), 0);
});

testRunner.test('should initialize field with correct dimensions', () => {
    const gameState = new GameState();
    const field = gameState.getField();
    
    assertEqual(field.length, GameState.FIELD_HEIGHT);
    assertEqual(field[0].length, GameState.FIELD_WIDTH);
    
    // Check that all cells are initially null
    for (let y = 0; y < GameState.FIELD_HEIGHT; y++) {
        for (let x = 0; x < GameState.FIELD_WIDTH; x++) {
            assertEqual(gameState.getFieldCell(x, y), null);
        }
    }
});

testRunner.test('should reset to initial state', () => {
    const gameState = new GameState();
    
    // Modify state
    gameState.setScore(1000);
    gameState.setLevel(5);
    gameState.setStatus(GameState.STATUS.GAME_OVER);
    gameState.setChainCount(10);
    
    // Reset
    gameState.reset();
    
    // Check that values are back to initial state
    assertEqual(gameState.getScore(), 0);
    assertEqual(gameState.getLevel(), 1);
    assertEqual(gameState.getStatus(), GameState.STATUS.PLAYING);
    assertEqual(gameState.getChainCount(), 0);
});

// Status management tests
testRunner.test('should manage game status correctly', () => {
    const gameState = new GameState();
    
    // Initially playing
    assert(gameState.isPlaying());
    assert(!gameState.isPaused());
    assert(!gameState.isGameOver());
    
    // Pause
    gameState.pause();
    assertEqual(gameState.getStatus(), GameState.STATUS.PAUSED);
    assert(!gameState.isPlaying());
    assert(gameState.isPaused());
    assert(!gameState.isGameOver());
    
    // Resume
    gameState.resume();
    assertEqual(gameState.getStatus(), GameState.STATUS.PLAYING);
    assert(gameState.isPlaying());
    assert(!gameState.isPaused());
    assert(!gameState.isGameOver());
    
    // Game over
    gameState.gameOver();
    assertEqual(gameState.getStatus(), GameState.STATUS.GAME_OVER);
    assert(!gameState.isPlaying());
    assert(!gameState.isPaused());
    assert(gameState.isGameOver());
});

testRunner.test('should throw error for invalid status', () => {
    const gameState = new GameState();
    
    assertThrows(() => gameState.setStatus('invalid'), 'Invalid game status');
    assertThrows(() => gameState.setStatus(''), 'Invalid game status');
    assertThrows(() => gameState.setStatus(null), 'Invalid game status');
});

testRunner.test('should start new game correctly', () => {
    const gameState = new GameState();
    
    // Modify state
    gameState.setScore(1000);
    gameState.setStatus(GameState.STATUS.GAME_OVER);
    
    // Start new game
    gameState.startNewGame();
    
    assertEqual(gameState.getScore(), 0);
    assertEqual(gameState.getStatus(), GameState.STATUS.PLAYING);
});

// Field management tests
testRunner.test('should set and get field cells correctly', () => {
    const gameState = new GameState();
    const puyo = new Puyo('red', 2, 3);
    
    gameState.setFieldCell(2, 3, puyo);
    const retrievedPuyo = gameState.getFieldCell(2, 3);
    
    assertEqual(retrievedPuyo, puyo);
    assertEqual(retrievedPuyo.color, 'red');
});

testRunner.test('should validate field coordinates', () => {
    const gameState = new GameState();
    
    // Valid coordinates
    assert(gameState.isValidFieldPosition(0, 0));
    assert(gameState.isValidFieldPosition(5, 11));
    assert(gameState.isValidFieldPosition(2, 6));
    
    // Invalid coordinates
    assert(!gameState.isValidFieldPosition(-1, 0));
    assert(!gameState.isValidFieldPosition(0, -1));
    assert(!gameState.isValidFieldPosition(6, 0));
    assert(!gameState.isValidFieldPosition(0, 12));
    assert(!gameState.isValidFieldPosition(1.5, 0));
    assert(!gameState.isValidFieldPosition(0, 2.7));
});

testRunner.test('should throw error for invalid field coordinates', () => {
    const gameState = new GameState();
    const puyo = new Puyo('blue');
    
    assertThrows(() => gameState.getFieldCell(-1, 0), 'Invalid field coordinates');
    assertThrows(() => gameState.getFieldCell(0, -1), 'Invalid field coordinates');
    assertThrows(() => gameState.getFieldCell(6, 0), 'Invalid field coordinates');
    assertThrows(() => gameState.getFieldCell(0, 12), 'Invalid field coordinates');
    assertThrows(() => gameState.setFieldCell(-1, 0, puyo), 'Invalid field coordinates');
    assertThrows(() => gameState.setFieldCell(6, 0, puyo), 'Invalid field coordinates');
});

testRunner.test('should check if field position is empty', () => {
    const gameState = new GameState();
    const puyo = new Puyo('green');
    
    // Initially all positions should be empty
    assert(gameState.isFieldPositionEmpty(2, 3));
    
    // Place a puyo
    gameState.setFieldCell(2, 3, puyo);
    assert(!gameState.isFieldPositionEmpty(2, 3));
    
    // Clear the position
    gameState.clearFieldPosition(2, 3);
    assert(gameState.isFieldPositionEmpty(2, 3));
    assertEqual(gameState.getFieldCell(2, 3), null);
});

testRunner.test('should return false for empty check on invalid coordinates', () => {
    const gameState = new GameState();
    
    assert(!gameState.isFieldPositionEmpty(-1, 0));
    assert(!gameState.isFieldPositionEmpty(6, 0));
    assert(!gameState.isFieldPositionEmpty(0, 12));
});

testRunner.test('should get field copy', () => {
    const gameState = new GameState();
    const puyo = new Puyo('yellow');
    
    gameState.setFieldCell(1, 2, puyo);
    const fieldCopy = gameState.getField();
    
    // Should be a copy, not the same reference
    assert(fieldCopy !== gameState.field);
    assertEqual(fieldCopy[2][1], puyo);
    
    // Modifying copy should not affect original
    fieldCopy[2][1] = null;
    assertEqual(gameState.getFieldCell(1, 2), puyo);
});

// Puyo pair management tests
testRunner.test('should set and get current pair', () => {
    const gameState = new GameState();
    const puyo1 = new Puyo('red');
    const puyo2 = new Puyo('blue');
    const pair = new PuyoPair(puyo1, puyo2);
    
    gameState.setCurrentPair(pair);
    assertEqual(gameState.getCurrentPair(), pair);
    
    gameState.setCurrentPair(null);
    assertEqual(gameState.getCurrentPair(), null);
});

testRunner.test('should set and get next pair', () => {
    const gameState = new GameState();
    const puyo1 = new Puyo('green');
    const puyo2 = new Puyo('yellow');
    const pair = new PuyoPair(puyo1, puyo2);
    
    gameState.setNextPair(pair);
    assertEqual(gameState.getNextPair(), pair);
    
    gameState.setNextPair(null);
    assertEqual(gameState.getNextPair(), null);
});

testRunner.test('should throw error for invalid puyo pairs', () => {
    const gameState = new GameState();
    
    assertThrows(() => gameState.setCurrentPair('not a pair'), 'Current pair must be a PuyoPair instance or null');
    assertThrows(() => gameState.setNextPair('not a pair'), 'Next pair must be a PuyoPair instance or null');
    assertThrows(() => gameState.setCurrentPair(123), 'Current pair must be a PuyoPair instance or null');
    assertThrows(() => gameState.setNextPair(123), 'Next pair must be a PuyoPair instance or null');
});

// Score management tests
testRunner.test('should manage score correctly', () => {
    const gameState = new GameState();
    
    assertEqual(gameState.getScore(), 0);
    
    gameState.addScore(100);
    assertEqual(gameState.getScore(), 100);
    
    gameState.addScore(250);
    assertEqual(gameState.getScore(), 350);
    
    gameState.setScore(1000);
    assertEqual(gameState.getScore(), 1000);
});

testRunner.test('should throw error for invalid score values', () => {
    const gameState = new GameState();
    
    assertThrows(() => gameState.addScore(-10), 'Points must be a non-negative number');
    assertThrows(() => gameState.addScore('invalid'), 'Points must be a non-negative number');
    assertThrows(() => gameState.setScore(-100), 'Score must be a non-negative number');
    assertThrows(() => gameState.setScore('invalid'), 'Score must be a non-negative number');
});

// Level management tests
testRunner.test('should manage level correctly', () => {
    const gameState = new GameState();
    
    assertEqual(gameState.getLevel(), 1);
    
    gameState.setLevel(5);
    assertEqual(gameState.getLevel(), 5);
    
    gameState.levelUp();
    assertEqual(gameState.getLevel(), 6);
});

testRunner.test('should throw error for invalid level values', () => {
    const gameState = new GameState();
    
    assertThrows(() => gameState.setLevel(0), 'Level must be a positive number');
    assertThrows(() => gameState.setLevel(-1), 'Level must be a positive number');
    assertThrows(() => gameState.setLevel('invalid'), 'Level must be a positive number');
});

// Chain management tests
testRunner.test('should manage chain count correctly', () => {
    const gameState = new GameState();
    
    assertEqual(gameState.getChainCount(), 0);
    assertEqual(gameState.getHighestChain(), 0);
    
    gameState.setChainCount(5);
    assertEqual(gameState.getChainCount(), 5);
    assertEqual(gameState.getHighestChain(), 5);
    
    gameState.setChainCount(3);
    assertEqual(gameState.getChainCount(), 3);
    assertEqual(gameState.getHighestChain(), 5); // Should remain highest
    
    gameState.setChainCount(8);
    assertEqual(gameState.getChainCount(), 8);
    assertEqual(gameState.getHighestChain(), 8); // New highest
    
    gameState.resetChainCount();
    assertEqual(gameState.getChainCount(), 0);
    assertEqual(gameState.getHighestChain(), 8); // Highest preserved
});

testRunner.test('should throw error for invalid chain count', () => {
    const gameState = new GameState();
    
    assertThrows(() => gameState.setChainCount(-1), 'Chain count must be a non-negative number');
    assertThrows(() => gameState.setChainCount('invalid'), 'Chain count must be a non-negative number');
});

// Time management tests
testRunner.test('should manage last clear time', () => {
    const gameState = new GameState();
    const timestamp = Date.now();
    
    assertEqual(gameState.getLastClearTime(), 0);
    
    gameState.setLastClearTime(timestamp);
    assertEqual(gameState.getLastClearTime(), timestamp);
});

testRunner.test('should throw error for invalid timestamp', () => {
    const gameState = new GameState();
    
    assertThrows(() => gameState.setLastClearTime('invalid'), 'Timestamp must be a number');
    assertThrows(() => gameState.setLastClearTime(null), 'Timestamp must be a number');
});

// Statistics tests
testRunner.test('should track lines and puyos cleared', () => {
    const gameState = new GameState();
    
    assertEqual(gameState.getLinesCleared(), 0);
    assertEqual(gameState.getTotalPuyosCleared(), 0);
    
    gameState.addLinesCleared(2);
    gameState.addPuyosCleared(8);
    
    assertEqual(gameState.getLinesCleared(), 2);
    assertEqual(gameState.getTotalPuyosCleared(), 8);
    
    gameState.addLinesCleared(1);
    gameState.addPuyosCleared(4);
    
    assertEqual(gameState.getLinesCleared(), 3);
    assertEqual(gameState.getTotalPuyosCleared(), 12);
});

testRunner.test('should throw error for invalid cleared counts', () => {
    const gameState = new GameState();
    
    assertThrows(() => gameState.addLinesCleared(-1), 'Lines must be a non-negative number');
    assertThrows(() => gameState.addLinesCleared('invalid'), 'Lines must be a non-negative number');
    assertThrows(() => gameState.addPuyosCleared(-1), 'Puyos must be a non-negative number');
    assertThrows(() => gameState.addPuyosCleared('invalid'), 'Puyos must be a non-negative number');
});

testRunner.test('should track zenkeshi count', () => {
    const gameState = new GameState();
    
    assertEqual(gameState.getZenkeshiCount(), 0);
    
    gameState.incrementZenkeshiCount();
    assertEqual(gameState.getZenkeshiCount(), 1);
    
    gameState.incrementZenkeshiCount();
    assertEqual(gameState.getZenkeshiCount(), 2);
});

testRunner.test('should get game statistics', () => {
    const gameState = new GameState();
    
    gameState.setScore(1500);
    gameState.setLevel(3);
    gameState.setChainCount(7);
    gameState.addLinesCleared(5);
    gameState.addPuyosCleared(20);
    gameState.incrementZenkeshiCount();
    
    const stats = gameState.getStatistics();
    
    assertEqual(stats.score, 1500);
    assertEqual(stats.level, 3);
    assertEqual(stats.chainCount, 7);
    assertEqual(stats.highestChain, 7);
    assertEqual(stats.linesCleared, 5);
    assertEqual(stats.totalPuyosCleared, 20);
    assertEqual(stats.zenkeshiCount, 1);
    assertEqual(stats.gameStatus, GameState.STATUS.PLAYING);
    assert(typeof stats.gameDuration === 'number');
});

// Field state tests
testRunner.test('should check if field is empty', () => {
    const gameState = new GameState();
    
    // Initially empty
    assert(gameState.isFieldEmpty());
    
    // Add a puyo
    const puyo = new Puyo('red');
    gameState.setFieldCell(2, 5, puyo);
    assert(!gameState.isFieldEmpty());
    
    // Clear the puyo
    gameState.clearFieldPosition(2, 5);
    assert(gameState.isFieldEmpty());
});

testRunner.test('should check if top row is occupied', () => {
    const gameState = new GameState();
    
    // Initially not occupied
    assert(!gameState.isTopRowOccupied());
    
    // Add puyo to top row
    const puyo = new Puyo('blue');
    gameState.setFieldCell(3, 0, puyo);
    assert(gameState.isTopRowOccupied());
    
    // Clear top row
    gameState.clearFieldPosition(3, 0);
    assert(!gameState.isTopRowOccupied());
    
    // Add puyo to non-top row
    gameState.setFieldCell(3, 5, puyo);
    assert(!gameState.isTopRowOccupied());
});

// Clone and serialization tests
testRunner.test('should clone game state correctly', () => {
    const original = new GameState();
    
    // Set up original state
    original.setScore(2000);
    original.setLevel(4);
    original.setChainCount(6);
    original.setStatus(GameState.STATUS.PAUSED);
    
    const puyo = new Puyo('green');
    original.setFieldCell(1, 2, puyo);
    
    const puyo1 = new Puyo('red');
    const puyo2 = new Puyo('blue');
    const pair = new PuyoPair(puyo1, puyo2);
    original.setCurrentPair(pair);
    
    // Clone
    const cloned = original.clone();
    
    // Check that values are copied
    assertEqual(cloned.getScore(), 2000);
    assertEqual(cloned.getLevel(), 4);
    assertEqual(cloned.getChainCount(), 6);
    assertEqual(cloned.getStatus(), GameState.STATUS.PAUSED);
    assertEqual(cloned.getFieldCell(1, 2).color, 'green');
    assertEqual(cloned.getCurrentPair().puyo1.color, 'red');
    
    // Check that they are different instances
    assert(cloned !== original);
    assert(cloned.field !== original.field);
    assert(cloned.getFieldCell(1, 2) !== original.getFieldCell(1, 2));
    assert(cloned.getCurrentPair() !== original.getCurrentPair());
});

testRunner.test('should serialize to JSON correctly', () => {
    const gameState = new GameState();
    
    gameState.setScore(1500);
    gameState.setLevel(3);
    gameState.setChainCount(5);
    
    const puyo = new Puyo('purple');
    gameState.setFieldCell(2, 3, puyo);
    
    const json = gameState.toJSON();
    
    assertEqual(json.score, 1500);
    assertEqual(json.level, 3);
    assertEqual(json.chainCount, 5);
    assertEqual(json.gameStatus, GameState.STATUS.PLAYING);
    assertEqual(json.field[3][2].color, 'purple');
    assert(Array.isArray(json.field));
    assertEqual(json.field.length, GameState.FIELD_HEIGHT);
    assertEqual(json.field[0].length, GameState.FIELD_WIDTH);
});

testRunner.test('should deserialize from JSON correctly', () => {
    const json = {
        field: new Array(GameState.FIELD_HEIGHT).fill(null).map(() => 
            new Array(GameState.FIELD_WIDTH).fill(null)
        ),
        score: 2500,
        level: 5,
        currentPair: null,
        nextPair: null,
        gameStatus: GameState.STATUS.PAUSED,
        chainCount: 8,
        lastClearTime: 12345,
        linesCleared: 10,
        totalPuyosCleared: 40,
        gameStartTime: Date.now() - 60000,
        gameDuration: 0,
        highestChain: 8,
        zenkeshiCount: 2
    };
    
    // Add a puyo to the field
    json.field[4][1] = { color: 'yellow', x: 1, y: 4, state: 'fixed', animationFrame: 0 };
    
    const gameState = GameState.fromJSON(json);
    
    assertEqual(gameState.getScore(), 2500);
    assertEqual(gameState.getLevel(), 5);
    assertEqual(gameState.getStatus(), GameState.STATUS.PAUSED);
    assertEqual(gameState.getChainCount(), 8);
    assertEqual(gameState.getLastClearTime(), 12345);
    assertEqual(gameState.getLinesCleared(), 10);
    assertEqual(gameState.getTotalPuyosCleared(), 40);
    assertEqual(gameState.getHighestChain(), 8);
    assertEqual(gameState.getZenkeshiCount(), 2);
    assertEqual(gameState.getFieldCell(1, 4).color, 'yellow');
});

// Constants tests
testRunner.test('should have correct status constants', () => {
    assertEqual(GameState.STATUS.PLAYING, 'playing');
    assertEqual(GameState.STATUS.PAUSED, 'paused');
    assertEqual(GameState.STATUS.GAME_OVER, 'gameOver');
});

testRunner.test('should have correct field dimension constants', () => {
    assertEqual(GameState.FIELD_WIDTH, 6);
    assertEqual(GameState.FIELD_HEIGHT, 12);
});

// Export the test runner for use in test HTML
export { testRunner as GameStateTestRunner };

// Auto-run tests if this module is loaded directly
if (typeof window !== 'undefined') {
    // Browser environment - make test runner available globally
    window.GameStateTestRunner = testRunner;
}