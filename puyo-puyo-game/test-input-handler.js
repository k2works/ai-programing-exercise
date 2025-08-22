/**
 * Simple test script to verify InputHandler functionality
 * Can be run directly in Node.js environment
 */

// Mock DOM environment for Node.js
global.document = {
    addEventListener: function(event, handler) {
        console.log(`Mock: Added ${event} listener`);
    },
    getElementById: function(id) {
        if (id === 'game-canvas') {
            return {
                addEventListener: function(event, handler) {
                    console.log(`Mock: Added ${event} listener to canvas`);
                },
                getBoundingClientRect: function() {
                    return { left: 0, top: 0, width: 300, height: 600 };
                }
            };
        }
        return null;
    }
};

global.performance = {
    now: function() {
        return Date.now();
    }
};

// Import the InputHandler
import { InputHandler } from './js/input/InputHandler.js';

// Mock GameEngine
class MockGameEngine {
    constructor() {
        this.lastInput = null;
        this.inputHistory = [];
    }

    handleInput(action) {
        this.lastInput = action;
        this.inputHistory.push(action);
        console.log('Game received input:', action);
    }
}

// Test the InputHandler
console.log('='.repeat(50));
console.log('Testing InputHandler');
console.log('='.repeat(50));

try {
    const mockEngine = new MockGameEngine();
    const inputHandler = new InputHandler(mockEngine);
    
    console.log('✓ InputHandler created successfully');
    
    // Test key mapping
    console.log('\nTesting key mapping:');
    const testKeys = [
        { key: 'ArrowLeft', expected: { type: 'move', direction: 'left' } },
        { key: 'ArrowRight', expected: { type: 'move', direction: 'right' } },
        { key: 'ArrowDown', expected: { type: 'move', direction: 'down' } },
        { key: 'ArrowUp', expected: { type: 'rotate' } },
        { key: 'Space', expected: { type: 'rotate' } },
        { key: 'KeyP', expected: { type: 'pause' } },
        { key: 'KeyR', expected: { type: 'restart' } }
    ];
    
    testKeys.forEach(test => {
        const action = inputHandler.keyToAction(test.key);
        const matches = JSON.stringify(action) === JSON.stringify(test.expected);
        console.log(`${matches ? '✓' : '✗'} ${test.key} -> ${JSON.stringify(action)}`);
    });
    
    // Test game key detection
    console.log('\nTesting game key detection:');
    const gameKeys = ['ArrowLeft', 'ArrowRight', 'ArrowUp', 'ArrowDown', 'Space', 'KeyP', 'KeyR'];
    const nonGameKeys = ['KeyA', 'Digit1', 'Enter'];
    
    gameKeys.forEach(key => {
        const isGame = inputHandler.isGameKey(key);
        console.log(`${isGame ? '✓' : '✗'} ${key} is ${isGame ? '' : 'not '}a game key`);
    });
    
    nonGameKeys.forEach(key => {
        const isGame = inputHandler.isGameKey(key);
        console.log(`${!isGame ? '✓' : '✗'} ${key} is ${isGame ? '' : 'not '}a game key`);
    });
    
    // Test swipe detection
    console.log('\nTesting swipe detection:');
    const swipeTests = [
        { deltaX: 50, deltaY: 10, expected: { type: 'move', direction: 'right' } },
        { deltaX: -50, deltaY: 10, expected: { type: 'move', direction: 'left' } },
        { deltaX: 10, deltaY: 50, expected: { type: 'move', direction: 'down' } },
        { deltaX: 10, deltaY: -50, expected: { type: 'rotate' } }
    ];
    
    swipeTests.forEach(test => {
        const action = inputHandler.getSwipeAction(test.deltaX, test.deltaY);
        const matches = JSON.stringify(action) === JSON.stringify(test.expected);
        console.log(`${matches ? '✓' : '✗'} Swipe (${test.deltaX}, ${test.deltaY}) -> ${JSON.stringify(action)}`);
    });
    
    // Test tap zone detection
    console.log('\nTesting tap zone detection:');
    const tapTests = [
        { x: 50, y: 300, expected: { type: 'move', direction: 'left' } },   // Left zone
        { x: 250, y: 300, expected: { type: 'move', direction: 'right' } }, // Right zone
        { x: 150, y: 300, expected: { type: 'rotate' } }                   // Center zone
    ];
    
    tapTests.forEach(test => {
        const touch = { clientX: test.x, clientY: test.y };
        const action = inputHandler.getTapAction(touch);
        const matches = JSON.stringify(action) === JSON.stringify(test.expected);
        console.log(`${matches ? '✓' : '✗'} Tap at (${test.x}, ${test.y}) -> ${JSON.stringify(action)}`);
    });
    
    console.log('\n' + '='.repeat(50));
    console.log('InputHandler tests completed successfully!');
    console.log('='.repeat(50));
    
} catch (error) {
    console.error('✗ InputHandler test failed:', error.message);
    console.error(error.stack);
}