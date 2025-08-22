/**
 * Unit tests for InputHandler class
 * Tests keyboard input handling, touch input processing, key mapping, and input validation
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
        console.log('Running InputHandler tests...');
        
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

// Mock DOM elements for testing
function createMockCanvas() {
    return {
        id: 'game-canvas',
        addEventListener: function(event, handler) {
            this[`_${event}`] = handler;
        },
        getBoundingClientRect: function() {
            return {
                left: 0,
                top: 0,
                width: 300,
                height: 600
            };
        }
    };
}

// Create test runner instance
const testRunner = new TestRunner();

// Constructor and initialization tests
testRunner.test('should create InputHandler with game engine', () => {
    const mockEngine = new MockGameEngine();
    const inputHandler = new InputHandler(mockEngine);
    
    assert(inputHandler.gameEngine === mockEngine);
    assert(inputHandler.keyStates instanceof Map);
    assert(inputHandler.touchStates instanceof Map);
    assertEqual(inputHandler.inputDelay, 100);
});

// Key mapping tests
testRunner.test('should map arrow keys to correct actions', () => {
    const mockEngine = new MockGameEngine();
    const inputHandler = new InputHandler(mockEngine);
    
    // Test left arrow
    const leftAction = inputHandler.keyToAction('ArrowLeft');
    assertDeepEqual(leftAction, { type: 'move', direction: 'left' });
    
    // Test right arrow
    const rightAction = inputHandler.keyToAction('ArrowRight');
    assertDeepEqual(rightAction, { type: 'move', direction: 'right' });
    
    // Test down arrow
    const downAction = inputHandler.keyToAction('ArrowDown');
    assertDeepEqual(downAction, { type: 'move', direction: 'down' });
    
    // Test up arrow
    const upAction = inputHandler.keyToAction('ArrowUp');
    assertDeepEqual(upAction, { type: 'rotate' });
});

testRunner.test('should map space key to rotate action', () => {
    const mockEngine = new MockGameEngine();
    const inputHandler = new InputHandler(mockEngine);
    
    const spaceAction = inputHandler.keyToAction('Space');
    assertDeepEqual(spaceAction, { type: 'rotate' });
});

testRunner.test('should map control keys correctly', () => {
    const mockEngine = new MockGameEngine();
    const inputHandler = new InputHandler(mockEngine);
    
    // Test pause key
    const pauseAction = inputHandler.keyToAction('KeyP');
    assertDeepEqual(pauseAction, { type: 'pause' });
    
    // Test restart key
    const restartAction = inputHandler.keyToAction('KeyR');
    assertDeepEqual(restartAction, { type: 'restart' });
});

testRunner.test('should return null for unmapped keys', () => {
    const mockEngine = new MockGameEngine();
    const inputHandler = new InputHandler(mockEngine);
    
    const unknownAction = inputHandler.keyToAction('KeyA');
    assertEqual(unknownAction, null);
    
    const unknownAction2 = inputHandler.keyToAction('Digit1');
    assertEqual(unknownAction2, null);
});

// Game key detection tests
testRunner.test('should identify game keys correctly', () => {
    const mockEngine = new MockGameEngine();
    const inputHandler = new InputHandler(mockEngine);
    
    // Test game keys
    assert(inputHandler.isGameKey('ArrowLeft'));
    assert(inputHandler.isGameKey('ArrowRight'));
    assert(inputHandler.isGameKey('ArrowUp'));
    assert(inputHandler.isGameKey('ArrowDown'));
    assert(inputHandler.isGameKey('Space'));
    assert(inputHandler.isGameKey('KeyP'));
    assert(inputHandler.isGameKey('KeyR'));
    
    // Test non-game keys
    assert(!inputHandler.isGameKey('KeyA'));
    assert(!inputHandler.isGameKey('Digit1'));
    assert(!inputHandler.isGameKey('Enter'));
});

// Key state management tests
testRunner.test('should track key states correctly', () => {
    const mockEngine = new MockGameEngine();
    const inputHandler = new InputHandler(mockEngine);
    
    // Initially no keys pressed
    assert(!inputHandler.isKeyPressed('ArrowLeft'));
    
    // Simulate key down
    const keyDownEvent = { code: 'ArrowLeft' };
    inputHandler.handleKeyDown(keyDownEvent);
    
    assert(inputHandler.isKeyPressed('ArrowLeft'));
    
    // Simulate key up
    const keyUpEvent = { code: 'ArrowLeft' };
    inputHandler.handleKeyUp(keyUpEvent);
    
    assert(!inputHandler.isKeyPressed('ArrowLeft'));
});

// Touch zone detection tests
testRunner.test('should detect left tap zone correctly', () => {
    const mockEngine = new MockGameEngine();
    const inputHandler = new InputHandler(mockEngine);
    
    // Mock canvas element
    const mockCanvas = createMockCanvas();
    document.getElementById = () => mockCanvas;
    
    // Create touch in left zone (x < 30% of width)
    const leftTouch = {
        clientX: 50,  // 50 < 90 (30% of 300)
        clientY: 300
    };
    
    const action = inputHandler.getTapAction(leftTouch);
    assertDeepEqual(action, { type: 'move', direction: 'left' });
});

testRunner.test('should detect right tap zone correctly', () => {
    const mockEngine = new MockGameEngine();
    const inputHandler = new InputHandler(mockEngine);
    
    // Mock canvas element
    const mockCanvas = createMockCanvas();
    document.getElementById = () => mockCanvas;
    
    // Create touch in right zone (x > 70% of width)
    const rightTouch = {
        clientX: 250,  // 250 > 210 (70% of 300)
        clientY: 300
    };
    
    const action = inputHandler.getTapAction(rightTouch);
    assertDeepEqual(action, { type: 'move', direction: 'right' });
});

testRunner.test('should detect center tap zone as rotate', () => {
    const mockEngine = new MockGameEngine();
    const inputHandler = new InputHandler(mockEngine);
    
    // Mock canvas element
    const mockCanvas = createMockCanvas();
    document.getElementById = () => mockCanvas;
    
    // Create touch in center zone
    const centerTouch = {
        clientX: 150,  // Between 90 and 210
        clientY: 300
    };
    
    const action = inputHandler.getTapAction(centerTouch);
    assertDeepEqual(action, { type: 'rotate' });
});

// Swipe detection tests
testRunner.test('should detect horizontal swipes correctly', () => {
    const mockEngine = new MockGameEngine();
    const inputHandler = new InputHandler(mockEngine);
    
    // Test right swipe
    const rightSwipeAction = inputHandler.getSwipeAction(50, 10);
    assertDeepEqual(rightSwipeAction, { type: 'move', direction: 'right' });
    
    // Test left swipe
    const leftSwipeAction = inputHandler.getSwipeAction(-50, 10);
    assertDeepEqual(leftSwipeAction, { type: 'move', direction: 'left' });
});

testRunner.test('should detect vertical swipes correctly', () => {
    const mockEngine = new MockGameEngine();
    const inputHandler = new InputHandler(mockEngine);
    
    // Test down swipe
    const downSwipeAction = inputHandler.getSwipeAction(10, 50);
    assertDeepEqual(downSwipeAction, { type: 'move', direction: 'down' });
    
    // Test up swipe (should be rotate)
    const upSwipeAction = inputHandler.getSwipeAction(10, -50);
    assertDeepEqual(upSwipeAction, { type: 'rotate' });
});

// Touch gesture recognition tests
testRunner.test('should recognize tap vs swipe based on distance and time', () => {
    const mockEngine = new MockGameEngine();
    const inputHandler = new InputHandler(mockEngine);
    
    // Mock canvas element
    const mockCanvas = createMockCanvas();
    document.getElementById = () => mockCanvas;
    
    // Test tap (short distance, short time)
    const tapState = {
        startX: 100,
        startY: 300,
        currentX: 105,  // Small movement
        currentY: 305,
        startTime: performance.now() - 100  // Short time
    };
    
    const tapTouch = { clientX: 105, clientY: 305 };
    const tapAction = inputHandler.touchToAction(tapState, tapTouch);
    
    // Should be a rotate action (center tap)
    assertDeepEqual(tapAction, { type: 'rotate' });
    
    // Test swipe (long distance)
    const swipeState = {
        startX: 100,
        startY: 300,
        currentX: 150,  // Large movement
        currentY: 305,
        startTime: performance.now() - 100
    };
    
    const swipeTouch = { clientX: 150, clientY: 305 };
    const swipeAction = inputHandler.touchToAction(swipeState, swipeTouch);
    
    // Should be a right move action
    assertDeepEqual(swipeAction, { type: 'move', direction: 'right' });
});

// Input state management tests
testRunner.test('should clear input states correctly', () => {
    const mockEngine = new MockGameEngine();
    const inputHandler = new InputHandler(mockEngine);
    
    // Set some key states
    inputHandler.keyStates.set('ArrowLeft', true);
    inputHandler.keyStates.set('Space', true);
    
    // Set some touch states
    inputHandler.touchStates.set(1, { startX: 100, startY: 200 });
    
    // Clear states
    inputHandler.clearInputStates();
    
    assertEqual(inputHandler.keyStates.size, 0);
    assertEqual(inputHandler.touchStates.size, 0);
});

// Input timing and repeat tests
testRunner.test('should handle key repeat with proper timing', () => {
    const mockEngine = new MockGameEngine();
    const inputHandler = new InputHandler(mockEngine);
    
    // First key press should work
    const keyEvent = { code: 'ArrowLeft' };
    inputHandler.handleKeyDown(keyEvent);
    
    assertEqual(mockEngine.getInputHistory().length, 1);
    assertDeepEqual(mockEngine.getLastInput(), { type: 'move', direction: 'left' });
    
    // Immediate repeat should be ignored (within delay)
    inputHandler.handleKeyDown(keyEvent);
    assertEqual(mockEngine.getInputHistory().length, 1);
    
    // Simulate time passing
    inputHandler.lastInputTime = performance.now() - 200; // Beyond delay
    inputHandler.handleKeyDown(keyEvent);
    
    assertEqual(mockEngine.getInputHistory().length, 2);
});

// Integration tests
testRunner.test('should handle complete keyboard input flow', () => {
    const mockEngine = new MockGameEngine();
    const inputHandler = new InputHandler(mockEngine);
    
    // Test sequence of inputs
    const inputs = [
        { code: 'ArrowLeft', expected: { type: 'move', direction: 'left' } },
        { code: 'ArrowRight', expected: { type: 'move', direction: 'right' } },
        { code: 'Space', expected: { type: 'rotate' } },
        { code: 'ArrowDown', expected: { type: 'move', direction: 'down' } }
    ];
    
    inputs.forEach((input, index) => {
        inputHandler.handleKeyDown({ code: input.code });
        assertEqual(mockEngine.getInputHistory().length, index + 1);
        assertDeepEqual(mockEngine.getLastInput(), input.expected);
    });
});

testRunner.test('should handle complete touch input flow', () => {
    const mockEngine = new MockGameEngine();
    const inputHandler = new InputHandler(mockEngine);
    
    // Mock canvas element
    const mockCanvas = createMockCanvas();
    document.getElementById = () => mockCanvas;
    
    // Simulate touch sequence
    const touchId = 1;
    
    // Touch start
    const startEvent = {
        changedTouches: [{
            identifier: touchId,
            clientX: 50,  // Left zone
            clientY: 300
        }]
    };
    
    inputHandler.handleTouchStart(startEvent);
    assert(inputHandler.touchStates.has(touchId));
    
    // Touch end (tap)
    const endEvent = {
        changedTouches: [{
            identifier: touchId,
            clientX: 55,  // Small movement
            clientY: 305
        }]
    };
    
    inputHandler.handleTouchEnd(endEvent);
    
    // Should generate left move action
    assertDeepEqual(mockEngine.getLastInput(), { type: 'move', direction: 'left' });
    assert(!inputHandler.touchStates.has(touchId));
});

// Error handling tests
testRunner.test('should handle missing canvas element gracefully', () => {
    const mockEngine = new MockGameEngine();
    const inputHandler = new InputHandler(mockEngine);
    
    // Mock getElementById to return null
    document.getElementById = () => null;
    
    const touch = { clientX: 100, clientY: 200 };
    const action = inputHandler.getTapAction(touch);
    
    assertEqual(action, null);
});

testRunner.test('should handle invalid touch states gracefully', () => {
    const mockEngine = new MockGameEngine();
    const inputHandler = new InputHandler(mockEngine);
    
    // Try to end touch that doesn't exist
    const endEvent = {
        changedTouches: [{
            identifier: 999,  // Non-existent touch
            clientX: 100,
            clientY: 200
        }]
    };
    
    // Should not throw error
    inputHandler.handleTouchEnd(endEvent);
    assertEqual(mockEngine.getInputHistory().length, 0);
});

// Mobile device detection tests
testRunner.test('should detect touch device correctly', () => {
    const mockEngine = new MockGameEngine();
    const inputHandler = new InputHandler(mockEngine);
    
    // Mock touch support
    const originalOntouchstart = window.ontouchstart;
    window.ontouchstart = {};
    
    assert(inputHandler.isTouchDevice());
    
    // Restore original
    if (originalOntouchstart === undefined) {
        delete window.ontouchstart;
    } else {
        window.ontouchstart = originalOntouchstart;
    }
});

testRunner.test('should detect mobile device correctly', () => {
    const mockEngine = new MockGameEngine();
    const inputHandler = new InputHandler(mockEngine);
    
    // Mock user agent
    const originalUserAgent = navigator.userAgent;
    Object.defineProperty(navigator, 'userAgent', {
        value: 'Mozilla/5.0 (iPhone; CPU iPhone OS 14_0 like Mac OS X)',
        configurable: true
    });
    
    assert(inputHandler.isMobileDevice());
    
    // Restore original
    Object.defineProperty(navigator, 'userAgent', {
        value: originalUserAgent,
        configurable: true
    });
});

// Puyo pair tap detection tests
testRunner.test('should detect tap on puyo pair correctly', () => {
    const mockEngine = new MockGameEngine();
    const inputHandler = new InputHandler(mockEngine);
    
    // Mock puyo pair
    const mockPuyoPair = {
        x: 2,
        y: 1,
        rotation: 0  // Vertical
    };
    
    // Test tap on primary puyo
    assert(inputHandler.isTapOnPuyoPair(2.2, 1.1, mockPuyoPair));
    
    // Test tap on secondary puyo (below primary for rotation 0)
    assert(inputHandler.isTapOnPuyoPair(2.1, 2.2, mockPuyoPair));
    
    // Test tap outside puyo pair
    assert(!inputHandler.isTapOnPuyoPair(4.0, 1.0, mockPuyoPair));
});

testRunner.test('should handle different puyo pair rotations', () => {
    const mockEngine = new MockGameEngine();
    const inputHandler = new InputHandler(mockEngine);
    
    const mockPuyoPair = {
        x: 2,
        y: 2,
        rotation: 1  // Horizontal, puyo2 to the right
    };
    
    // Test tap on primary puyo
    assert(inputHandler.isTapOnPuyoPair(2.1, 2.1, mockPuyoPair));
    
    // Test tap on secondary puyo (right of primary for rotation 1)
    assert(inputHandler.isTapOnPuyoPair(3.1, 2.1, mockPuyoPair));
    
    // Test tap outside
    assert(!inputHandler.isTapOnPuyoPair(1.0, 2.0, mockPuyoPair));
});

// Double tap detection tests
testRunner.test('should detect double tap for fast drop', () => {
    const mockEngine = new MockGameEngine();
    const inputHandler = new InputHandler(mockEngine);
    
    // Mock canvas element
    const mockCanvas = createMockCanvas();
    document.getElementById = () => mockCanvas;
    
    // First tap
    const firstTouchState = {
        startX: 150,
        startY: 300,
        currentX: 150,
        currentY: 300,
        startTime: performance.now() - 50
    };
    
    const firstTouch = { clientX: 150, clientY: 300 };
    const firstAction = inputHandler.touchToAction(firstTouchState, firstTouch);
    
    // Should be rotate action
    assertDeepEqual(firstAction, { type: 'rotate' });
    
    // Second tap quickly after first
    const secondTouchState = {
        startX: 152,
        startY: 302,
        currentX: 152,
        currentY: 302,
        startTime: performance.now() - 50
    };
    
    const secondTouch = { clientX: 152, clientY: 302 };
    
    // Simulate quick second tap
    inputHandler.lastTapTime = performance.now() - 100; // Within double tap delay
    const secondAction = inputHandler.touchToAction(secondTouchState, secondTouch);
    
    // Should be fast drop action
    assertDeepEqual(secondAction, { type: 'move', direction: 'down', fast: true });
});

// Enhanced swipe detection tests
testRunner.test('should require minimum swipe distance for directional detection', () => {
    const mockEngine = new MockGameEngine();
    const inputHandler = new InputHandler(mockEngine);
    
    // Small swipe should return null
    const smallSwipeAction = inputHandler.getSwipeAction(20, 10);
    assertEqual(smallSwipeAction, null);
    
    // Large swipe should work
    const largeSwipeAction = inputHandler.getSwipeAction(50, 10);
    assertDeepEqual(largeSwipeAction, { type: 'move', direction: 'right' });
});

testRunner.test('should handle diagonal swipes correctly', () => {
    const mockEngine = new MockGameEngine();
    const inputHandler = new InputHandler(mockEngine);
    
    // Diagonal swipe that's not clearly horizontal or vertical
    const diagonalAction = inputHandler.getSwipeAction(30, 35);
    assertEqual(diagonalAction, null);
});

// Touch zone information tests
testRunner.test('should provide accurate touch zone information', () => {
    const mockEngine = new MockGameEngine();
    const inputHandler = new InputHandler(mockEngine);
    
    // Mock canvas element
    const mockCanvas = createMockCanvas();
    document.getElementById = () => mockCanvas;
    
    // Test left zone
    const leftTouch = { clientX: 50, clientY: 300 };
    const leftZoneInfo = inputHandler.getTouchZoneInfo(leftTouch);
    
    assertEqual(leftZoneInfo.zone, 'left');
    assert(leftZoneInfo.gameX < 2); // Should be in left part of game field
    
    // Test right zone
    const rightTouch = { clientX: 250, clientY: 300 };
    const rightZoneInfo = inputHandler.getTouchZoneInfo(rightTouch);
    
    assertEqual(rightZoneInfo.zone, 'right');
    assert(rightZoneInfo.gameX > 4); // Should be in right part of game field
    
    // Test center zone
    const centerTouch = { clientX: 150, clientY: 300 };
    const centerZoneInfo = inputHandler.getTouchZoneInfo(centerTouch);
    
    assertEqual(centerZoneInfo.zone, 'center');
});

// Mobile optimization tests
testRunner.test('should optimize settings for mobile devices', () => {
    const mockEngine = new MockGameEngine();
    
    // Mock mobile device
    const originalUserAgent = navigator.userAgent;
    Object.defineProperty(navigator, 'userAgent', {
        value: 'Mozilla/5.0 (iPhone; CPU iPhone OS 14_0 like Mac OS X)',
        configurable: true
    });
    
    const inputHandler = new InputHandler(mockEngine);
    
    // Check if mobile optimizations were applied
    assertEqual(inputHandler.touchSettings.minSwipeDistance, 25);
    assertEqual(inputHandler.touchSettings.maxTapTime, 250);
    assertEqual(inputHandler.touchSettings.tapTolerance, 1.0);
    assertEqual(inputHandler.inputDelay, 80);
    
    // Restore original
    Object.defineProperty(navigator, 'userAgent', {
        value: originalUserAgent,
        configurable: true
    });
});

// Haptic feedback tests
testRunner.test('should trigger haptic feedback on mobile devices', () => {
    const mockEngine = new MockGameEngine();
    const inputHandler = new InputHandler(mockEngine);
    
    // Mock vibrate API
    let vibrateCallCount = 0;
    let lastVibratePattern = null;
    
    navigator.vibrate = (pattern) => {
        vibrateCallCount++;
        lastVibratePattern = pattern;
        return true;
    };
    
    // Mock mobile device
    const originalUserAgent = navigator.userAgent;
    Object.defineProperty(navigator, 'userAgent', {
        value: 'Mozilla/5.0 (Android 10; Mobile)',
        configurable: true
    });
    
    // Test different haptic feedback types
    inputHandler.triggerHapticFeedback('light');
    assertEqual(vibrateCallCount, 1);
    assertEqual(lastVibratePattern, 10);
    
    inputHandler.triggerHapticFeedback('medium');
    assertEqual(vibrateCallCount, 2);
    assertEqual(lastVibratePattern, 20);
    
    inputHandler.triggerHapticFeedback('heavy');
    assertEqual(vibrateCallCount, 3);
    assertEqual(lastVibratePattern, 50);
    
    // Restore original
    Object.defineProperty(navigator, 'userAgent', {
        value: originalUserAgent,
        configurable: true
    });
    
    delete navigator.vibrate;
});

// Export the test runner for use in test HTML
export { testRunner as InputHandlerTestRunner };

// Auto-run tests if this module is loaded directly
if (typeof window !== 'undefined') {
    // Browser environment - make test runner available globally
    window.InputHandlerTestRunner = testRunner;
}