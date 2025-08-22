/**
 * Unit tests for Renderer class
 * Tests canvas context management, field rendering, and puyo rendering
 */

import { Renderer } from '../../rendering/Renderer.js';
import { Puyo } from '../../models/Puyo.js';
import { PuyoPair } from '../../models/PuyoPair.js';

// Mock canvas and context for testing
class MockCanvas {
    constructor(width = 360, height = 720) {
        this.width = width;
        this.height = height;
        this.parentElement = {
            getBoundingClientRect: () => ({ width: 400, height: 800 })
        };
    }
    
    getContext(type) {
        return new MockContext();
    }
}

class MockContext {
    constructor() {
        this.fillStyle = '';
        this.strokeStyle = '';
        this.lineWidth = 1;
        this.font = '';
        this.textAlign = 'start';
        this.textBaseline = 'alphabetic';
        this.imageSmoothingEnabled = true;
        
        // Track method calls for testing
        this.calls = [];
    }
    
    clearRect(x, y, width, height) {
        this.calls.push(['clearRect', x, y, width, height]);
    }
    
    fillRect(x, y, width, height) {
        this.calls.push(['fillRect', x, y, width, height]);
    }
    
    strokeRect(x, y, width, height) {
        this.calls.push(['strokeRect', x, y, width, height]);
    }
    
    beginPath() {
        this.calls.push(['beginPath']);
    }
    
    arc(x, y, radius, startAngle, endAngle) {
        this.calls.push(['arc', x, y, radius, startAngle, endAngle]);
    }
    
    moveTo(x, y) {
        this.calls.push(['moveTo', x, y]);
    }
    
    lineTo(x, y) {
        this.calls.push(['lineTo', x, y]);
    }
    
    fill() {
        this.calls.push(['fill']);
    }
    
    stroke() {
        this.calls.push(['stroke']);
    }
    
    fillText(text, x, y) {
        this.calls.push(['fillText', text, x, y]);
    }
    
    save() {
        this.calls.push(['save']);
    }
    
    restore() {
        this.calls.push(['restore']);
    }
    
    translate(x, y) {
        this.calls.push(['translate', x, y]);
    }
    
    createRadialGradient(x0, y0, r0, x1, y1, r1) {
        this.calls.push(['createRadialGradient', x0, y0, r0, x1, y1, r1]);
        return {
            addColorStop: (offset, color) => {
                this.calls.push(['addColorStop', offset, color]);
            }
        };
    }
    
    getLastCall() {
        return this.calls[this.calls.length - 1];
    }
    
    getCallsOfType(type) {
        return this.calls.filter(call => call[0] === type);
    }
    
    clearCalls() {
        this.calls = [];
    }
}

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
        console.log('Running Renderer tests...');
        
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
        
        console.log(`\nRenderer Test Summary: ${passed} passed, ${failed} failed`);
        
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

function assertGreaterThan(actual, expected, message) {
    if (actual <= expected) {
        throw new Error(message || `Expected ${actual} to be greater than ${expected}`);
    }
}

function assertArrayEqual(actual, expected, message) {
    if (JSON.stringify(actual) !== JSON.stringify(expected)) {
        throw new Error(message || `Expected ${JSON.stringify(expected)}, but got ${JSON.stringify(actual)}`);
    }
}

// Create test runner instance
const testRunner = new TestRunner();

// Constructor tests
testRunner.test('should initialize with canvas elements', () => {
    const gameCanvas = new MockCanvas(360, 720);
    const nextPuyoCanvas = new MockCanvas(120, 120);
    const renderer = new Renderer(gameCanvas, nextPuyoCanvas);
    
    assertEqual(renderer.gameCanvas, gameCanvas);
    assertEqual(renderer.nextPuyoCanvas, nextPuyoCanvas);
    assert(renderer.gameContext instanceof MockContext);
    assert(renderer.nextPuyoContext instanceof MockContext);
});

testRunner.test('should set correct field dimensions', () => {
    const gameCanvas = new MockCanvas();
    const nextPuyoCanvas = new MockCanvas();
    const renderer = new Renderer(gameCanvas, nextPuyoCanvas);
    
    assertEqual(renderer.fieldWidth, 6);
    assertEqual(renderer.fieldHeight, 12);
});

testRunner.test('should initialize puyo colors', () => {
    const gameCanvas = new MockCanvas();
    const nextPuyoCanvas = new MockCanvas();
    const renderer = new Renderer(gameCanvas, nextPuyoCanvas);
    
    assertEqual(renderer.puyoColors.red, '#FF4444');
    assertEqual(renderer.puyoColors.blue, '#4444FF');
    assertEqual(renderer.puyoColors.green, '#44FF44');
    assertEqual(renderer.puyoColors.yellow, '#FFFF44');
    assertEqual(renderer.puyoColors.purple, '#FF44FF');
});

testRunner.test('should calculate cell size based on canvas dimensions', () => {
    const gameCanvas = new MockCanvas(360, 720);
    const nextPuyoCanvas = new MockCanvas();
    const renderer = new Renderer(gameCanvas, nextPuyoCanvas);
    
    assertEqual(renderer.cellSize, 60); // min(360/6, 720/12) = min(60, 60) = 60
});

// Canvas management tests
testRunner.test('should initialize canvas settings', () => {
    const gameCanvas = new MockCanvas();
    const nextPuyoCanvas = new MockCanvas();
    const renderer = new Renderer(gameCanvas, nextPuyoCanvas);
    
    assertEqual(renderer.gameContext.imageSmoothingEnabled, true);
    assertEqual(renderer.gameContext.textAlign, 'center');
    assertEqual(renderer.gameContext.textBaseline, 'middle');
    assertEqual(renderer.nextPuyoContext.imageSmoothingEnabled, true);
});

testRunner.test('should clear both canvases', () => {
    const gameCanvas = new MockCanvas(360, 720);
    const nextPuyoCanvas = new MockCanvas(120, 120);
    const renderer = new Renderer(gameCanvas, nextPuyoCanvas);
    
    renderer.gameContext.clearCalls();
    renderer.nextPuyoContext.clearCalls();
    
    renderer.clear();
    
    assertEqual(renderer.gameContext.getCallsOfType('clearRect').length, 1);
    assertEqual(renderer.nextPuyoContext.getCallsOfType('clearRect').length, 1);
    
    const gameClearCall = renderer.gameContext.getCallsOfType('clearRect')[0];
    assertArrayEqual(gameClearCall, ['clearRect', 0, 0, 360, 720]);
    
    const nextClearCall = renderer.nextPuyoContext.getCallsOfType('clearRect')[0];
    assertArrayEqual(nextClearCall, ['clearRect', 0, 0, 120, 120]);
});

// Field rendering tests
testRunner.test('should render empty field with background and grid', () => {
    const gameCanvas = new MockCanvas(360, 720);
    const nextPuyoCanvas = new MockCanvas();
    const renderer = new Renderer(gameCanvas, nextPuyoCanvas);
    
    renderer.gameContext.clearCalls();
    const emptyField = Array(12).fill(null).map(() => Array(6).fill(null));
    
    renderer.renderField(emptyField);
    
    // Should draw background
    const fillRectCalls = renderer.gameContext.getCallsOfType('fillRect');
    assertGreaterThan(fillRectCalls.length, 0);
    assertArrayEqual(fillRectCalls[0], ['fillRect', 0, 0, 360, 720]);
    
    // Should draw grid lines
    const strokeCalls = renderer.gameContext.getCallsOfType('stroke');
    assertEqual(strokeCalls.length, 20); // 7 vertical + 13 horizontal lines
});

testRunner.test('should handle null field gracefully', () => {
    const gameCanvas = new MockCanvas();
    const nextPuyoCanvas = new MockCanvas();
    const renderer = new Renderer(gameCanvas, nextPuyoCanvas);
    
    renderer.gameContext.clearCalls();
    
    renderer.renderField(null);
    
    // Should not throw error and not make any drawing calls
    assertEqual(renderer.gameContext.calls.length, 0);
});

testRunner.test('should render field with puyo', () => {
    const gameCanvas = new MockCanvas(360, 720);
    const nextPuyoCanvas = new MockCanvas();
    const renderer = new Renderer(gameCanvas, nextPuyoCanvas);
    
    renderer.gameContext.clearCalls();
    const field = Array(12).fill(null).map(() => Array(6).fill(null));
    const redPuyo = new Puyo('red', 2, 5);
    field[5][2] = redPuyo;
    
    renderer.renderField(field);
    
    // Should draw background, grid, and puyo
    const fillRectCalls = renderer.gameContext.getCallsOfType('fillRect');
    const arcCalls = renderer.gameContext.getCallsOfType('arc');
    
    assertGreaterThan(fillRectCalls.length, 0);
    assertGreaterThan(arcCalls.length, 0); // Puyo circles
});

// Grid drawing tests
testRunner.test('should draw correct number of grid lines', () => {
    const gameCanvas = new MockCanvas(360, 720);
    const nextPuyoCanvas = new MockCanvas();
    const renderer = new Renderer(gameCanvas, nextPuyoCanvas);
    
    renderer.gameContext.clearCalls();
    
    renderer.drawGrid(renderer.gameContext);
    
    const strokeCalls = renderer.gameContext.getCallsOfType('stroke');
    
    // 7 vertical lines (0 to 6) + 13 horizontal lines (0 to 12) = 20 lines
    assertEqual(strokeCalls.length, 20); // 7 vertical + 13 horizontal lines
});

testRunner.test('should set correct stroke style for grid', () => {
    const gameCanvas = new MockCanvas();
    const nextPuyoCanvas = new MockCanvas();
    const renderer = new Renderer(gameCanvas, nextPuyoCanvas);
    
    renderer.drawGrid(renderer.gameContext);
    
    assertEqual(renderer.gameContext.strokeStyle, '#4A5568');
    assertEqual(renderer.gameContext.lineWidth, 1);
});

// Puyo rendering tests
testRunner.test('should render puyo with correct color and position', () => {
    const gameCanvas = new MockCanvas(360, 720);
    const nextPuyoCanvas = new MockCanvas();
    const renderer = new Renderer(gameCanvas, nextPuyoCanvas);
    
    renderer.gameContext.clearCalls();
    const redPuyo = new Puyo('red', 2, 3);
    
    renderer.renderPuyo(redPuyo, 2, 3);
    
    const arcCalls = renderer.gameContext.getCallsOfType('arc');
    assertGreaterThan(arcCalls.length, 0);
    
    // Check if puyo is drawn at correct position
    const mainArc = arcCalls[0];
    assertEqual(mainArc[1], 150); // (2 + 0.5) * 60 = 150
    assertEqual(mainArc[2], 210); // (3 + 0.5) * 60 = 210
    assertEqual(mainArc[3], 24);  // 60 * 0.4 = 24 (radius)
});

testRunner.test('should handle null puyo gracefully', () => {
    const gameCanvas = new MockCanvas();
    const nextPuyoCanvas = new MockCanvas();
    const renderer = new Renderer(gameCanvas, nextPuyoCanvas);
    
    renderer.gameContext.clearCalls();
    
    renderer.renderPuyo(null, 0, 0);
    
    assertEqual(renderer.gameContext.calls.length, 0);
});

testRunner.test('should handle puyo without color', () => {
    const gameCanvas = new MockCanvas();
    const nextPuyoCanvas = new MockCanvas();
    const renderer = new Renderer(gameCanvas, nextPuyoCanvas);
    
    renderer.gameContext.clearCalls();
    const invalidPuyo = { x: 0, y: 0 }; // No color property
    
    renderer.renderPuyo(invalidPuyo, 0, 0);
    
    assertEqual(renderer.gameContext.calls.length, 0);
});

testRunner.test('should render different puyo colors correctly', () => {
    const gameCanvas = new MockCanvas();
    const nextPuyoCanvas = new MockCanvas();
    const renderer = new Renderer(gameCanvas, nextPuyoCanvas);
    
    const colors = ['red', 'blue', 'green', 'yellow', 'purple'];
    
    colors.forEach(color => {
        renderer.gameContext.clearCalls();
        const puyo = new Puyo(color, 0, 0);
        
        renderer.renderPuyo(puyo, 0, 0);
        
        // Should have drawn the puyo (multiple arc calls for body, outline, highlight)
        const arcCalls = renderer.gameContext.getCallsOfType('arc');
        assertGreaterThan(arcCalls.length, 0);
    });
});

testRunner.test('should render clearing effect for clearing puyo', () => {
    const gameCanvas = new MockCanvas();
    const nextPuyoCanvas = new MockCanvas();
    const renderer = new Renderer(gameCanvas, nextPuyoCanvas);
    
    renderer.gameContext.clearCalls();
    const clearingPuyo = new Puyo('red', 0, 0);
    clearingPuyo.state = 'clearing';
    
    renderer.renderPuyo(clearingPuyo, 0, 0);
    
    const arcCalls = renderer.gameContext.getCallsOfType('arc');
    // Should have extra arc call for clearing effect
    assertGreaterThan(arcCalls.length, 2);
});

testRunner.test('should render falling effect for falling puyo', () => {
    const gameCanvas = new MockCanvas();
    const nextPuyoCanvas = new MockCanvas();
    const renderer = new Renderer(gameCanvas, nextPuyoCanvas);
    
    renderer.gameContext.clearCalls();
    const fallingPuyo = new Puyo('red', 0, 0);
    fallingPuyo.state = 'falling';
    
    renderer.renderPuyo(fallingPuyo, 0, 0);
    
    const arcCalls = renderer.gameContext.getCallsOfType('arc');
    // Should have extra arc call for shadow effect
    assertGreaterThan(arcCalls.length, 2);
});

// Next puyo rendering tests
testRunner.test('should render next puyo pair in preview canvas', () => {
    const gameCanvas = new MockCanvas();
    const nextPuyoCanvas = new MockCanvas(120, 120);
    const renderer = new Renderer(gameCanvas, nextPuyoCanvas);
    
    renderer.nextPuyoContext.clearCalls();
    const puyo1 = new Puyo('red');
    const puyo2 = new Puyo('blue');
    const puyoPair = new PuyoPair(puyo1, puyo2);
    
    renderer.renderNextPuyo(puyoPair);
    
    // Should clear canvas and draw background (background + shadow)
    assertEqual(renderer.nextPuyoContext.getCallsOfType('clearRect').length, 1);
    assertEqual(renderer.nextPuyoContext.getCallsOfType('fillRect').length, 2);
    
    // Should draw both puyo
    const arcCalls = renderer.nextPuyoContext.getCallsOfType('arc');
    assertGreaterThan(arcCalls.length, 0);
});

testRunner.test('should handle null puyo pair gracefully', () => {
    const gameCanvas = new MockCanvas();
    const nextPuyoCanvas = new MockCanvas(120, 120);
    const renderer = new Renderer(gameCanvas, nextPuyoCanvas);
    
    renderer.nextPuyoContext.clearCalls();
    
    renderer.renderNextPuyo(null);
    
    // Should render empty next puyo (clear + background + border + shadow + text)
    assertGreaterThan(renderer.nextPuyoContext.calls.length, 0);
    assertEqual(renderer.nextPuyoContext.getCallsOfType('clearRect').length, 1);
    assertEqual(renderer.nextPuyoContext.getCallsOfType('fillText').length, 1);
});

// Game over rendering tests
testRunner.test('should render game over screen with overlay and text', () => {
    const gameCanvas = new MockCanvas(360, 720);
    const nextPuyoCanvas = new MockCanvas();
    const renderer = new Renderer(gameCanvas, nextPuyoCanvas);
    
    renderer.gameContext.clearCalls();
    
    renderer.renderGameOver();
    
    // Should draw overlay
    const fillRectCalls = renderer.gameContext.getCallsOfType('fillRect');
    assertEqual(fillRectCalls.length, 1);
    assertArrayEqual(fillRectCalls[0], ['fillRect', 0, 0, 360, 720]);
    
    // Should draw text
    const fillTextCalls = renderer.gameContext.getCallsOfType('fillText');
    assertEqual(fillTextCalls.length, 1);
    assertEqual(fillTextCalls[0][1], 'ゲームオーバー');
});

// Placeholder rendering tests
testRunner.test('should render placeholder content', () => {
    const gameCanvas = new MockCanvas(360, 720);
    const nextPuyoCanvas = new MockCanvas();
    const renderer = new Renderer(gameCanvas, nextPuyoCanvas);
    
    renderer.gameContext.clearCalls();
    
    renderer.renderPlaceholder();
    
    // Should draw background
    const fillRectCalls = renderer.gameContext.getCallsOfType('fillRect');
    assertEqual(fillRectCalls.length, 1);
    
    // Should draw text
    const fillTextCalls = renderer.gameContext.getCallsOfType('fillText');
    assertGreaterThan(fillTextCalls.length, 0);
});

// UI rendering tests
testRunner.test('should render UI with score and chains', () => {
    const gameCanvas = new MockCanvas();
    const nextPuyoCanvas = new MockCanvas();
    const renderer = new Renderer(gameCanvas, nextPuyoCanvas);
    
    // Mock DOM elements
    const mockScoreElement = { textContent: '', classList: { add: () => {}, remove: () => {} } };
    const mockChainElement = { textContent: '', classList: { add: () => {}, remove: () => {} } };
    
    // Mock document.getElementById
    const originalGetElementById = global.document?.getElementById;
    global.document = {
        getElementById: (id) => {
            if (id === 'score-display') return mockScoreElement;
            if (id === 'chain-display') return mockChainElement;
            return null;
        }
    };
    
    const puyo1 = new Puyo('red');
    const puyo2 = new Puyo('blue');
    const nextPuyo = new PuyoPair(puyo1, puyo2);
    
    renderer.renderUI(12345, 3, nextPuyo);
    
    assertEqual(mockScoreElement.textContent, '12,345');
    assertEqual(mockChainElement.textContent, '3');
    
    // Restore original function
    if (originalGetElementById) {
        global.document.getElementById = originalGetElementById;
    }
});

testRunner.test('should update score display with proper formatting', () => {
    const gameCanvas = new MockCanvas();
    const nextPuyoCanvas = new MockCanvas();
    const renderer = new Renderer(gameCanvas, nextPuyoCanvas);
    
    const mockScoreElement = { textContent: '' };
    global.document = {
        getElementById: (id) => id === 'score-display' ? mockScoreElement : null
    };
    
    renderer.updateScoreDisplay(1234567);
    assertEqual(mockScoreElement.textContent, '1,234,567');
});

testRunner.test('should update chain display and add animation class', () => {
    const gameCanvas = new MockCanvas();
    const nextPuyoCanvas = new MockCanvas();
    const renderer = new Renderer(gameCanvas, nextPuyoCanvas);
    
    let addedClass = '';
    let removedClass = '';
    const mockChainElement = { 
        textContent: '',
        classList: { 
            add: (className) => { addedClass = className; },
            remove: (className) => { removedClass = className; }
        }
    };
    
    global.document = {
        getElementById: (id) => id === 'chain-display' ? mockChainElement : null
    };
    
    // Mock setTimeout
    const originalSetTimeout = global.setTimeout;
    global.setTimeout = (fn, delay) => {
        assertEqual(delay, 1000);
        fn(); // Execute immediately for testing
    };
    
    renderer.updateChainDisplay(5);
    
    assertEqual(mockChainElement.textContent, '5');
    assertEqual(addedClass, 'chain-active');
    assertEqual(removedClass, 'chain-active');
    
    // Restore setTimeout
    global.setTimeout = originalSetTimeout;
});

testRunner.test('should handle missing DOM elements gracefully', () => {
    const gameCanvas = new MockCanvas();
    const nextPuyoCanvas = new MockCanvas();
    const renderer = new Renderer(gameCanvas, nextPuyoCanvas);
    
    global.document = {
        getElementById: () => null
    };
    
    // Should not throw error
    try {
        renderer.updateScoreDisplay(1000);
        renderer.updateChainDisplay(2);
        assert(true);
    } catch (error) {
        throw new Error('Should handle missing DOM elements gracefully');
    }
});

// Enhanced next puyo rendering tests
testRunner.test('should render empty next puyo with placeholder', () => {
    const gameCanvas = new MockCanvas();
    const nextPuyoCanvas = new MockCanvas(120, 120);
    const renderer = new Renderer(gameCanvas, nextPuyoCanvas);
    
    renderer.nextPuyoContext.clearCalls();
    
    renderer.renderEmptyNextPuyo();
    
    // Should clear canvas and draw background
    assertEqual(renderer.nextPuyoContext.getCallsOfType('clearRect').length, 1);
    assertEqual(renderer.nextPuyoContext.getCallsOfType('fillRect').length, 2); // Background + shadow
    assertEqual(renderer.nextPuyoContext.getCallsOfType('strokeRect').length, 1); // Border
    
    // Should draw placeholder text
    const fillTextCalls = renderer.nextPuyoContext.getCallsOfType('fillText');
    assertEqual(fillTextCalls.length, 1);
    assertEqual(fillTextCalls[0][1], '次のぷよ');
});

testRunner.test('should render next puyo background with border and shadow', () => {
    const gameCanvas = new MockCanvas();
    const nextPuyoCanvas = new MockCanvas(120, 120);
    const renderer = new Renderer(gameCanvas, nextPuyoCanvas);
    
    renderer.nextPuyoContext.clearCalls();
    
    renderer.renderNextPuyoBackground(renderer.nextPuyoContext, 120, 120);
    
    // Should draw background, border, and shadow
    assertEqual(renderer.nextPuyoContext.getCallsOfType('fillRect').length, 2); // Background + shadow
    assertEqual(renderer.nextPuyoContext.getCallsOfType('strokeRect').length, 1); // Border
});

// Visual distinction tests
testRunner.test('should render fixed puyo with distinct visual effect', () => {
    const gameCanvas = new MockCanvas();
    const nextPuyoCanvas = new MockCanvas();
    const renderer = new Renderer(gameCanvas, nextPuyoCanvas);
    
    renderer.gameContext.clearCalls();
    const fixedPuyo = new Puyo('red', 0, 0);
    fixedPuyo.state = 'fixed';
    
    renderer.renderPuyo(fixedPuyo, 0, 0);
    
    const arcCalls = renderer.gameContext.getCallsOfType('arc');
    // Should have extra arc calls for fixed effect (border + inner shadow)
    assertGreaterThan(arcCalls.length, 2);
});

testRunner.test('should render enhanced falling effect with motion blur', () => {
    const gameCanvas = new MockCanvas();
    const nextPuyoCanvas = new MockCanvas();
    const renderer = new Renderer(gameCanvas, nextPuyoCanvas);
    
    renderer.gameContext.clearCalls();
    const fallingPuyo = new Puyo('blue', 1, 1);
    fallingPuyo.state = 'falling';
    
    renderer.renderPuyo(fallingPuyo, 1, 1);
    
    const arcCalls = renderer.gameContext.getCallsOfType('arc');
    // Should have extra arc calls for shadow and motion blur effects
    assertGreaterThan(arcCalls.length, 3);
});

// Animation integration tests
testRunner.test('should initialize animation manager', () => {
    const gameCanvas = new MockCanvas();
    const nextPuyoCanvas = new MockCanvas();
    const renderer = new Renderer(gameCanvas, nextPuyoCanvas);
    
    assert(renderer.animationManager);
    assertEqual(renderer.animationManager.getAnimationCount(), 0);
    assertEqual(renderer.screenShakeOffset.x, 0);
    assertEqual(renderer.screenShakeOffset.y, 0);
});

testRunner.test('should create clear animation', () => {
    const gameCanvas = new MockCanvas();
    const nextPuyoCanvas = new MockCanvas();
    const renderer = new Renderer(gameCanvas, nextPuyoCanvas);
    
    const animationId = renderer.startClearAnimation(2, 3);
    
    assertEqual(renderer.animationManager.getAnimationCount(), 1);
    
    const animation = renderer.animationManager.getAnimation(animationId);
    assertEqual(animation.type, 'clear');
    assertEqual(animation.properties.x, 2);
    assertEqual(animation.properties.y, 3);
});

testRunner.test('should create chain highlight animation', () => {
    const gameCanvas = new MockCanvas();
    const nextPuyoCanvas = new MockCanvas();
    const renderer = new Renderer(gameCanvas, nextPuyoCanvas);
    
    const animationId = renderer.startChainHighlight(1, 2, 5);
    
    assertEqual(renderer.animationManager.getAnimationCount(), 1);
    
    const animation = renderer.animationManager.getAnimation(animationId);
    assertEqual(animation.type, 'chainHighlight');
    assertEqual(animation.properties.x, 1);
    assertEqual(animation.properties.y, 2);
    assertEqual(animation.properties.chainLevel, 5);
});

testRunner.test('should create screen shake animation', () => {
    const gameCanvas = new MockCanvas();
    const nextPuyoCanvas = new MockCanvas();
    const renderer = new Renderer(gameCanvas, nextPuyoCanvas);
    
    const animationId = renderer.startScreenShake(8);
    
    assertEqual(renderer.animationManager.getAnimationCount(), 1);
    
    const animation = renderer.animationManager.getAnimation(animationId);
    assertEqual(animation.type, 'screenShake');
    assertEqual(animation.properties.intensity, 8);
});

testRunner.test('should create move animation', () => {
    const gameCanvas = new MockCanvas();
    const nextPuyoCanvas = new MockCanvas();
    const renderer = new Renderer(gameCanvas, nextPuyoCanvas);
    
    const animationId = renderer.startMoveAnimation(0, 0, 5, 10);
    
    assertEqual(renderer.animationManager.getAnimationCount(), 1);
    
    const animation = renderer.animationManager.getAnimation(animationId);
    assertEqual(animation.type, 'move');
    assertEqual(animation.properties.fromX, 0);
    assertEqual(animation.properties.fromY, 0);
    assertEqual(animation.properties.toX, 5);
    assertEqual(animation.properties.toY, 10);
});

testRunner.test('should create drop animation', () => {
    const gameCanvas = new MockCanvas();
    const nextPuyoCanvas = new MockCanvas();
    const renderer = new Renderer(gameCanvas, nextPuyoCanvas);
    
    const animationId = renderer.startDropAnimation(3, 0, 8);
    
    assertEqual(renderer.animationManager.getAnimationCount(), 1);
    
    const animation = renderer.animationManager.getAnimation(animationId);
    assertEqual(animation.type, 'drop');
    assertEqual(animation.properties.x, 3);
    assertEqual(animation.properties.fromY, 0);
    assertEqual(animation.properties.toY, 8);
});

testRunner.test('should update animations and screen shake', () => {
    const gameCanvas = new MockCanvas();
    const nextPuyoCanvas = new MockCanvas();
    const renderer = new Renderer(gameCanvas, nextPuyoCanvas);
    
    // Create screen shake animation
    renderer.startScreenShake(5);
    
    const currentTime = Date.now();
    const completed = renderer.updateAnimations(currentTime);
    
    // Screen shake should update the offset
    assert(Math.abs(renderer.screenShakeOffset.x) <= 5);
    assert(Math.abs(renderer.screenShakeOffset.y) <= 5);
});

testRunner.test('should check for active animations', () => {
    const gameCanvas = new MockCanvas();
    const nextPuyoCanvas = new MockCanvas();
    const renderer = new Renderer(gameCanvas, nextPuyoCanvas);
    
    assertEqual(renderer.hasActiveAnimations(), false);
    
    renderer.startClearAnimation(0, 0);
    assertEqual(renderer.hasActiveAnimations(), true);
    
    renderer.clearAllAnimations();
    assertEqual(renderer.hasActiveAnimations(), false);
    assertEqual(renderer.screenShakeOffset.x, 0);
    assertEqual(renderer.screenShakeOffset.y, 0);
});

testRunner.test('should render puyo with animated position', () => {
    const gameCanvas = new MockCanvas();
    const nextPuyoCanvas = new MockCanvas();
    const renderer = new Renderer(gameCanvas, nextPuyoCanvas);
    
    renderer.gameContext.clearCalls();
    const puyo = new Puyo('red', 0, 0);
    const animatedPosition = { x: 2.5, y: 3.5 };
    
    renderer.renderPuyo(puyo, 0, 0, renderer.gameContext, animatedPosition);
    
    const arcCalls = renderer.gameContext.getCallsOfType('arc');
    assertGreaterThan(arcCalls.length, 0);
    
    // Check if puyo is drawn at animated position
    const mainArc = arcCalls[0];
    assertEqual(mainArc[1], 180); // (2.5 + 0.5) * 60 = 180
    assertEqual(mainArc[2], 240); // (3.5 + 0.5) * 60 = 240
});

testRunner.test('should render animations', () => {
    const gameCanvas = new MockCanvas();
    const nextPuyoCanvas = new MockCanvas();
    const renderer = new Renderer(gameCanvas, nextPuyoCanvas);
    
    renderer.gameContext.clearCalls();
    
    // Create some animations
    renderer.startChainHighlight(1, 1, 2);
    renderer.startClearAnimation(2, 2);
    
    renderer.renderAnimations();
    
    // Should have made drawing calls for animations
    const calls = renderer.gameContext.calls;
    assertGreaterThan(calls.length, 0);
    
    // Should have save/restore calls for context management
    const saveCalls = renderer.gameContext.calls.filter(call => call[0] === 'save');
    const restoreCalls = renderer.gameContext.calls.filter(call => call[0] === 'restore');
    assertGreaterThan(saveCalls.length, 0);
    assertGreaterThan(restoreCalls.length, 0);
});

// Canvas resizing tests
testRunner.test('should resize canvas and recalculate cell size', () => {
    const gameCanvas = new MockCanvas(360, 720);
    const nextPuyoCanvas = new MockCanvas();
    const renderer = new Renderer(gameCanvas, nextPuyoCanvas);
    
    const originalCellSize = renderer.cellSize;
    
    // Mock container with different dimensions
    gameCanvas.parentElement.getBoundingClientRect = () => ({ width: 300, height: 600 });
    
    renderer.resize();
    
    // Cell size should be recalculated
    assertEqual(renderer.cellSize, 50); // min(300/6, 600/12) = min(50, 50) = 50
    assert(renderer.cellSize !== originalCellSize);
});

testRunner.test('should handle missing parent element gracefully', () => {
    const gameCanvas = new MockCanvas();
    const nextPuyoCanvas = new MockCanvas();
    const renderer = new Renderer(gameCanvas, nextPuyoCanvas);
    
    gameCanvas.parentElement = null;
    
    // Should not throw error
    try {
        renderer.resize();
        assert(true); // Test passes if no error is thrown
    } catch (error) {
        throw new Error('resize() should handle missing parent element gracefully');
    }
});

// Export test runner for use in main test runner
export const RendererTestRunner = testRunner;

// Export test utilities for other test files
export { MockCanvas, MockContext };