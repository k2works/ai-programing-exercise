/**
 * Unit tests for GameEngine class
 * Tests game loop, state management, and lifecycle methods
 */

import { GameEngine } from '../../engine/GameEngine.js';
import { GameState } from '../../models/GameState.js';
import { Puyo } from '../../models/Puyo.js';

// Mock requestAnimationFrame and cancelAnimationFrame for Node.js environment
if (typeof global !== 'undefined' && !global.requestAnimationFrame) {
    let frameId = 0;
    global.requestAnimationFrame = (callback) => {
        frameId++;
        setTimeout(() => callback(performance.now()), 16);
        return frameId;
    };
    global.cancelAnimationFrame = (id) => {
        // Mock implementation - in real tests we might track this
    };
}

// Mock performance.now if not available
if (typeof global !== 'undefined' && !global.performance) {
    global.performance = {
        now: () => Date.now()
    };
}

// Mock renderer for testing
class MockRenderer {
    constructor() {
        this.clearCalled = false;
        this.renderPlaceholderCalled = false;
    }
    
    clear() {
        this.clearCalled = true;
    }
    
    renderPlaceholder() {
        this.renderPlaceholderCalled = true;
    }
    
    reset() {
        this.clearCalled = false;
        this.renderPlaceholderCalled = false;
    }
}

// Mock audio manager for testing
class MockAudioManager {
    constructor() {
        this.sounds = {};
    }
}

// Test suite for GameEngine
export function runGameEngineTests() {
    console.log('Running GameEngine tests...');
    
    let testsPassed = 0;
    let testsTotal = 0;
    
    function test(name, testFn) {
        testsTotal++;
        try {
            testFn();
            console.log(`✓ ${name}`);
            testsPassed++;
        } catch (error) {
            console.error(`✗ ${name}: ${error.message}`);
        }
    }
    
    function assert(condition, message) {
        if (!condition) {
            throw new Error(message);
        }
    }
    
    function assertEquals(actual, expected, message) {
        if (actual !== expected) {
            throw new Error(`${message}: expected ${expected}, got ${actual}`);
        }
    }
    
    function assertNotNull(value, message) {
        if (value === null || value === undefined) {
            throw new Error(message);
        }
    }
    
    // Test GameEngine constructor
    test('GameEngine constructor initializes correctly', () => {
        const renderer = new MockRenderer();
        const audioManager = new MockAudioManager();
        const engine = new GameEngine(renderer, audioManager);
        
        assertEquals(engine.renderer, renderer, 'Renderer should be set');
        assertEquals(engine.audioManager, audioManager, 'Audio manager should be set');
        assertEquals(engine.isRunning, false, 'Should not be running initially');
        assertEquals(engine.isPausedState, false, 'Should not be paused initially');
        assertEquals(engine.gameState, null, 'Game state should be null initially');
        assertEquals(engine.animationFrameId, null, 'Animation frame ID should be null initially');
    });
    
    // Test start method
    test('start() initializes game state and starts game loop', () => {
        const renderer = new MockRenderer();
        const audioManager = new MockAudioManager();
        const engine = new GameEngine(renderer, audioManager);
        
        let gameStartedEventReceived = false;
        let scoreUpdateEventReceived = false;
        let chainUpdateEventReceived = false;
        
        engine.onGameEvent = (event) => {
            if (event.type === 'gameStarted') {
                gameStartedEventReceived = true;
            } else if (event.type === 'scoreUpdate') {
                scoreUpdateEventReceived = true;
                assertEquals(event.score, 0, 'Initial score should be 0');
            } else if (event.type === 'chainUpdate') {
                chainUpdateEventReceived = true;
                assertEquals(event.chainCount, 0, 'Initial chain count should be 0');
            }
        };
        
        engine.start();
        
        assertEquals(engine.isGameRunning(), true, 'Game should be running after start');
        assertEquals(engine.isPaused(), false, 'Game should not be paused after start');
        assertNotNull(engine.gameState, 'Game state should be initialized');
        assert(engine.gameState instanceof GameState, 'Game state should be GameState instance');
        assertEquals(engine.gameState.getScore(), 0, 'Initial score should be 0');
        assertEquals(engine.gameState.getStatus(), 'playing', 'Game status should be playing');
        assertNotNull(engine.animationFrameId, 'Animation frame should be scheduled');
        
        assert(gameStartedEventReceived, 'Game started event should be emitted');
        assert(scoreUpdateEventReceived, 'Score update event should be emitted');
        assert(chainUpdateEventReceived, 'Chain update event should be emitted');
        
        // Clean up
        engine.stop();
    });
    
    // Test start when already running
    test('start() when already running should warn and not restart', () => {
        const renderer = new MockRenderer();
        const audioManager = new MockAudioManager();
        const engine = new GameEngine(renderer, audioManager);
        
        engine.start();
        const firstGameState = engine.gameState;
        const firstAnimationId = engine.animationFrameId;
        
        // Try to start again
        engine.start();
        
        assertEquals(engine.gameState, firstGameState, 'Game state should not change');
        assertEquals(engine.animationFrameId, firstAnimationId, 'Animation frame ID should not change');
        
        // Clean up
        engine.stop();
    });
    
    // Test pause method
    test('pause() pauses the game correctly', () => {
        const renderer = new MockRenderer();
        const audioManager = new MockAudioManager();
        const engine = new GameEngine(renderer, audioManager);
        
        let gamePausedEventReceived = false;
        engine.onGameEvent = (event) => {
            if (event.type === 'gamePaused') {
                gamePausedEventReceived = true;
            }
        };
        
        engine.start();
        engine.pause();
        
        assertEquals(engine.isGameRunning(), true, 'Game should still be running');
        assertEquals(engine.isPaused(), true, 'Game should be paused');
        assertEquals(engine.gameState.getStatus(), 'paused', 'Game state should be paused');
        assert(gamePausedEventReceived, 'Game paused event should be emitted');
        
        // Clean up
        engine.stop();
    });
    
    // Test pause when not running
    test('pause() when not running should warn', () => {
        const renderer = new MockRenderer();
        const audioManager = new MockAudioManager();
        const engine = new GameEngine(renderer, audioManager);
        
        // Should not throw error, just warn
        engine.pause();
        
        assertEquals(engine.isPaused(), false, 'Should not be paused when not running');
    });
    
    // Test resume method
    test('resume() resumes the game correctly', () => {
        const renderer = new MockRenderer();
        const audioManager = new MockAudioManager();
        const engine = new GameEngine(renderer, audioManager);
        
        let gameResumedEventReceived = false;
        engine.onGameEvent = (event) => {
            if (event.type === 'gameResumed') {
                gameResumedEventReceived = true;
            }
        };
        
        engine.start();
        engine.pause();
        engine.resume();
        
        assertEquals(engine.isGameRunning(), true, 'Game should be running');
        assertEquals(engine.isPaused(), false, 'Game should not be paused');
        assertEquals(engine.gameState.getStatus(), 'playing', 'Game state should be playing');
        assert(gameResumedEventReceived, 'Game resumed event should be emitted');
        
        // Clean up
        engine.stop();
    });
    
    // Test resume when not paused
    test('resume() when not paused should warn', () => {
        const renderer = new MockRenderer();
        const audioManager = new MockAudioManager();
        const engine = new GameEngine(renderer, audioManager);
        
        engine.start();
        
        // Should not throw error, just warn
        engine.resume();
        
        assertEquals(engine.isPaused(), false, 'Should still not be paused');
        
        // Clean up
        engine.stop();
    });
    
    // Test stop method
    test('stop() stops the game correctly', () => {
        const renderer = new MockRenderer();
        const audioManager = new MockAudioManager();
        const engine = new GameEngine(renderer, audioManager);
        
        let gameStoppedEventReceived = false;
        engine.onGameEvent = (event) => {
            if (event.type === 'gameStopped') {
                gameStoppedEventReceived = true;
            }
        };
        
        engine.start();
        engine.stop();
        
        assertEquals(engine.isGameRunning(), false, 'Game should not be running');
        assertEquals(engine.isPaused(), false, 'Game should not be paused');
        assertEquals(engine.animationFrameId, null, 'Animation frame should be cancelled');
        assert(gameStoppedEventReceived, 'Game stopped event should be emitted');
    });
    
    // Test restart method
    test('restart() restarts the game with fresh state', () => {
        const renderer = new MockRenderer();
        const audioManager = new MockAudioManager();
        const engine = new GameEngine(renderer, audioManager);
        
        let gameRestartedEventReceived = false;
        engine.onGameEvent = (event) => {
            if (event.type === 'gameRestarted') {
                gameRestartedEventReceived = true;
            }
        };
        
        // Start and modify state
        engine.start();
        const originalGameState = engine.gameState;
        engine.gameState.addScore(100);
        
        // Restart
        engine.restart();
        
        assertEquals(engine.isGameRunning(), true, 'Game should be running after restart');
        assertEquals(engine.isPaused(), false, 'Game should not be paused after restart');
        assertNotNull(engine.gameState, 'Game state should exist after restart');
        assert(engine.gameState !== originalGameState, 'Game state should be new instance');
        assertEquals(engine.gameState.getScore(), 0, 'Score should be reset to 0');
        assertEquals(engine.gameState.getStatus(), 'playing', 'Game status should be playing');
        assert(gameRestartedEventReceived, 'Game restarted event should be emitted');
        
        // Clean up
        engine.stop();
    });
    
    // Test reset method
    test('reset() resets game to initial state without starting', () => {
        const renderer = new MockRenderer();
        const audioManager = new MockAudioManager();
        const engine = new GameEngine(renderer, audioManager);
        
        engine.start();
        engine.reset();
        
        assertEquals(engine.isGameRunning(), false, 'Game should not be running after reset');
        assertEquals(engine.isPaused(), false, 'Game should not be paused after reset');
        assertEquals(engine.gameState, null, 'Game state should be null after reset');
        assertEquals(engine.animationFrameId, null, 'Animation frame should be null after reset');
        assertEquals(engine.getCurrentFps(), 60, 'FPS should be reset to 60');
    });
    
    // Test game loop performance monitoring
    test('game loop tracks FPS correctly', () => {
        const renderer = new MockRenderer();
        const audioManager = new MockAudioManager();
        const engine = new GameEngine(renderer, audioManager);
        
        assertEquals(engine.getCurrentFps(), 60, 'Initial FPS should be 60');
        
        engine.start();
        
        // FPS tracking is updated in the game loop, so we just verify the method exists
        // and returns a reasonable value
        const fps = engine.getCurrentFps();
        assert(fps >= 0 && fps <= 120, 'FPS should be in reasonable range');
        
        // Clean up
        engine.stop();
    });
    
    // Test render method calls
    test('render() calls renderer methods correctly', () => {
        const renderer = new MockRenderer();
        const audioManager = new MockAudioManager();
        const engine = new GameEngine(renderer, audioManager);
        
        engine.start();
        
        // Manually call render to test
        renderer.reset();
        engine.render();
        
        assert(renderer.clearCalled, 'Renderer clear should be called');
        assert(renderer.renderPlaceholderCalled, 'Renderer renderPlaceholder should be called');
        
        // Clean up
        engine.stop();
    });
    
    // Test input handling
    test('handleInput() processes input events when running', () => {
        const renderer = new MockRenderer();
        const audioManager = new MockAudioManager();
        const engine = new GameEngine(renderer, audioManager);
        
        engine.start();
        
        // Should not throw error
        engine.handleInput({ type: 'move', direction: 'left' });
        engine.handleInput({ type: 'rotate' });
        
        // Clean up
        engine.stop();
    });
    
    // Test input handling when paused
    test('handleInput() ignores input when paused', () => {
        const renderer = new MockRenderer();
        const audioManager = new MockAudioManager();
        const engine = new GameEngine(renderer, audioManager);
        
        engine.start();
        engine.pause();
        
        // Should not throw error and should be ignored
        engine.handleInput({ type: 'move', direction: 'left' });
        
        // Clean up
        engine.stop();
    });
    
    // Test game state getter
    test('getGameState() returns current game state', () => {
        const renderer = new MockRenderer();
        const audioManager = new MockAudioManager();
        const engine = new GameEngine(renderer, audioManager);
        
        assertEquals(engine.getGameState(), null, 'Should return null when not started');
        
        engine.start();
        const gameState = engine.getGameState();
        
        assertNotNull(gameState, 'Should return game state when started');
        assert(gameState instanceof GameState, 'Should return GameState instance');
        
        // Clean up
        engine.stop();
    });
    
    // Test game over detection
    test('isGameOver() detects game over condition correctly', () => {
        const renderer = new MockRenderer();
        const audioManager = new MockAudioManager();
        const engine = new GameEngine(renderer, audioManager);
        
        assertEquals(engine.isGameOver(), false, 'Should return false when no game state');
        
        engine.start();
        assertEquals(engine.isGameOver(), false, 'Should return false with empty field');
        
        // Simulate top row occupation by placing a puyo in the top row
        const testPuyo = new Puyo('red', 0, 0);
        engine.gameState.setFieldCell(0, 0, testPuyo);
        assertEquals(engine.isGameOver(), true, 'Should return true when top row is occupied');
        
        // Clean up
        engine.stop();
    });
    
    // Test game over handling
    test('handleGameOver() handles game over correctly', () => {
        const renderer = new MockRenderer();
        const audioManager = new MockAudioManager();
        const engine = new GameEngine(renderer, audioManager);
        
        let gameOverEventReceived = false;
        let finalScore = 0;
        
        engine.onGameEvent = (event) => {
            if (event.type === 'gameOver') {
                gameOverEventReceived = true;
                finalScore = event.finalScore;
            }
        };
        
        engine.start();
        engine.gameState.addScore(1500); // Add some score
        
        engine.handleGameOver();
        
        assertEquals(engine.isGameRunning(), false, 'Game should be stopped');
        assertEquals(engine.gameState.isGameOver(), true, 'Game state should be game over');
        assert(gameOverEventReceived, 'Game over event should be emitted');
        assertEquals(finalScore, 1500, 'Final score should be correct');
    });
    
    // Test input handling when game over
    test('handleInput() ignores input when game is over', () => {
        const renderer = new MockRenderer();
        const audioManager = new MockAudioManager();
        const engine = new GameEngine(renderer, audioManager);
        
        engine.start();
        engine.gameState.gameOver(); // Set game over state
        
        // Should not throw error and should be ignored
        engine.handleInput({ type: 'move', direction: 'left' });
        engine.handleInput({ type: 'rotate' });
        
        // Clean up
        engine.stop();
    });
    
    // Test restart from game over
    test('handleRestartFromGameOver() restarts game correctly', () => {
        const renderer = new MockRenderer();
        const audioManager = new MockAudioManager();
        const engine = new GameEngine(renderer, audioManager);
        
        engine.start();
        engine.gameState.addScore(1000);
        engine.handleGameOver();
        
        assertEquals(engine.isGameRunning(), false, 'Game should be stopped after game over');
        
        engine.handleRestartFromGameOver();
        
        assertEquals(engine.isGameRunning(), true, 'Game should be running after restart');
        assertEquals(engine.gameState.getScore(), 0, 'Score should be reset');
        assertEquals(engine.gameState.isPlaying(), true, 'Game should be playing');
        
        // Clean up
        engine.stop();
    });
    
    // Test return to menu
    test('handleReturnToMenu() returns to menu correctly', () => {
        const renderer = new MockRenderer();
        const audioManager = new MockAudioManager();
        const engine = new GameEngine(renderer, audioManager);
        
        let returnToMenuEventReceived = false;
        engine.onGameEvent = (event) => {
            if (event.type === 'returnToMenu') {
                returnToMenuEventReceived = true;
            }
        };
        
        engine.start();
        engine.handleGameOver();
        
        engine.handleReturnToMenu();
        
        assertEquals(engine.isGameRunning(), false, 'Game should not be running');
        assertEquals(engine.getGameState(), null, 'Game state should be null');
        assert(returnToMenuEventReceived, 'Return to menu event should be emitted');
    });
    
    // Test show game over screen
    test('showGameOverScreen() shows game over screen with options', () => {
        const renderer = new MockRenderer();
        const audioManager = new MockAudioManager();
        const engine = new GameEngine(renderer, audioManager);
        
        let showGameOverScreenEventReceived = false;
        let screenData = null;
        
        engine.onGameEvent = (event) => {
            if (event.type === 'showGameOverScreen') {
                showGameOverScreenEventReceived = true;
                screenData = event;
            }
        };
        
        engine.start();
        engine.gameState.addScore(2500);
        
        engine.showGameOverScreen();
        
        assert(showGameOverScreenEventReceived, 'Show game over screen event should be emitted');
        assertEquals(screenData.finalScore, 2500, 'Final score should be correct');
        assert(screenData.options.restart, 'Restart option should be available');
        assert(screenData.options.returnToMenu, 'Return to menu option should be available');
    });
    
    // Test update stops when game over
    test('update() stops updating when game is over', () => {
        const renderer = new MockRenderer();
        const audioManager = new MockAudioManager();
        const engine = new GameEngine(renderer, audioManager);
        
        engine.start();
        engine.gameState.gameOver();
        
        // Should not throw error and should not update
        engine.update(16.67);
        
        // Game should remain in game over state
        assertEquals(engine.gameState.isGameOver(), true, 'Game should remain in game over state');
        
        // Clean up
        engine.stop();
    });
    
    console.log(`GameEngine tests completed: ${testsPassed}/${testsTotal} passed`);
    return { passed: testsPassed, total: testsTotal };
}

// Run tests if this file is executed directly
if (typeof window !== 'undefined' && window.location.pathname.includes('test')) {
    runGameEngineTests();
}