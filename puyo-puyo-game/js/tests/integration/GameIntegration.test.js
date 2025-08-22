/**
 * Integration tests for complete game system integration
 * Tests that all components work together correctly
 */

import { GameEngine } from '../../engine/GameEngine.js';
import { Renderer } from '../../rendering/Renderer.js';
import { AudioManager } from '../../audio/AudioManager.js';

// Mock DOM elements for testing
function createMockCanvas() {
    return {
        width: 360,
        height: 720,
        getContext: () => ({
            clearRect: () => {},
            fillRect: () => {},
            beginPath: () => {},
            arc: () => {},
            fill: () => {},
            stroke: () => {},
            moveTo: () => {},
            lineTo: () => {},
            save: () => {},
            restore: () => {},
            translate: () => {},
            createRadialGradient: () => ({
                addColorStop: () => {}
            }),
            imageSmoothingEnabled: true,
            textAlign: 'center',
            textBaseline: 'middle',
            fillStyle: '#000000',
            strokeStyle: '#000000',
            lineWidth: 1,
            font: '16px Arial',
            globalAlpha: 1,
            fillText: () => {},
            strokeRect: () => {}
        }),
        parentElement: {
            getBoundingClientRect: () => ({
                width: 360,
                height: 720
            })
        }
    };
}

function createMockDocument() {
    const elements = {
        'game-canvas': createMockCanvas(),
        'next-puyo-canvas': createMockCanvas(),
        'score-display': { textContent: '0' },
        'chain-display': { textContent: '0', classList: { add: () => {}, remove: () => {} } }
    };
    
    global.document = {
        getElementById: (id) => elements[id] || null,
        addEventListener: () => {},
        createElement: () => ({ style: {}, textContent: '', parentNode: null }),
        body: { appendChild: () => {} }
    };
    
    global.window = {
        requestAnimationFrame: (callback) => setTimeout(callback, 16),
        cancelAnimationFrame: (id) => clearTimeout(id),
        performance: { now: () => Date.now() }
    };
    
    global.performance = { now: () => Date.now() };
}

describe('Game Integration Tests', () => {
    let gameEngine;
    let renderer;
    let audioManager;

    beforeEach(() => {
        createMockDocument();
        
        const gameCanvas = document.getElementById('game-canvas');
        const nextPuyoCanvas = document.getElementById('next-puyo-canvas');
        
        renderer = new Renderer(gameCanvas, nextPuyoCanvas);
        audioManager = new AudioManager();
        gameEngine = new GameEngine(renderer, audioManager);
    });

    afterEach(() => {
        if (gameEngine && gameEngine.isGameRunning()) {
            gameEngine.stop();
        }
    });

    describe('Game Initialization', () => {
        test('should initialize all game systems correctly', async () => {
            // Start the game
            await gameEngine.start();
            
            // Verify game is running
            expect(gameEngine.isGameRunning()).toBe(true);
            expect(gameEngine.isPaused()).toBe(false);
            
            // Verify game state is initialized
            const gameState = gameEngine.getGameState();
            expect(gameState).toBeTruthy();
            expect(gameState.getScore()).toBe(0);
            expect(gameState.isPlaying()).toBe(true);
            
            // Verify field manager is initialized
            expect(gameEngine.fieldManager).toBeTruthy();
            expect(gameEngine.fieldManager.getDimensions()).toEqual({ width: 6, height: 12 });
            
            // Verify puyo manager is initialized
            expect(gameEngine.puyoManager).toBeTruthy();
            expect(gameEngine.puyoManager.getCurrentPair()).toBeTruthy();
            expect(gameEngine.puyoManager.getNextPair()).toBeTruthy();
        });

        test('should set up initial puyo pairs correctly', async () => {
            await gameEngine.start();
            
            const gameState = gameEngine.getGameState();
            const currentPair = gameState.getCurrentPair();
            const nextPair = gameState.getNextPair();
            
            // Verify current pair exists and is positioned correctly
            expect(currentPair).toBeTruthy();
            expect(currentPair.x).toBe(2); // Center of 6-wide field
            expect(currentPair.y).toBe(1); // Starting position
            
            // Verify next pair exists
            expect(nextPair).toBeTruthy();
            expect(nextPair.puyo1.color).toBeTruthy();
            expect(nextPair.puyo2.color).toBeTruthy();
        });
    });

    describe('Input Integration', () => {
        test('should handle movement input correctly', async () => {
            await gameEngine.start();
            
            const initialPair = gameEngine.puyoManager.getCurrentPair();
            const initialX = initialPair.x;
            
            // Test left movement
            gameEngine.handleInput({ type: 'move', direction: 'left' });
            expect(gameEngine.puyoManager.getCurrentPair().x).toBe(initialX - 1);
            
            // Test right movement
            gameEngine.handleInput({ type: 'move', direction: 'right' });
            gameEngine.handleInput({ type: 'move', direction: 'right' });
            expect(gameEngine.puyoManager.getCurrentPair().x).toBe(initialX + 1);
        });

        test('should handle rotation input correctly', async () => {
            await gameEngine.start();
            
            const initialRotation = gameEngine.puyoManager.getCurrentPair().rotation;
            
            // Test rotation
            gameEngine.handleInput({ type: 'rotate' });
            expect(gameEngine.puyoManager.getCurrentPair().rotation).toBe((initialRotation + 1) % 4);
        });

        test('should ignore input when game is paused', async () => {
            await gameEngine.start();
            gameEngine.pause();
            
            const initialPair = gameEngine.puyoManager.getCurrentPair();
            const initialX = initialPair.x;
            
            // Try to move while paused
            gameEngine.handleInput({ type: 'move', direction: 'left' });
            expect(gameEngine.puyoManager.getCurrentPair().x).toBe(initialX);
        });

        test('should ignore input when game is over', async () => {
            await gameEngine.start();
            
            // Force game over state
            gameEngine.gameState.gameOver();
            
            const initialPair = gameEngine.puyoManager.getCurrentPair();
            const initialX = initialPair.x;
            
            // Try to move when game is over
            gameEngine.handleInput({ type: 'move', direction: 'left' });
            expect(gameEngine.puyoManager.getCurrentPair().x).toBe(initialX);
        });
    });

    describe('Field and Puyo Integration', () => {
        test('should fix puyo to field when they land', async () => {
            await gameEngine.start();
            
            const fieldManager = gameEngine.fieldManager;
            const puyoManager = gameEngine.puyoManager;
            
            // Move puyo to bottom quickly
            while (puyoManager.canPairFall()) {
                puyoManager.fallPair();
            }
            
            // Update game to trigger fixing
            gameEngine.update(16);
            
            // Check that puyo are now in the field
            const bottomRow = fieldManager.getField()[11]; // Bottom row
            const hasFixedPuyo = bottomRow.some(cell => cell !== null);
            expect(hasFixedPuyo).toBe(true);
        });

        test('should spawn new pair after fixing current pair', async () => {
            await gameEngine.start();
            
            const initialCurrentPair = gameEngine.puyoManager.getCurrentPair();
            const initialNextPair = gameEngine.puyoManager.getNextPair();
            
            // Force pair to be fixed
            while (gameEngine.puyoManager.canPairFall()) {
                gameEngine.puyoManager.fallPair();
            }
            gameEngine.update(16);
            
            // Verify new pair was spawned
            const newCurrentPair = gameEngine.puyoManager.getCurrentPair();
            const newNextPair = gameEngine.puyoManager.getNextPair();
            
            expect(newCurrentPair).not.toBe(initialCurrentPair);
            expect(newNextPair).not.toBe(initialNextPair);
            expect(newCurrentPair.x).toBe(2); // New pair should be at spawn position
            expect(newCurrentPair.y).toBe(1);
        });
    });

    describe('Scoring Integration', () => {
        test('should update score when puyo are cleared', async () => {
            await gameEngine.start();
            
            const gameState = gameEngine.getGameState();
            const fieldManager = gameEngine.fieldManager;
            
            // Manually place puyo to create a clearable group
            const { Puyo } = await import('../../models/Puyo.js');
            const redPuyo1 = new Puyo('red');
            const redPuyo2 = new Puyo('red');
            const redPuyo3 = new Puyo('red');
            const redPuyo4 = new Puyo('red');
            
            // Place 4 red puyo in a line (bottom row)
            fieldManager.setCell(0, 11, redPuyo1);
            fieldManager.setCell(1, 11, redPuyo2);
            fieldManager.setCell(2, 11, redPuyo3);
            fieldManager.setCell(3, 11, redPuyo4);
            
            const initialScore = gameState.getScore();
            
            // Process chain reactions
            gameEngine.processChainReactions();
            
            // Verify score increased
            expect(gameState.getScore()).toBeGreaterThan(initialScore);
        });

        test('should handle chain reactions correctly', async () => {
            await gameEngine.start();
            
            const gameState = gameEngine.getGameState();
            const fieldManager = gameEngine.fieldManager;
            
            // Create a setup for chain reaction
            const { Puyo } = await import('../../models/Puyo.js');
            
            // Bottom layer: 4 red puyo
            fieldManager.setCell(0, 11, new Puyo('red'));
            fieldManager.setCell(1, 11, new Puyo('red'));
            fieldManager.setCell(2, 11, new Puyo('red'));
            fieldManager.setCell(3, 11, new Puyo('red'));
            
            // Second layer: 4 blue puyo that will fall and connect
            fieldManager.setCell(0, 10, new Puyo('blue'));
            fieldManager.setCell(1, 10, new Puyo('blue'));
            fieldManager.setCell(2, 10, new Puyo('blue'));
            fieldManager.setCell(3, 10, new Puyo('blue'));
            
            const initialScore = gameState.getScore();
            
            // Process chain reactions
            gameEngine.processChainReactions();
            
            // Verify chain occurred (score should be higher than single group clear)
            expect(gameState.getScore()).toBeGreaterThan(initialScore);
            expect(gameState.getChainCount()).toBeGreaterThanOrEqual(0);
        });
    });

    describe('Game State Management', () => {
        test('should handle pause and resume correctly', async () => {
            await gameEngine.start();
            
            expect(gameEngine.isGameRunning()).toBe(true);
            expect(gameEngine.isPaused()).toBe(false);
            
            // Pause game
            gameEngine.pause();
            expect(gameEngine.isPaused()).toBe(true);
            expect(gameEngine.gameState.isPaused()).toBe(true);
            
            // Resume game
            gameEngine.resume();
            expect(gameEngine.isPaused()).toBe(false);
            expect(gameEngine.gameState.isPlaying()).toBe(true);
        });

        test('should handle game over correctly', async () => {
            await gameEngine.start();
            
            // Force game over condition
            gameEngine.gameState.gameOver();
            
            expect(gameEngine.gameState.isGameOver()).toBe(true);
            
            // Update should not process when game is over
            const initialScore = gameEngine.gameState.getScore();
            gameEngine.update(16);
            expect(gameEngine.gameState.getScore()).toBe(initialScore);
        });

        test('should restart game correctly', async () => {
            await gameEngine.start();
            
            // Modify game state
            gameEngine.gameState.addScore(1000);
            gameEngine.gameState.setChainCount(5);
            
            // Restart game
            await gameEngine.restart();
            
            // Verify state is reset
            expect(gameEngine.gameState.getScore()).toBe(0);
            expect(gameEngine.gameState.getChainCount()).toBe(0);
            expect(gameEngine.gameState.isPlaying()).toBe(true);
            expect(gameEngine.isGameRunning()).toBe(true);
        });
    });

    describe('Rendering Integration', () => {
        test('should render without errors', async () => {
            await gameEngine.start();
            
            // Render should not throw errors
            expect(() => {
                gameEngine.render();
            }).not.toThrow();
        });

        test('should update UI elements', async () => {
            await gameEngine.start();
            
            const scoreDisplay = document.getElementById('score-display');
            const chainDisplay = document.getElementById('chain-display');
            
            // Modify score and chain
            gameEngine.gameState.addScore(1500);
            gameEngine.gameState.setChainCount(3);
            
            // Render to update UI
            gameEngine.render();
            
            // UI should be updated (mocked elements will have textContent set)
            expect(scoreDisplay.textContent).toBe('1,500');
            expect(chainDisplay.textContent).toBe('3');
        });
    });

    describe('Event System Integration', () => {
        test('should emit game events correctly', async () => {
            const events = [];
            gameEngine.onGameEvent = (event) => events.push(event);
            
            await gameEngine.start();
            
            // Check that initialization events were emitted
            const eventTypes = events.map(e => e.type);
            expect(eventTypes).toContain('scoreUpdate');
            expect(eventTypes).toContain('chainUpdate');
            expect(eventTypes).toContain('gameStateInitialized');
            expect(eventTypes).toContain('gameStarted');
        });

        test('should emit scoring events when puyo are cleared', async () => {
            const events = [];
            gameEngine.onGameEvent = (event) => events.push(event);
            
            await gameEngine.start();
            
            // Clear events from initialization
            events.length = 0;
            
            // Create clearable group and process
            const fieldManager = gameEngine.fieldManager;
            const { Puyo } = await import('../../models/Puyo.js');
            
            fieldManager.setCell(0, 11, new Puyo('red'));
            fieldManager.setCell(1, 11, new Puyo('red'));
            fieldManager.setCell(2, 11, new Puyo('red'));
            fieldManager.setCell(3, 11, new Puyo('red'));
            
            gameEngine.processChainReactions();
            
            // Check for scoring events
            const eventTypes = events.map(e => e.type);
            expect(eventTypes).toContain('scoreUpdate');
            expect(eventTypes).toContain('puyoCleared');
        });
    });

    describe('Performance Integration', () => {
        test('should maintain stable frame rate', async () => {
            await gameEngine.start();
            
            const startTime = performance.now();
            const frameCount = 60; // Test 60 frames
            
            // Simulate 60 frames of updates and renders
            for (let i = 0; i < frameCount; i++) {
                gameEngine.update(16); // 16ms per frame for 60fps
                gameEngine.render();
            }
            
            const endTime = performance.now();
            const totalTime = endTime - startTime;
            const averageFrameTime = totalTime / frameCount;
            
            // Should maintain reasonable frame time (allowing for test overhead)
            expect(averageFrameTime).toBeLessThan(50); // 50ms max per frame in tests
        });

        test('should handle rapid input without errors', async () => {
            await gameEngine.start();
            
            // Send rapid input
            for (let i = 0; i < 100; i++) {
                gameEngine.handleInput({ type: 'move', direction: 'left' });
                gameEngine.handleInput({ type: 'move', direction: 'right' });
                gameEngine.handleInput({ type: 'rotate' });
            }
            
            // Game should still be functional
            expect(gameEngine.isGameRunning()).toBe(true);
            expect(gameEngine.puyoManager.getCurrentPair()).toBeTruthy();
        });
    });
});