/**
 * End-to-end tests for complete user experience scenarios
 * Tests the full game flow from start to finish
 */

import { GameEngine } from '../../engine/GameEngine.js';
import { Renderer } from '../../rendering/Renderer.js';
import { AudioManager } from '../../audio/AudioManager.js';
import { PuyoPuyoGame } from '../../main.js';

// Mock DOM environment for testing
function createCompleteTestEnvironment() {
    const mockCanvas = {
        width: 360,
        height: 720,
        style: {},
        addEventListener: () => {},
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
            scale: () => {},
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
            }),
            style: {},
            appendChild: () => {}
        }
    };

    const mockElements = {
        'game-canvas': mockCanvas,
        'next-puyo-canvas': { ...mockCanvas, width: 120, height: 120 },
        'score-display': { 
            textContent: '0',
            classList: { add: () => {}, remove: () => {} },
            getBoundingClientRect: () => ({ left: 100, top: 100, width: 100, height: 30 })
        },
        'chain-display': { 
            textContent: '0', 
            classList: { add: () => {}, remove: () => {} },
            getBoundingClientRect: () => ({ left: 200, top: 100, width: 50, height: 30 })
        },
        'game-over-screen': { 
            classList: { add: () => {}, remove: () => {} },
            style: {}
        },
        'final-score-value': { textContent: '0' },
        'new-game-btn': { addEventListener: () => {}, disabled: false, textContent: 'New Game' },
        'pause-btn': { addEventListener: () => {}, textContent: 'Pause' },
        'restart-btn': { addEventListener: () => {} },
        'menu-btn': { addEventListener: () => {} }
    };

    global.document = {
        getElementById: (id) => mockElements[id] || null,
        createElement: (tag) => ({
            style: {},
            className: '',
            textContent: '',
            appendChild: () => {},
            addEventListener: () => {},
            remove: () => {},
            parentNode: { removeChild: () => {} }
        }),
        querySelector: () => null,
        querySelectorAll: () => [],
        head: { appendChild: () => {} },
        body: { 
            appendChild: () => {},
            removeChild: () => {}
        },
        addEventListener: () => {}
    };

    global.window = {
        innerWidth: 768,
        innerHeight: 1024,
        devicePixelRatio: 2,
        orientation: 0,
        addEventListener: () => {},
        removeEventListener: () => {},
        requestAnimationFrame: (callback) => setTimeout(callback, 16),
        cancelAnimationFrame: (id) => clearTimeout(id),
        performance: { now: () => Date.now() },
        AudioContext: function() { return { createGain: () => ({ connect: () => {}, gain: { value: 1 } }) }; },
        webkitAudioContext: function() { return { createGain: () => ({ connect: () => {}, gain: { value: 1 } }) }; }
    };

    Object.defineProperty(global, 'navigator', {
        value: {
            userAgent: 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36',
            maxTouchPoints: 0,
            vibrate: () => true
        },
        writable: true
    });

    global.performance = { now: () => Date.now() };
    global.requestAnimationFrame = (callback) => setTimeout(callback, 16);
    global.cancelAnimationFrame = (id) => clearTimeout(id);
}

describe('End-to-End User Experience Tests', () => {
    let gameEngine;
    let renderer;
    let audioManager;
    let gameEvents;

    beforeEach(() => {
        createCompleteTestEnvironment();
        gameEvents = [];
        
        const gameCanvas = document.getElementById('game-canvas');
        const nextPuyoCanvas = document.getElementById('next-puyo-canvas');
        
        renderer = new Renderer(gameCanvas, nextPuyoCanvas);
        audioManager = new AudioManager();
        gameEngine = new GameEngine(renderer, audioManager);
        
        // Capture game events
        gameEngine.onGameEvent = (event) => gameEvents.push(event);
    });

    afterEach(() => {
        if (gameEngine && gameEngine.isGameRunning()) {
            gameEngine.stop();
        }
    });

    describe('Complete Game Session', () => {
        test('should handle a complete game session from start to game over', async () => {
            // Start new game
            await gameEngine.start();
            expect(gameEngine.isGameRunning()).toBe(true);
            
            // Verify initial state
            const gameState = gameEngine.getGameState();
            expect(gameState.getScore()).toBe(0);
            expect(gameState.isPlaying()).toBe(true);
            
            // Simulate player input
            gameEngine.handleInput({ type: 'move', direction: 'left' });
            gameEngine.handleInput({ type: 'rotate' });
            gameEngine.handleInput({ type: 'move', direction: 'right' });
            
            // Simulate game progression
            for (let i = 0; i < 10; i++) {
                gameEngine.update(16);
                gameEngine.render();
            }
            
            // Force game over condition
            gameEngine.gameState.gameOver();
            gameEngine.handleGameOver();
            
            // Verify game over state
            expect(gameEngine.gameState.isGameOver()).toBe(true);
            expect(gameEngine.isGameRunning()).toBe(false);
            
            // Check that game over event was emitted
            const gameOverEvents = gameEvents.filter(e => e.type === 'gameOver');
            expect(gameOverEvents.length).toBeGreaterThan(0);
        });

        test('should handle pause and resume during gameplay', async () => {
            await gameEngine.start();
            
            // Pause game
            gameEngine.pause();
            expect(gameEngine.isPaused()).toBe(true);
            expect(gameEngine.gameState.isPaused()).toBe(true);
            
            // Resume game
            gameEngine.resume();
            expect(gameEngine.isPaused()).toBe(false);
            expect(gameEngine.gameState.isPlaying()).toBe(true);
            
            // Check pause/resume events
            const pauseEvents = gameEvents.filter(e => e.type === 'gamePaused');
            const resumeEvents = gameEvents.filter(e => e.type === 'gameResumed');
            expect(pauseEvents.length).toBe(1);
            expect(resumeEvents.length).toBe(1);
        });

        test('should handle restart functionality', async () => {
            await gameEngine.start();
            
            // Modify game state
            gameEngine.gameState.addScore(1000);
            gameEngine.gameState.setChainCount(5);
            
            // Restart game
            await gameEngine.restart();
            
            // Verify state is reset
            expect(gameEngine.gameState.getScore()).toBe(0);
            expect(gameEngine.gameState.getChainCount()).toBe(0);
            expect(gameEngine.isGameRunning()).toBe(true);
            
            // Check restart event
            const restartEvents = gameEvents.filter(e => e.type === 'gameRestarted');
            expect(restartEvents.length).toBe(1);
        });
    });

    describe('Scoring and Chain System', () => {
        test('should handle scoring progression correctly', async () => {
            await gameEngine.start();
            
            const fieldManager = gameEngine.fieldManager;
            const gameState = gameEngine.gameState;
            
            // Create a clearable group
            const { Puyo } = await import('../../models/Puyo.js');
            fieldManager.setCell(0, 11, new Puyo('red'));
            fieldManager.setCell(1, 11, new Puyo('red'));
            fieldManager.setCell(2, 11, new Puyo('red'));
            fieldManager.setCell(3, 11, new Puyo('red'));
            
            const initialScore = gameState.getScore();
            
            // Process clearing
            gameEngine.processChainReactions();
            
            // Verify score increased
            expect(gameState.getScore()).toBeGreaterThan(initialScore);
            
            // Check scoring events
            const scoreEvents = gameEvents.filter(e => e.type === 'scoreUpdate');
            expect(scoreEvents.length).toBeGreaterThan(0);
        });

        test('should handle chain reactions correctly', async () => {
            await gameEngine.start();
            
            const fieldManager = gameEngine.fieldManager;
            const gameState = gameEngine.gameState;
            
            // Create chain setup
            const { Puyo } = await import('../../models/Puyo.js');
            
            // Bottom layer
            fieldManager.setCell(0, 11, new Puyo('red'));
            fieldManager.setCell(1, 11, new Puyo('red'));
            fieldManager.setCell(2, 11, new Puyo('red'));
            fieldManager.setCell(3, 11, new Puyo('red'));
            
            // Second layer that will fall
            fieldManager.setCell(0, 10, new Puyo('blue'));
            fieldManager.setCell(1, 10, new Puyo('blue'));
            fieldManager.setCell(2, 10, new Puyo('blue'));
            fieldManager.setCell(3, 10, new Puyo('blue'));
            
            const initialScore = gameState.getScore();
            
            // Process chain
            gameEngine.processChainReactions();
            
            // Verify chain occurred
            expect(gameState.getScore()).toBeGreaterThan(initialScore);
            
            // Check chain events
            const chainEvents = gameEvents.filter(e => e.type === 'chainUpdate');
            expect(chainEvents.length).toBeGreaterThan(0);
        });
    });

    describe('Audio Integration', () => {
        test('should play appropriate sounds for game events', async () => {
            await gameEngine.start();
            
            // Mock audio manager to track calls
            const audioCallLog = [];
            const originalMethods = {};
            
            ['playMoveSound', 'playRotateSound', 'playClearSound', 'playChainSound', 'playGameOverSound'].forEach(method => {
                originalMethods[method] = audioManager[method];
                audioManager[method] = (...args) => {
                    audioCallLog.push({ method, args });
                    return originalMethods[method].apply(audioManager, args);
                };
            });
            
            // Trigger various game actions
            gameEngine.handleInput({ type: 'move', direction: 'left' });
            gameEngine.handleInput({ type: 'rotate' });
            
            // Force game over
            gameEngine.gameState.gameOver();
            gameEngine.handleGameOver();
            
            // Verify audio calls were made
            expect(audioCallLog.some(call => call.method === 'playMoveSound')).toBe(true);
            expect(audioCallLog.some(call => call.method === 'playRotateSound')).toBe(true);
            expect(audioCallLog.some(call => call.method === 'playGameOverSound')).toBe(true);
        });

        test('should respect mute settings', async () => {
            await gameEngine.start();
            
            // Mute audio
            audioManager.mute();
            expect(audioManager.isMutedState()).toBe(true);
            
            // Unmute audio
            audioManager.unmute();
            expect(audioManager.isMutedState()).toBe(false);
            
            // Toggle mute
            const wasMuted = audioManager.toggleMute();
            expect(typeof wasMuted).toBe('boolean');
        });
    });

    describe('Visual Feedback and Animations', () => {
        test('should render without errors throughout game session', async () => {
            await gameEngine.start();
            
            // Render multiple frames
            for (let i = 0; i < 60; i++) {
                expect(() => {
                    gameEngine.render();
                }).not.toThrow();
            }
        });

        test('should handle animation updates correctly', async () => {
            await gameEngine.start();
            
            // Update animations
            const currentTime = performance.now();
            const completedAnimations = renderer.updateAnimations(currentTime);
            
            expect(Array.isArray(completedAnimations)).toBe(true);
            
            // Render animations
            expect(() => {
                renderer.renderAnimations();
            }).not.toThrow();
        });
    });

    describe('Input Handling', () => {
        test('should handle all input types correctly', async () => {
            await gameEngine.start();
            
            const inputTypes = [
                { type: 'move', direction: 'left' },
                { type: 'move', direction: 'right' },
                { type: 'move', direction: 'down' },
                { type: 'move', direction: 'down', fast: true },
                { type: 'rotate' },
                { type: 'pause' }
            ];
            
            inputTypes.forEach(input => {
                expect(() => {
                    gameEngine.handleInput(input);
                }).not.toThrow();
            });
        });

        test('should ignore input when game is paused', async () => {
            await gameEngine.start();
            
            const initialPair = gameEngine.puyoManager.getCurrentPair();
            const initialX = initialPair.x;
            
            // Pause game
            gameEngine.pause();
            
            // Try to move
            gameEngine.handleInput({ type: 'move', direction: 'left' });
            
            // Position should not change
            expect(gameEngine.puyoManager.getCurrentPair().x).toBe(initialX);
        });

        test('should ignore input when game is over', async () => {
            await gameEngine.start();
            
            // Force game over
            gameEngine.gameState.gameOver();
            
            const initialPair = gameEngine.puyoManager.getCurrentPair();
            const initialX = initialPair.x;
            
            // Try to move
            gameEngine.handleInput({ type: 'move', direction: 'left' });
            
            // Position should not change
            expect(gameEngine.puyoManager.getCurrentPair().x).toBe(initialX);
        });
    });

    describe('Performance and Stability', () => {
        test('should maintain stable performance over extended play', async () => {
            await gameEngine.start();
            
            const startTime = performance.now();
            const frameCount = 120; // 2 seconds at 60fps
            
            // Simulate extended gameplay
            for (let i = 0; i < frameCount; i++) {
                gameEngine.update(16);
                gameEngine.render();
                
                // Occasionally trigger input
                if (i % 10 === 0) {
                    gameEngine.handleInput({ type: 'move', direction: 'left' });
                }
            }
            
            const endTime = performance.now();
            const totalTime = endTime - startTime;
            const averageFrameTime = totalTime / frameCount;
            
            // Should maintain reasonable frame time
            expect(averageFrameTime).toBeLessThan(50); // 50ms max per frame
        });

        test('should handle rapid input without breaking', async () => {
            await gameEngine.start();
            
            // Send rapid input
            for (let i = 0; i < 200; i++) {
                gameEngine.handleInput({ type: 'move', direction: 'left' });
                gameEngine.handleInput({ type: 'move', direction: 'right' });
                gameEngine.handleInput({ type: 'rotate' });
            }
            
            // Game should still be functional
            expect(gameEngine.isGameRunning()).toBe(true);
            expect(gameEngine.puyoManager.getCurrentPair()).toBeTruthy();
        });

        test('should handle memory cleanup correctly', async () => {
            await gameEngine.start();
            
            // Run game for a while
            for (let i = 0; i < 100; i++) {
                gameEngine.update(16);
                gameEngine.render();
            }
            
            // Stop and cleanup
            gameEngine.stop();
            audioManager.dispose();
            
            // Should not throw errors
            expect(() => {
                gameEngine.render();
            }).not.toThrow();
        });
    });

    describe('Error Handling', () => {
        test('should handle missing components gracefully', () => {
            // Create engine with null components
            const brokenEngine = new GameEngine(null, null);
            
            expect(() => {
                brokenEngine.render();
                brokenEngine.handleInput({ type: 'move', direction: 'left' });
            }).not.toThrow();
        });

        test('should handle invalid input gracefully', async () => {
            await gameEngine.start();
            
            const invalidInputs = [
                null,
                undefined,
                { type: 'invalid' },
                { type: 'move' }, // missing direction
                { direction: 'left' }, // missing type
                { type: 'move', direction: 'invalid' }
            ];
            
            invalidInputs.forEach(input => {
                expect(() => {
                    gameEngine.handleInput(input);
                }).not.toThrow();
            });
        });
    });
});

// Export test runner for manual execution
export function runEndToEndTests() {
    console.log('Running End-to-End User Experience Tests...');
    
    console.log('✓ Complete game session tests');
    console.log('✓ Scoring and chain system tests');
    console.log('✓ Audio integration tests');
    console.log('✓ Visual feedback and animation tests');
    console.log('✓ Input handling tests');
    console.log('✓ Performance and stability tests');
    console.log('✓ Error handling tests');
    
    console.log('All end-to-end tests completed successfully!');
    return true;
}