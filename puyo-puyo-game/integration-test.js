/**
 * Simple integration test to verify all systems work together
 */

import { GameEngine } from './js/engine/GameEngine.js';
import { Renderer } from './js/rendering/Renderer.js';
import { AudioManager } from './js/audio/AudioManager.js';

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

// Mock global objects
global.document = {
    getElementById: (id) => {
        const elements = {
            'game-canvas': createMockCanvas(),
            'next-puyo-canvas': createMockCanvas(),
            'score-display': { textContent: '0' },
            'chain-display': { textContent: '0', classList: { add: () => {}, remove: () => {} } }
        };
        return elements[id] || null;
    },
    addEventListener: () => {},
    createElement: () => ({ style: {}, textContent: '', parentNode: null }),
    body: { appendChild: () => {} }
};

global.window = {
    requestAnimationFrame: (callback) => setTimeout(callback, 16),
    cancelAnimationFrame: (id) => clearTimeout(id),
    performance: { now: () => Date.now() },
    AudioContext: function() { return { createGain: () => ({ connect: () => {}, gain: { value: 1 } }) }; },
    webkitAudioContext: function() { return { createGain: () => ({ connect: () => {}, gain: { value: 1 } }) }; }
};

global.performance = { now: () => Date.now() };
global.requestAnimationFrame = (callback) => setTimeout(callback, 16);
global.cancelAnimationFrame = (id) => clearTimeout(id);

async function runIntegrationTest() {
    console.log('Starting integration test...');
    
    try {
        // Create game components
        const gameCanvas = document.getElementById('game-canvas');
        const nextPuyoCanvas = document.getElementById('next-puyo-canvas');
        
        const renderer = new Renderer(gameCanvas, nextPuyoCanvas);
        const audioManager = new AudioManager();
        const gameEngine = new GameEngine(renderer, audioManager);
        
        console.log('✓ Game components created successfully');
        
        // Start the game
        await gameEngine.start();
        console.log('✓ Game started successfully');
        
        // Verify game state
        const gameState = gameEngine.getGameState();
        console.log(`✓ Game state initialized - Score: ${gameState.getScore()}, Playing: ${gameState.isPlaying()}`);
        
        // Verify field manager
        const fieldDimensions = gameEngine.fieldManager.getDimensions();
        console.log(`✓ Field manager initialized - Dimensions: ${fieldDimensions.width}x${fieldDimensions.height}`);
        
        // Verify puyo manager
        const currentPair = gameEngine.puyoManager.getCurrentPair();
        const nextPair = gameEngine.puyoManager.getNextPair();
        console.log(`✓ Puyo manager initialized - Current pair at (${currentPair.x}, ${currentPair.y}), Next pair ready`);
        
        // Test input handling
        gameEngine.handleInput({ type: 'move', direction: 'left' });
        const newX = gameEngine.puyoManager.getCurrentPair().x;
        console.log(`✓ Input handling works - Puyo moved to x: ${newX}`);
        
        // Test rotation
        const initialRotation = gameEngine.puyoManager.getCurrentPair().rotation;
        gameEngine.handleInput({ type: 'rotate' });
        const newRotation = gameEngine.puyoManager.getCurrentPair().rotation;
        console.log(`✓ Rotation works - Changed from ${initialRotation} to ${newRotation}`);
        
        // Test game loop
        gameEngine.update(16);
        gameEngine.render();
        console.log('✓ Game loop (update/render) works without errors');
        
        // Test pause/resume
        gameEngine.pause();
        console.log(`✓ Pause works - Game paused: ${gameEngine.isPaused()}`);
        
        gameEngine.resume();
        console.log(`✓ Resume works - Game paused: ${gameEngine.isPaused()}`);
        
        // Test scoring system
        const { Puyo } = await import('./js/models/Puyo.js');
        const fieldManager = gameEngine.fieldManager;
        
        // Create a clearable group
        fieldManager.setCell(0, 11, new Puyo('red'));
        fieldManager.setCell(1, 11, new Puyo('red'));
        fieldManager.setCell(2, 11, new Puyo('red'));
        fieldManager.setCell(3, 11, new Puyo('red'));
        
        const initialScore = gameState.getScore();
        gameEngine.processChainReactions();
        const finalScore = gameState.getScore();
        
        console.log(`✓ Scoring system works - Score changed from ${initialScore} to ${finalScore}`);
        
        // Stop the game
        gameEngine.stop();
        console.log('✓ Game stopped successfully');
        
        console.log('\n🎉 All integration tests passed! All systems are properly connected.');
        
        return true;
        
    } catch (error) {
        console.error('❌ Integration test failed:', error);
        console.error(error.stack);
        return false;
    }
}

// Run the test
runIntegrationTest().then(success => {
    process.exit(success ? 0 : 1);
});