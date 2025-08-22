/**
 * Main entry point for the Puyo Puyo game
 * Initializes the game engine and handles the main game loop
 */

import { GameEngine } from './engine/GameEngine.js';
import { InputHandler } from './input/InputHandler.js';
import { Renderer } from './rendering/Renderer.js';
import { AudioManager } from './audio/AudioManager.js';

class PuyoPuyoGame {
    constructor() {
        this.gameEngine = null;
        this.inputHandler = null;
        this.renderer = null;
        this.audioManager = null;
        this.isInitialized = false;
    }

    /**
     * Initialize the game components
     */
    async init() {
        try {
            // Get canvas elements
            const gameCanvas = document.getElementById('game-canvas');
            const nextPuyoCanvas = document.getElementById('next-puyo-canvas');
            
            if (!gameCanvas || !nextPuyoCanvas) {
                throw new Error('Required canvas elements not found');
            }

            // Initialize renderer
            this.renderer = new Renderer(gameCanvas, nextPuyoCanvas);
            
            // Initialize audio manager
            this.audioManager = new AudioManager();
            
            // Initialize game engine
            this.gameEngine = new GameEngine(this.renderer, this.audioManager);
            
            // Initialize input handler
            this.inputHandler = new InputHandler(this.gameEngine);
            
            // Setup UI event listeners
            this.setupUIEventListeners();
            
            this.isInitialized = true;
            console.log('Puyo Puyo game initialized successfully');
            
        } catch (error) {
            console.error('Failed to initialize game:', error);
            this.showError('ゲームの初期化に失敗しました。ページを再読み込みしてください。');
        }
    }

    /**
     * Setup event listeners for UI buttons
     */
    setupUIEventListeners() {
        // New game button
        const newGameBtn = document.getElementById('new-game-btn');
        if (newGameBtn) {
            newGameBtn.addEventListener('click', () => this.startNewGame());
        }

        // Pause button
        const pauseBtn = document.getElementById('pause-btn');
        if (pauseBtn) {
            pauseBtn.addEventListener('click', () => this.togglePause());
        }

        // Restart button (game over screen)
        const restartBtn = document.getElementById('restart-btn');
        if (restartBtn) {
            restartBtn.addEventListener('click', () => this.restartGame());
        }

        // Menu button (game over screen)
        const menuBtn = document.getElementById('menu-btn');
        if (menuBtn) {
            menuBtn.addEventListener('click', () => this.returnToMenu());
        }

        // Mobile control buttons
        this.setupMobileControls();
    }

    /**
     * Setup mobile control buttons
     */
    setupMobileControls() {
        const leftBtn = document.getElementById('left-btn');
        const rightBtn = document.getElementById('right-btn');
        const downBtn = document.getElementById('down-btn');
        const rotateBtn = document.getElementById('rotate-btn');

        if (leftBtn) {
            leftBtn.addEventListener('touchstart', (e) => {
                e.preventDefault();
                this.gameEngine?.handleInput({ type: 'move', direction: 'left' });
            });
        }

        if (rightBtn) {
            rightBtn.addEventListener('touchstart', (e) => {
                e.preventDefault();
                this.gameEngine?.handleInput({ type: 'move', direction: 'right' });
            });
        }

        if (downBtn) {
            downBtn.addEventListener('touchstart', (e) => {
                e.preventDefault();
                this.gameEngine?.handleInput({ type: 'move', direction: 'down' });
            });
        }

        if (rotateBtn) {
            rotateBtn.addEventListener('touchstart', (e) => {
                e.preventDefault();
                this.gameEngine?.handleInput({ type: 'rotate' });
            });
        }
    }

    /**
     * Start a new game
     */
    startNewGame() {
        if (!this.isInitialized) {
            console.warn('Game not initialized yet');
            return;
        }

        this.hideGameOverScreen();
        this.gameEngine.start();
        this.updatePauseButton(false);
    }

    /**
     * Toggle game pause state
     */
    togglePause() {
        if (!this.isInitialized || !this.gameEngine.isRunning()) {
            return;
        }

        if (this.gameEngine.isPaused()) {
            this.gameEngine.resume();
            this.updatePauseButton(false);
        } else {
            this.gameEngine.pause();
            this.updatePauseButton(true);
        }
    }

    /**
     * Restart the game from game over screen
     */
    restartGame() {
        this.startNewGame();
    }

    /**
     * Return to menu from game over screen
     */
    returnToMenu() {
        this.hideGameOverScreen();
        this.gameEngine.reset();
        this.updatePauseButton(false);
    }

    /**
     * Show game over screen
     */
    showGameOverScreen(finalScore) {
        const gameOverScreen = document.getElementById('game-over-screen');
        const finalScoreElement = document.getElementById('final-score-value');
        
        if (gameOverScreen) {
            gameOverScreen.classList.remove('hidden');
        }
        
        if (finalScoreElement) {
            finalScoreElement.textContent = finalScore.toLocaleString();
        }
    }

    /**
     * Hide game over screen
     */
    hideGameOverScreen() {
        const gameOverScreen = document.getElementById('game-over-screen');
        if (gameOverScreen) {
            gameOverScreen.classList.add('hidden');
        }
    }

    /**
     * Update pause button text
     */
    updatePauseButton(isPaused) {
        const pauseBtn = document.getElementById('pause-btn');
        if (pauseBtn) {
            pauseBtn.textContent = isPaused ? '再開' : '一時停止';
        }
    }

    /**
     * Show error message to user
     */
    showError(message) {
        // Create a simple error display
        const errorDiv = document.createElement('div');
        errorDiv.style.cssText = `
            position: fixed;
            top: 50%;
            left: 50%;
            transform: translate(-50%, -50%);
            background: #fed7d7;
            color: #c53030;
            padding: 20px;
            border-radius: 8px;
            border: 2px solid #feb2b2;
            z-index: 1000;
            text-align: center;
            font-weight: bold;
        `;
        errorDiv.textContent = message;
        document.body.appendChild(errorDiv);

        // Remove error after 5 seconds
        setTimeout(() => {
            if (errorDiv.parentNode) {
                errorDiv.parentNode.removeChild(errorDiv);
            }
        }, 5000);
    }

    /**
     * Handle game events from the game engine
     */
    handleGameEvent(event) {
        switch (event.type) {
            case 'gameOver':
                this.showGameOverScreen(event.finalScore);
                break;
            case 'scoreUpdate':
                this.updateScoreDisplay(event.score);
                break;
            case 'chainUpdate':
                this.updateChainDisplay(event.chainCount);
                break;
            default:
                console.log('Unhandled game event:', event);
        }
    }

    /**
     * Update score display
     */
    updateScoreDisplay(score) {
        const scoreElement = document.getElementById('score-display');
        if (scoreElement) {
            scoreElement.textContent = score.toLocaleString();
        }
    }

    /**
     * Update chain display
     */
    updateChainDisplay(chainCount) {
        const chainElement = document.getElementById('chain-display');
        if (chainElement) {
            chainElement.textContent = chainCount.toString();
        }
    }
}

// Initialize the game when the DOM is loaded
document.addEventListener('DOMContentLoaded', async () => {
    const game = new PuyoPuyoGame();
    
    // Make game instance globally available for debugging
    window.puyoPuyoGame = game;
    
    // Initialize the game
    await game.init();
    
    // Setup game event handling
    if (game.gameEngine) {
        game.gameEngine.onGameEvent = (event) => game.handleGameEvent(event);
    }
});

// Handle page visibility changes to pause/resume game
document.addEventListener('visibilitychange', () => {
    if (window.puyoPuyoGame && window.puyoPuyoGame.gameEngine) {
        if (document.hidden) {
            window.puyoPuyoGame.gameEngine.pause();
        }
        // Note: Don't auto-resume when page becomes visible
        // Let the user manually resume if they want to
    }
});

export { PuyoPuyoGame };