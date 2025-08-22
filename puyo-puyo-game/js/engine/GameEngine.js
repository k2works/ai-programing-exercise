/**
 * GameEngine - Core game loop and state management
 * Coordinates all game systems and manages the main game loop
 */

import { GameState } from '../models/GameState.js';

export class GameEngine {
    constructor(renderer, audioManager) {
        this.renderer = renderer;
        this.audioManager = audioManager;
        
        // Game state
        this.isRunning = false;
        this.isPausedState = false;
        this.gameState = null;
        
        // Game loop timing
        this.lastFrameTime = 0;
        this.animationFrameId = null;
        this.targetFrameTime = 16.67; // 60fps = 16.67ms per frame
        this.frameTimeAccumulator = 0;
        
        // Performance monitoring
        this.frameCount = 0;
        this.lastFpsTime = 0;
        this.currentFps = 60;
        
        // Event callback
        this.onGameEvent = null;
        
        // Initialize placeholder for future components
        this.fieldManager = null;
        this.puyoManager = null;
        this.scoreManager = null;
        this.chainCalculator = null;
        
        // Bind methods to preserve context
        this.gameLoop = this.gameLoop.bind(this);
    }

    /**
     * Start the game
     * Requirements: 1.1, 1.2 - Initialize clean playfield and reset score
     */
    start() {
        if (this.isRunning) {
            console.warn('Game is already running');
            return;
        }
        
        this.isRunning = true;
        this.isPausedState = false;
        this.initializeGameState();
        this.startGameLoop();
        
        // Emit game started event
        this.emitGameEvent({
            type: 'gameStarted',
            gameState: this.gameState
        });
        
        console.log('Game started');
    }

    /**
     * Pause the game
     */
    pause() {
        if (!this.isRunning) {
            console.warn('Cannot pause: game is not running');
            return;
        }
        
        if (this.isPausedState) {
            console.warn('Game is already paused');
            return;
        }
        
        this.isPausedState = true;
        
        // Update game state
        if (this.gameState) {
            this.gameState.pause();
        }
        
        this.emitGameEvent({
            type: 'gamePaused'
        });
        
        console.log('Game paused');
    }

    /**
     * Resume the game
     */
    resume() {
        if (!this.isRunning) {
            console.warn('Cannot resume: game is not running');
            return;
        }
        
        if (!this.isPausedState) {
            console.warn('Game is not paused');
            return;
        }
        
        this.isPausedState = false;
        
        // Reset frame timing to prevent large delta time
        this.lastFrameTime = performance.now();
        this.frameTimeAccumulator = 0;
        
        // Update game state
        if (this.gameState) {
            this.gameState.resume();
        }
        
        this.emitGameEvent({
            type: 'gameResumed'
        });
        
        console.log('Game resumed');
    }

    /**
     * Stop the game
     */
    stop() {
        if (!this.isRunning) {
            return;
        }
        
        this.isRunning = false;
        this.isPausedState = false;
        
        // Cancel animation frame
        if (this.animationFrameId) {
            cancelAnimationFrame(this.animationFrameId);
            this.animationFrameId = null;
        }
        
        this.emitGameEvent({
            type: 'gameStopped'
        });
        
        console.log('Game stopped');
    }

    /**
     * Restart the game with a fresh state
     * Requirements: 1.1, 1.2 - Reset to clean state and start new game
     */
    restart() {
        console.log('Restarting game...');
        
        // Stop current game
        this.stop();
        
        // Clear any existing state
        this.gameState = null;
        this.frameTimeAccumulator = 0;
        this.frameCount = 0;
        this.lastFpsTime = 0;
        this.currentFps = 60;
        
        // Start fresh game
        this.start();
        
        this.emitGameEvent({
            type: 'gameRestarted'
        });
        
        console.log('Game restarted');
    }

    /**
     * Reset the game to initial state without starting
     */
    reset() {
        this.stop();
        this.gameState = null;
        this.frameTimeAccumulator = 0;
        this.frameCount = 0;
        this.lastFpsTime = 0;
        this.currentFps = 60;
        
        console.log('Game reset');
    }

    /**
     * Check if game is running
     */
    isGameRunning() {
        return this.isRunning;
    }

    /**
     * Check if game is paused
     */
    isPaused() {
        return this.isPausedState;
    }

    /**
     * Get current FPS
     */
    getCurrentFps() {
        return this.currentFps;
    }

    /**
     * Get game state
     */
    getGameState() {
        return this.gameState;
    }

    /**
     * Initialize game state
     * Requirements: 1.1, 1.2 - Clean 12x6 playfield and reset score to zero
     */
    initializeGameState() {
        // Create new game state instance
        this.gameState = new GameState();
        
        // Start new game (this initializes clean field and resets score)
        this.gameState.startNewGame();
        
        // Emit initial state events
        this.emitGameEvent({
            type: 'scoreUpdate',
            score: this.gameState.getScore()
        });
        
        this.emitGameEvent({
            type: 'chainUpdate',
            chainCount: this.gameState.getChainCount()
        });
        
        this.emitGameEvent({
            type: 'gameStateInitialized',
            gameState: this.gameState
        });
        
        console.log('Game state initialized - Clean 12x6 field, Score: 0');
    }

    /**
     * Start the main game loop using requestAnimationFrame
     */
    startGameLoop() {
        this.lastFrameTime = performance.now();
        this.lastFpsTime = this.lastFrameTime;
        this.frameCount = 0;
        this.frameTimeAccumulator = 0;
        
        // Start the game loop
        this.animationFrameId = requestAnimationFrame(this.gameLoop);
        
        console.log('Game loop started with requestAnimationFrame');
    }

    /**
     * Main game loop using requestAnimationFrame
     * Maintains 60fps target and handles update/render cycles
     */
    gameLoop(currentTime) {
        if (!this.isRunning) {
            return;
        }

        // Calculate delta time
        const deltaTime = currentTime - this.lastFrameTime;
        this.lastFrameTime = currentTime;
        
        // Update FPS counter
        this.updateFpsCounter(currentTime, deltaTime);
        
        // Accumulate frame time for fixed timestep updates
        this.frameTimeAccumulator += deltaTime;
        
        // Only update if not paused
        if (!this.isPausedState) {
            // Use fixed timestep for consistent game logic
            while (this.frameTimeAccumulator >= this.targetFrameTime) {
                this.update(this.targetFrameTime);
                this.frameTimeAccumulator -= this.targetFrameTime;
            }
        }
        
        // Always render (even when paused to show pause state)
        this.render();
        
        // Schedule next frame
        this.animationFrameId = requestAnimationFrame(this.gameLoop);
    }

    /**
     * Update FPS counter and performance monitoring
     */
    updateFpsCounter(currentTime, deltaTime) {
        this.frameCount++;
        
        // Update FPS every second
        if (currentTime - this.lastFpsTime >= 1000) {
            this.currentFps = Math.round((this.frameCount * 1000) / (currentTime - this.lastFpsTime));
            this.frameCount = 0;
            this.lastFpsTime = currentTime;
            
            // Emit performance event if FPS drops significantly
            if (this.currentFps < 45) {
                this.emitGameEvent({
                    type: 'performanceWarning',
                    fps: this.currentFps,
                    frameTime: deltaTime
                });
            }
        }
    }

    /**
     * Update game logic
     */
    update(deltaTime) {
        if (!this.gameState) {
            return;
        }
        
        // Don't update if game is over
        if (this.gameState.isGameOver()) {
            return;
        }

        // TODO: Update game components
        // - Update falling puyo
        // - Check for completed groups
        // - Handle chain reactions
        // - Update animations
        
        // Placeholder update logic
        this.updatePlaceholder(deltaTime);
        
        // Check for game over condition after updates
        if (this.isGameOver() && this.gameState.isPlaying()) {
            this.handleGameOver();
        }
    }

    /**
     * Placeholder update logic for initial implementation
     */
    updatePlaceholder(deltaTime) {
        // This is a placeholder that will be replaced by actual game logic
        // in subsequent tasks
    }

    /**
     * Render the game
     */
    render() {
        if (!this.renderer || !this.gameState) {
            return;
        }

        // Clear the canvas
        this.renderer.clear();
        
        // TODO: Render game components
        // - Render field
        // - Render puyo
        // - Render UI elements
        // - Render animations
        
        // Placeholder rendering
        this.renderPlaceholder();
    }

    /**
     * Placeholder rendering for initial implementation
     */
    renderPlaceholder() {
        // This is a placeholder that will be replaced by actual rendering logic
        // in subsequent tasks
        this.renderer.renderPlaceholder();
    }

    /**
     * Handle input from input handler
     * Requirements: 9.2 - Stop accepting input when game is over
     */
    handleInput(inputEvent) {
        // Don't process input if game is not running, paused, or game state is missing
        if (!this.isRunning || this.isPausedState || !this.gameState) {
            return;
        }
        
        // Don't process input if game is over (requirement 9.2)
        if (this.gameState.isGameOver()) {
            console.log('Input ignored: game is over');
            return;
        }

        // TODO: Process input events
        // - Move puyo pair
        // - Rotate puyo pair
        // - Drop puyo pair
        
        console.log('Input received:', inputEvent);
    }

    /**
     * Emit game event
     */
    emitGameEvent(event) {
        if (this.onGameEvent) {
            this.onGameEvent(event);
        }
    }

    /**
     * Check if game over condition is met
     * Requirements: 9.1 - Check if new puyo cannot be placed beyond top boundary
     * @returns {boolean} True if game over condition is met
     */
    isGameOver() {
        if (!this.gameState) {
            return false;
        }
        
        // Check if top row is occupied (standard game over condition)
        return this.gameState.isTopRowOccupied();
    }

    /**
     * Handle game over condition
     * Requirements: 9.1, 9.2, 9.3, 9.4 - Trigger game over, stop input, show screen with options
     */
    handleGameOver() {
        if (!this.gameState) {
            return;
        }
        
        console.log('Game over detected');
        
        // Set game state to game over
        this.gameState.gameOver();
        
        // Stop the game loop
        this.stop();
        
        // Get final score and statistics
        const finalScore = this.gameState.getScore();
        const statistics = this.gameState.getStatistics();
        
        // Emit game over event with final score and options
        this.emitGameEvent({
            type: 'gameOver',
            finalScore: finalScore,
            statistics: statistics,
            gameState: this.gameState
        });
        
        console.log(`Game over - Final Score: ${finalScore}`);
    }

    /**
     * Show game over screen with final score and options
     * Requirements: 9.3, 9.4 - Display final score with restart/menu options
     */
    showGameOverScreen() {
        if (!this.gameState) {
            return;
        }
        
        const finalScore = this.gameState.getScore();
        const statistics = this.gameState.getStatistics();
        
        this.emitGameEvent({
            type: 'showGameOverScreen',
            finalScore: finalScore,
            statistics: statistics,
            options: {
                restart: true,
                returnToMenu: true
            }
        });
    }

    /**
     * Handle restart from game over screen
     * Requirements: 9.4 - Provide restart option
     */
    handleRestartFromGameOver() {
        console.log('Restarting from game over...');
        this.restart();
    }

    /**
     * Handle return to menu from game over screen
     * Requirements: 9.4 - Provide menu return option
     */
    handleReturnToMenu() {
        console.log('Returning to menu...');
        this.reset();
        
        this.emitGameEvent({
            type: 'returnToMenu'
        });
    }
}