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
        
        // Initialize game components
        this.fieldManager = null;
        this.puyoManager = null;
        this.scoreManager = null;
        this.inputHandler = null;
        
        // Bind methods to preserve context
        this.gameLoop = this.gameLoop.bind(this);
    }

    /**
     * Start the game
     * Requirements: 1.1, 1.2 - Initialize clean playfield and reset score
     */
    async start() {
        if (this.isRunning) {
            console.warn('Game is already running');
            return;
        }
        
        this.isRunning = true;
        this.isPausedState = false;
        await this.initializeGameState();
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
    async restart() {
        console.log('Restarting game...');
        
        // Stop current game
        this.stop();
        
        // Clear any existing state
        this.gameState = null;
        this.fieldManager = null;
        this.puyoManager = null;
        this.frameTimeAccumulator = 0;
        this.frameCount = 0;
        this.lastFpsTime = 0;
        this.currentFps = 60;
        
        // Start fresh game
        await this.start();
        
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
     * Initialize game state and all game systems
     * Requirements: 1.1, 1.2 - Clean 12x6 playfield and reset score to zero
     */
    async initializeGameState() {
        // Import required modules
        const { FieldManager } = await import('../models/FieldManager.js');
        const { PuyoManager } = await import('../models/PuyoManager.js');
        
        // Create new game state instance
        this.gameState = new GameState();
        
        // Initialize field manager
        this.fieldManager = new FieldManager();
        
        // Initialize puyo manager with field manager
        this.puyoManager = new PuyoManager(this.fieldManager);
        
        // Start new game (this initializes clean field and resets score)
        this.gameState.startNewGame();
        
        // Set initial puyo pairs in game state
        this.gameState.setCurrentPair(this.puyoManager.getCurrentPair());
        this.gameState.setNextPair(this.puyoManager.getNextPair());
        
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
        
        console.log('Game systems initialized - Field, Puyo, Input, and Score managers connected');
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
        if (!this.gameState || !this.fieldManager || !this.puyoManager) {
            return;
        }
        
        // Don't update if game is over
        if (this.gameState.isGameOver()) {
            return;
        }

        // Update falling puyo
        const pairFixed = this.puyoManager.update(deltaTime);
        
        if (pairFixed) {
            // A puyo pair was just fixed to the field
            // Play landing sound
            if (this.audioManager) {
                this.audioManager.playSoftLandSound();
            }
            
            // Update game state with current field
            this.syncFieldToGameState();
            
            // Check for completed groups and handle chain reactions
            this.processChainReactions();
            
            // Update puyo pairs in game state
            this.gameState.setCurrentPair(this.puyoManager.getCurrentPair());
            this.gameState.setNextPair(this.puyoManager.getNextPair());
        }
        
        // Check for game over condition after updates
        if (this.isGameOver() && this.gameState.isPlaying()) {
            this.handleGameOver();
        }
    }

    /**
     * Sync field manager state to game state
     */
    syncFieldToGameState() {
        if (!this.fieldManager || !this.gameState) {
            return;
        }
        
        // Copy field from field manager to game state
        const field = this.fieldManager.getField();
        for (let y = 0; y < field.length; y++) {
            for (let x = 0; x < field[y].length; x++) {
                this.gameState.setFieldCell(x, y, field[y][x]);
            }
        }
    }

    /**
     * Process chain reactions after puyo are fixed
     */
    processChainReactions() {
        if (!this.fieldManager || !this.gameState) {
            return;
        }
        
        let chainActive = true;
        let chainProcessed = false;
        
        // Start chain if groups are found
        if (this.fieldManager.hasClearableGroups()) {
            this.gameState.startChain();
            chainActive = true;
        }
        
        // Process chain reactions
        while (chainActive) {
            // Find and clear connected groups
            const clearResult = this.fieldManager.findAndClearGroups();
            
            if (clearResult.clearedCount > 0) {
                chainProcessed = true;
                
                // Check for zenkeshi (all-clear)
                const isZenkeshi = this.gameState.detectZenkeshi();
                
                // Process scoring
                const scoringResult = this.gameState.processClearedGroups(
                    clearResult.groups, 
                    isZenkeshi
                );
                
                // Play audio feedback
                if (this.audioManager) {
                    if (isZenkeshi) {
                        this.audioManager.playAllClearSound();
                    } else if (scoringResult.chainLevel > 0) {
                        this.audioManager.playChainSound(scoringResult.chainLevel);
                    } else {
                        this.audioManager.playClearSound();
                    }
                    
                    // Play combo sound for multiple groups
                    if (clearResult.groupCount > 1) {
                        setTimeout(() => {
                            this.audioManager.playComboSound(clearResult.groupCount);
                        }, 100);
                    }
                }
                
                // Emit scoring events
                this.emitGameEvent({
                    type: 'scoreUpdate',
                    score: this.gameState.getScore(),
                    lastChainScore: scoringResult.score,
                    chainLevel: scoringResult.chainLevel
                });
                
                this.emitGameEvent({
                    type: 'chainUpdate',
                    chainCount: this.gameState.getChainCount()
                });
                
                // Emit puyo cleared event for animations
                this.emitGameEvent({
                    type: 'puyoCleared',
                    clearedPositions: clearResult.clearedPositions,
                    chainLevel: scoringResult.chainLevel,
                    isZenkeshi: isZenkeshi
                });
                
                // Apply gravity and check for more groups
                this.fieldManager.applyCascadingGravity();
                this.syncFieldToGameState();
                
                // Check if more groups can be cleared (continue chain)
                chainActive = this.fieldManager.hasClearableGroups();
            } else {
                // No more groups to clear, end chain
                chainActive = false;
            }
        }
        
        // End chain if any processing occurred
        if (chainProcessed) {
            const chainSummary = this.gameState.endChain();
            
            this.emitGameEvent({
                type: 'chainComplete',
                chainSummary: chainSummary
            });
        }
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
        
        // Render field with placed puyo
        if (this.fieldManager) {
            this.renderer.renderField(this.fieldManager.getField());
        }
        
        // Render current falling puyo pair
        if (this.puyoManager && this.puyoManager.getCurrentPair()) {
            const currentPair = this.puyoManager.getCurrentPair();
            const positions = currentPair.getPuyoPositions();
            
            // Render both puyo in the pair
            this.renderer.renderPuyo(currentPair.puyo1, positions.puyo1.x, positions.puyo1.y);
            this.renderer.renderPuyo(currentPair.puyo2, positions.puyo2.x, positions.puyo2.y);
        }
        
        // Render UI elements
        this.renderer.renderUI(
            this.gameState.getScore(),
            this.gameState.getChainCount(),
            this.gameState.getNextPair()
        );
        
        // Render animations
        this.renderer.updateAnimations(performance.now());
        this.renderer.renderAnimations();
        
        // Render game over screen if needed
        if (this.gameState.isGameOver()) {
            this.renderer.renderGameOver();
        }
    }



    /**
     * Handle input from input handler
     * Requirements: 9.2 - Stop accepting input when game is over
     */
    handleInput(inputEvent) {
        // Validate input event
        if (!inputEvent || typeof inputEvent !== 'object' || !inputEvent.type) {
            return;
        }
        
        // Don't process input if game is not running, paused, or game state is missing
        if (!this.isRunning || this.isPausedState || !this.gameState || !this.puyoManager) {
            return;
        }
        
        // Don't process input if game is over (requirement 9.2)
        if (this.gameState.isGameOver()) {
            console.log('Input ignored: game is over');
            return;
        }

        // Process input events
        switch (inputEvent.type) {
            case 'move':
                this.handleMoveInput(inputEvent.direction, inputEvent.fast);
                break;
            case 'rotate':
                this.handleRotateInput();
                break;
            case 'pause':
                this.handlePauseInput();
                break;
            case 'restart':
                this.handleRestartInput();
                break;
            default:
                console.log('Unknown input type:', inputEvent.type);
        }
    }

    /**
     * Handle movement input
     */
    handleMoveInput(direction, fast = false) {
        if (!this.puyoManager) {
            return;
        }
        
        let moved = false;
        
        switch (direction) {
            case 'left':
                moved = this.puyoManager.movePair('left');
                if (moved && this.audioManager) {
                    this.audioManager.playMoveSound();
                }
                break;
            case 'right':
                moved = this.puyoManager.movePair('right');
                if (moved && this.audioManager) {
                    this.audioManager.playMoveSound();
                }
                break;
            case 'down':
                if (fast) {
                    // Fast drop - move down as much as possible
                    let dropCount = 0;
                    while (this.puyoManager.canPairFall()) {
                        this.puyoManager.fallPair();
                        dropCount++;
                    }
                    if (dropCount > 0 && this.audioManager) {
                        this.audioManager.playHardLandSound();
                    }
                } else {
                    // Enable fast drop mode
                    this.puyoManager.dropPair();
                    if (this.audioManager) {
                        this.audioManager.playDropSound();
                    }
                }
                break;
        }
    }

    /**
     * Handle rotation input
     */
    handleRotateInput() {
        if (!this.puyoManager) {
            return;
        }
        
        const rotated = this.puyoManager.rotatePair();
        if (rotated && this.audioManager) {
            this.audioManager.playRotateSound();
        }
    }

    /**
     * Handle pause input
     */
    handlePauseInput() {
        if (this.isPausedState) {
            this.resume();
        } else {
            this.pause();
        }
    }

    /**
     * Handle restart input
     */
    async handleRestartInput() {
        await this.restart();
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
        if (!this.gameState || !this.fieldManager || !this.puyoManager) {
            return false;
        }
        
        // Check if puyo manager detects game over (cannot spawn new pair)
        return this.puyoManager.isGameOver() || this.fieldManager.isGameOver();
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
        
        // Play game over sound
        if (this.audioManager) {
            this.audioManager.playGameOverSound();
        }
        
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