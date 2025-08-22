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
            
            // Setup enhanced UI feedback
            this.setupEnhancedUIFeedback();
            
            this.isInitialized = true;
            console.log('Puyo Puyo game initialized successfully');
            
            // Show welcome notification
            this.showNotification('ぷよぷよゲームへようこそ！', 'success');
            
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
            newGameBtn.addEventListener('click', async () => await this.startNewGame());
        }

        // Pause button
        const pauseBtn = document.getElementById('pause-btn');
        if (pauseBtn) {
            pauseBtn.addEventListener('click', () => this.togglePause());
        }

        // Restart button (game over screen)
        const restartBtn = document.getElementById('restart-btn');
        if (restartBtn) {
            restartBtn.addEventListener('click', async () => await this.restartGame());
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
    async startNewGame() {
        if (!this.isInitialized) {
            console.warn('Game not initialized yet');
            return;
        }

        this.hideGameOverScreen();
        await this.gameEngine.start();
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
    async restartGame() {
        await this.startNewGame();
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
     * Show game over screen with smooth transition
     */
    showGameOverScreen(finalScore) {
        const gameOverScreen = document.getElementById('game-over-screen');
        const finalScoreElement = document.getElementById('final-score-value');
        
        if (gameOverScreen) {
            // Add fade-in animation
            gameOverScreen.style.opacity = '0';
            gameOverScreen.classList.remove('hidden');
            
            // Animate in
            setTimeout(() => {
                gameOverScreen.style.transition = 'opacity 0.5s ease-in-out';
                gameOverScreen.style.opacity = '1';
            }, 100);
        }
        
        if (finalScoreElement) {
            // Animate score counting up
            this.animateScoreCount(finalScoreElement, 0, finalScore, 1000);
        }
    }

    /**
     * Hide game over screen with smooth transition
     */
    hideGameOverScreen() {
        const gameOverScreen = document.getElementById('game-over-screen');
        if (gameOverScreen) {
            // Add fade-out animation
            gameOverScreen.style.transition = 'opacity 0.3s ease-in-out';
            gameOverScreen.style.opacity = '0';
            
            setTimeout(() => {
                gameOverScreen.classList.add('hidden');
                gameOverScreen.style.opacity = '';
                gameOverScreen.style.transition = '';
            }, 300);
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
     * Update chain display with enhanced visual feedback
     */
    updateChainDisplay(chainCount) {
        const chainElement = document.getElementById('chain-display');
        if (chainElement) {
            const previousCount = parseInt(chainElement.textContent) || 0;
            chainElement.textContent = chainCount.toString();
            
            // Add visual feedback for chain increases
            if (chainCount > previousCount && chainCount > 0) {
                this.addChainVisualFeedback(chainElement, chainCount);
            }
        }
    }

    /**
     * Add visual feedback for chain reactions
     */
    addChainVisualFeedback(element, chainCount) {
        // Remove existing animation classes
        element.classList.remove('chain-active', 'chain-mega');
        
        // Add appropriate animation based on chain level
        if (chainCount >= 5) {
            element.classList.add('chain-mega');
        } else {
            element.classList.add('chain-active');
        }
        
        // Create floating text effect for high chains
        if (chainCount >= 3) {
            this.createFloatingText(`${chainCount} Chain!`, element);
        }
        
        // Remove animation class after animation completes
        setTimeout(() => {
            element.classList.remove('chain-active', 'chain-mega');
        }, 1000);
    }

    /**
     * Create floating text effect
     */
    createFloatingText(text, nearElement) {
        const floatingText = document.createElement('div');
        floatingText.textContent = text;
        floatingText.style.cssText = `
            position: absolute;
            color: #ff6b6b;
            font-weight: bold;
            font-size: 1.2rem;
            pointer-events: none;
            z-index: 1000;
            animation: floatUp 2s ease-out forwards;
            text-shadow: 2px 2px 4px rgba(0,0,0,0.5);
        `;
        
        // Position near the element
        const rect = nearElement.getBoundingClientRect();
        floatingText.style.left = (rect.left + rect.width / 2) + 'px';
        floatingText.style.top = (rect.top - 20) + 'px';
        floatingText.style.transform = 'translateX(-50%)';
        
        document.body.appendChild(floatingText);
        
        // Remove after animation
        setTimeout(() => {
            if (floatingText.parentNode) {
                floatingText.parentNode.removeChild(floatingText);
            }
        }, 2000);
    }

    /**
     * Animate score counting up
     */
    animateScoreCount(element, startValue, endValue, duration) {
        const startTime = performance.now();
        const difference = endValue - startValue;
        
        const animate = (currentTime) => {
            const elapsed = currentTime - startTime;
            const progress = Math.min(elapsed / duration, 1);
            
            // Use easing function for smooth animation
            const easeOutQuart = 1 - Math.pow(1 - progress, 4);
            const currentValue = Math.floor(startValue + (difference * easeOutQuart));
            
            element.textContent = currentValue.toLocaleString();
            
            if (progress < 1) {
                requestAnimationFrame(animate);
            }
        };
        
        requestAnimationFrame(animate);
    }

    /**
     * Add button click feedback
     */
    addButtonFeedback(button) {
        if (!button) return;
        
        button.addEventListener('click', () => {
            // Play button sound if audio manager is available
            if (this.gameEngine?.audioManager) {
                this.gameEngine.audioManager.playButtonSound();
            }
            
            // Add visual feedback
            button.style.transform = 'scale(0.95)';
            setTimeout(() => {
                button.style.transform = '';
            }, 150);
        });
    }

    /**
     * Setup enhanced UI feedback
     */
    setupEnhancedUIFeedback() {
        // Add feedback to all buttons
        const buttons = document.querySelectorAll('.btn, .control-btn');
        buttons.forEach(button => this.addButtonFeedback(button));
        
        // Add hover effects for non-touch devices
        if (!this.isTouchDevice()) {
            buttons.forEach(button => {
                button.addEventListener('mouseenter', () => {
                    button.style.transform = 'translateY(-2px)';
                });
                
                button.addEventListener('mouseleave', () => {
                    button.style.transform = '';
                });
            });
        }
    }

    /**
     * Check if device supports touch
     */
    isTouchDevice() {
        return 'ontouchstart' in window || navigator.maxTouchPoints > 0;
    }

    /**
     * Add loading state to buttons
     */
    setButtonLoading(buttonId, isLoading) {
        const button = document.getElementById(buttonId);
        if (!button) return;
        
        if (isLoading) {
            button.disabled = true;
            button.dataset.originalText = button.textContent;
            button.textContent = '読み込み中...';
            button.classList.add('loading');
        } else {
            button.disabled = false;
            button.textContent = button.dataset.originalText || button.textContent;
            button.classList.remove('loading');
        }
    }

    /**
     * Show notification message
     */
    showNotification(message, type = 'info', duration = 3000) {
        const notification = document.createElement('div');
        notification.className = `notification notification-${type}`;
        notification.textContent = message;
        notification.style.cssText = `
            position: fixed;
            top: 20px;
            right: 20px;
            padding: 15px 20px;
            border-radius: 8px;
            color: white;
            font-weight: bold;
            z-index: 2000;
            animation: slideInRight 0.3s ease-out;
            max-width: 300px;
            word-wrap: break-word;
        `;
        
        // Set background color based on type
        const colors = {
            info: '#3498db',
            success: '#2ecc71',
            warning: '#f39c12',
            error: '#e74c3c'
        };
        notification.style.backgroundColor = colors[type] || colors.info;
        
        document.body.appendChild(notification);
        
        // Auto-remove after duration
        setTimeout(() => {
            notification.style.animation = 'slideOutRight 0.3s ease-in';
            setTimeout(() => {
                if (notification.parentNode) {
                    notification.parentNode.removeChild(notification);
                }
            }, 300);
        }, duration);
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