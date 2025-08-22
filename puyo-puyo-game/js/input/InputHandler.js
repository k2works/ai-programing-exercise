/**
 * InputHandler - Manages keyboard and touch input
 * Converts user input into game actions
 */

export class InputHandler {
    constructor(gameEngine) {
        this.gameEngine = gameEngine;
        this.keyStates = new Map();
        this.touchStates = new Map();
        
        // Input timing for smooth controls
        this.lastInputTime = 0;
        this.inputDelay = 100; // ms between repeated inputs
        
        // Bind event handlers
        this.bindKeyboardEvents();
        this.bindTouchEvents();
    }

    /**
     * Bind keyboard event listeners
     */
    bindKeyboardEvents() {
        document.addEventListener('keydown', (event) => this.handleKeyDown(event));
        document.addEventListener('keyup', (event) => this.handleKeyUp(event));
        
        // Prevent default behavior for game keys
        document.addEventListener('keydown', (event) => {
            if (this.isGameKey(event.code)) {
                event.preventDefault();
            }
        });
    }

    /**
     * Bind touch event listeners
     */
    bindTouchEvents() {
        const gameCanvas = document.getElementById('game-canvas');
        if (gameCanvas) {
            gameCanvas.addEventListener('touchstart', (event) => this.handleTouchStart(event));
            gameCanvas.addEventListener('touchmove', (event) => this.handleTouchMove(event));
            gameCanvas.addEventListener('touchend', (event) => this.handleTouchEnd(event));
            
            // Prevent default touch behaviors
            gameCanvas.addEventListener('touchstart', (event) => event.preventDefault());
            gameCanvas.addEventListener('touchmove', (event) => event.preventDefault());
        }
    }

    /**
     * Handle key down events
     */
    handleKeyDown(event) {
        const currentTime = performance.now();
        const key = event.code;
        
        // Check if this key is already being held down
        if (this.keyStates.get(key)) {
            // Handle key repeat with delay
            if (currentTime - this.lastInputTime < this.inputDelay) {
                return;
            }
        }
        
        this.keyStates.set(key, true);
        this.lastInputTime = currentTime;
        
        // Convert key to game action
        const action = this.keyToAction(key);
        if (action) {
            this.gameEngine.handleInput(action);
        }
    }

    /**
     * Handle key up events
     */
    handleKeyUp(event) {
        const key = event.code;
        this.keyStates.set(key, false);
    }

    /**
     * Handle touch start events
     */
    handleTouchStart(event) {
        const touches = event.changedTouches;
        
        for (let i = 0; i < touches.length; i++) {
            const touch = touches[i];
            const touchId = touch.identifier;
            
            this.touchStates.set(touchId, {
                startX: touch.clientX,
                startY: touch.clientY,
                currentX: touch.clientX,
                currentY: touch.clientY,
                startTime: performance.now()
            });
        }
    }

    /**
     * Handle touch move events
     */
    handleTouchMove(event) {
        const touches = event.changedTouches;
        
        for (let i = 0; i < touches.length; i++) {
            const touch = touches[i];
            const touchId = touch.identifier;
            const touchState = this.touchStates.get(touchId);
            
            if (touchState) {
                touchState.currentX = touch.clientX;
                touchState.currentY = touch.clientY;
            }
        }
    }

    /**
     * Handle touch end events
     */
    handleTouchEnd(event) {
        const touches = event.changedTouches;
        
        for (let i = 0; i < touches.length; i++) {
            const touch = touches[i];
            const touchId = touch.identifier;
            const touchState = this.touchStates.get(touchId);
            
            if (touchState) {
                const action = this.touchToAction(touchState, touch);
                if (action) {
                    this.gameEngine.handleInput(action);
                }
                
                this.touchStates.delete(touchId);
            }
        }
    }

    /**
     * Convert keyboard key to game action
     */
    keyToAction(key) {
        switch (key) {
            case 'ArrowLeft':
                return { type: 'move', direction: 'left' };
            case 'ArrowRight':
                return { type: 'move', direction: 'right' };
            case 'ArrowDown':
                return { type: 'move', direction: 'down' };
            case 'ArrowUp':
            case 'Space':
                return { type: 'rotate' };
            case 'KeyP':
                return { type: 'pause' };
            case 'KeyR':
                return { type: 'restart' };
            default:
                return null;
        }
    }

    /**
     * Convert touch gesture to game action
     */
    touchToAction(touchState, touch) {
        const deltaX = touchState.currentX - touchState.startX;
        const deltaY = touchState.currentY - touchState.startY;
        const deltaTime = performance.now() - touchState.startTime;
        
        const minSwipeDistance = 30;
        const maxTapTime = 200;
        
        // Determine if this was a tap or swipe
        const distance = Math.sqrt(deltaX * deltaX + deltaY * deltaY);
        
        if (distance < minSwipeDistance && deltaTime < maxTapTime) {
            // This was a tap - determine tap zone
            return this.getTapAction(touch);
        } else if (distance >= minSwipeDistance) {
            // This was a swipe - determine direction
            return this.getSwipeAction(deltaX, deltaY);
        }
        
        return null;
    }

    /**
     * Get action based on tap location
     */
    getTapAction(touch) {
        const canvas = document.getElementById('game-canvas');
        if (!canvas) return null;
        
        const rect = canvas.getBoundingClientRect();
        const x = touch.clientX - rect.left;
        const y = touch.clientY - rect.top;
        
        const canvasWidth = rect.width;
        const canvasHeight = rect.height;
        
        // Divide canvas into zones
        const leftZone = canvasWidth * 0.3;
        const rightZone = canvasWidth * 0.7;
        
        if (x < leftZone) {
            return { type: 'move', direction: 'left' };
        } else if (x > rightZone) {
            return { type: 'move', direction: 'right' };
        } else {
            // Center zone - rotate
            return { type: 'rotate' };
        }
    }

    /**
     * Get action based on swipe direction
     */
    getSwipeAction(deltaX, deltaY) {
        const absX = Math.abs(deltaX);
        const absY = Math.abs(deltaY);
        
        if (absX > absY) {
            // Horizontal swipe
            return deltaX > 0 
                ? { type: 'move', direction: 'right' }
                : { type: 'move', direction: 'left' };
        } else {
            // Vertical swipe
            return deltaY > 0 
                ? { type: 'move', direction: 'down' }
                : { type: 'rotate' };
        }
    }

    /**
     * Check if a key code is used for game controls
     */
    isGameKey(keyCode) {
        const gameKeys = [
            'ArrowLeft', 'ArrowRight', 'ArrowUp', 'ArrowDown',
            'Space', 'KeyP', 'KeyR'
        ];
        return gameKeys.includes(keyCode);
    }

    /**
     * Get current key state
     */
    isKeyPressed(keyCode) {
        return this.keyStates.get(keyCode) || false;
    }

    /**
     * Clear all input states
     */
    clearInputStates() {
        this.keyStates.clear();
        this.touchStates.clear();
    }
}