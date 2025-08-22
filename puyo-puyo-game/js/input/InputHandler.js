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
        
        // Touch-specific settings
        this.touchSettings = {
            minSwipeDistance: 30,
            maxTapTime: 200,
            maxTapDistance: 20,
            minDirectionalSwipe: 40,
            tapTolerance: 0.8,
            doubleTapDelay: 300
        };
        
        // Track last tap for double-tap detection
        this.lastTapTime = 0;
        this.lastTapPosition = { x: 0, y: 0 };
        
        // Bind event handlers
        this.bindKeyboardEvents();
        this.bindTouchEvents();
        
        // Optimize for mobile devices
        this.optimizeForMobile();
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
        
        // Determine if this was a tap or swipe
        const distance = Math.sqrt(deltaX * deltaX + deltaY * deltaY);
        
        if (distance < this.touchSettings.maxTapDistance && deltaTime < this.touchSettings.maxTapTime) {
            // This was a tap - check for double tap first
            const currentTime = performance.now();
            const timeSinceLastTap = currentTime - this.lastTapTime;
            const tapDistance = Math.sqrt(
                Math.pow(touch.clientX - this.lastTapPosition.x, 2) + 
                Math.pow(touch.clientY - this.lastTapPosition.y, 2)
            );
            
            if (timeSinceLastTap < this.touchSettings.doubleTapDelay && 
                tapDistance < this.touchSettings.maxTapDistance) {
                // Double tap detected - fast drop
                this.lastTapTime = 0; // Reset to prevent triple tap
                return { type: 'move', direction: 'down', fast: true };
            }
            
            // Update last tap info
            this.lastTapTime = currentTime;
            this.lastTapPosition = { x: touch.clientX, y: touch.clientY };
            
            // Single tap - determine tap zone
            return this.getTapAction(touch);
        } else if (distance >= this.touchSettings.minSwipeDistance) {
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
        
        // Convert screen coordinates to game coordinates
        const gameX = (x / canvasWidth) * 6; // 6 columns
        const gameY = (y / canvasHeight) * 12; // 12 rows
        
        // Check if tap is on current puyo pair
        if (this.gameEngine && this.gameEngine.puyoManager) {
            const currentPair = this.gameEngine.puyoManager.getCurrentPair();
            if (currentPair && this.isTapOnPuyoPair(gameX, gameY, currentPair)) {
                return { type: 'rotate' };
            }
        }
        
        // Divide canvas into touch zones for movement
        const leftZone = canvasWidth * 0.35;
        const rightZone = canvasWidth * 0.65;
        
        if (x < leftZone) {
            return { type: 'move', direction: 'left' };
        } else if (x > rightZone) {
            return { type: 'move', direction: 'right' };
        } else {
            // Center zone - rotate if not on puyo pair
            return { type: 'rotate' };
        }
    }

    /**
     * Check if tap coordinates are on the current puyo pair
     */
    isTapOnPuyoPair(gameX, gameY, puyoPair) {
        if (!puyoPair) return false;
        
        const tolerance = 0.8; // Allow some tolerance for easier tapping
        
        // Check primary puyo position
        const puyo1X = puyoPair.x;
        const puyo1Y = puyoPair.y;
        
        if (Math.abs(gameX - puyo1X) <= tolerance && Math.abs(gameY - puyo1Y) <= tolerance) {
            return true;
        }
        
        // Check secondary puyo position based on rotation
        let puyo2X = puyo1X;
        let puyo2Y = puyo1Y;
        
        switch (puyoPair.rotation) {
            case 0: // Vertical, puyo2 below puyo1
                puyo2Y = puyo1Y + 1;
                break;
            case 1: // Horizontal, puyo2 right of puyo1
                puyo2X = puyo1X + 1;
                break;
            case 2: // Vertical, puyo2 above puyo1
                puyo2Y = puyo1Y - 1;
                break;
            case 3: // Horizontal, puyo2 left of puyo1
                puyo2X = puyo1X - 1;
                break;
        }
        
        if (Math.abs(gameX - puyo2X) <= tolerance && Math.abs(gameY - puyo2Y) <= tolerance) {
            return true;
        }
        
        return false;
    }

    /**
     * Get action based on swipe direction
     */
    getSwipeAction(deltaX, deltaY) {
        const absX = Math.abs(deltaX);
        const absY = Math.abs(deltaY);
        
        // Minimum swipe distance for directional detection
        const minDirectionalSwipe = 40;
        
        if (absX > absY && absX > minDirectionalSwipe) {
            // Horizontal swipe - left or right movement
            return deltaX > 0 
                ? { type: 'move', direction: 'right' }
                : { type: 'move', direction: 'left' };
        } else if (absY > absX && absY > minDirectionalSwipe) {
            // Vertical swipe
            if (deltaY > 0) {
                // Swipe down - accelerate falling
                return { type: 'move', direction: 'down' };
            } else {
                // Swipe up - rotate (alternative to tap)
                return { type: 'rotate' };
            }
        }
        
        // If swipe is too small or diagonal, treat as tap
        return null;
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
        this.lastTapTime = 0;
        this.lastTapPosition = { x: 0, y: 0 };
    }

    /**
     * Check if device supports touch
     */
    isTouchDevice() {
        return 'ontouchstart' in window || navigator.maxTouchPoints > 0;
    }

    /**
     * Check if device is mobile
     */
    isMobileDevice() {
        return /Android|webOS|iPhone|iPad|iPod|BlackBerry|IEMobile|Opera Mini/i.test(navigator.userAgent);
    }

    /**
     * Optimize touch settings for mobile devices
     */
    optimizeForMobile() {
        if (this.isMobileDevice()) {
            // Adjust settings for mobile devices
            this.touchSettings.minSwipeDistance = 25;
            this.touchSettings.maxTapTime = 250;
            this.touchSettings.tapTolerance = 1.0;
            this.inputDelay = 80; // Faster input response on mobile
            
            // Add mobile-specific optimizations
            this.setupMobileOptimizations();
        }
    }

    /**
     * Setup mobile-specific optimizations
     */
    setupMobileOptimizations() {
        // Prevent default touch behaviors on game canvas
        const gameCanvas = document.getElementById('game-canvas');
        if (gameCanvas) {
            // Prevent scrolling and zooming
            gameCanvas.addEventListener('touchstart', (e) => e.preventDefault(), { passive: false });
            gameCanvas.addEventListener('touchmove', (e) => e.preventDefault(), { passive: false });
            gameCanvas.addEventListener('touchend', (e) => e.preventDefault(), { passive: false });
            
            // Prevent context menu on long press
            gameCanvas.addEventListener('contextmenu', (e) => e.preventDefault());
        }
        
        // Setup viewport meta tag for mobile
        this.setupMobileViewport();
        
        // Setup orientation change handling
        this.setupOrientationHandling();
        
        // Setup mobile-specific touch zones
        this.setupMobileTouchZones();
    }

    /**
     * Setup mobile viewport meta tag
     */
    setupMobileViewport() {
        if (typeof document === 'undefined') return;
        
        let viewport = document.querySelector('meta[name="viewport"]');
        if (!viewport) {
            viewport = document.createElement('meta');
            viewport.name = 'viewport';
            document.head.appendChild(viewport);
        }
        
        // Optimize viewport for mobile gaming
        viewport.content = 'width=device-width, initial-scale=1.0, user-scalable=no, maximum-scale=1.0, minimum-scale=1.0, viewport-fit=cover';
    }

    /**
     * Setup orientation change handling
     */
    setupOrientationHandling() {
        if (typeof window === 'undefined') return;
        
        window.addEventListener('orientationchange', () => {
            // Delay to allow orientation change to complete
            setTimeout(() => {
                this.handleOrientationChange();
            }, 100);
        });
    }

    /**
     * Handle orientation change
     */
    handleOrientationChange() {
        // Recalculate touch zones and canvas size
        if (this.gameEngine && this.gameEngine.renderer) {
            this.gameEngine.renderer.resize();
        }
        
        // Update mobile controls visibility
        this.updateMobileControlsVisibility();
        
        // Emit orientation change event
        if (this.gameEngine) {
            this.gameEngine.emitGameEvent({
                type: 'orientationChange',
                orientation: this.getOrientation()
            });
        }
    }

    /**
     * Get current device orientation
     */
    getOrientation() {
        if (typeof window === 'undefined') return 'portrait';
        
        if (window.orientation !== undefined) {
            return Math.abs(window.orientation) === 90 ? 'landscape' : 'portrait';
        }
        
        return window.innerWidth > window.innerHeight ? 'landscape' : 'portrait';
    }

    /**
     * Update mobile controls visibility based on device and orientation
     */
    updateMobileControlsVisibility() {
        const mobileControls = document.querySelector('.mobile-controls');
        if (!mobileControls) return;
        
        const shouldShow = this.isMobileDevice() || this.isTouchDevice();
        mobileControls.style.display = shouldShow ? 'block' : 'none';
        
        // Adjust position based on orientation
        const orientation = this.getOrientation();
        if (orientation === 'landscape') {
            mobileControls.style.bottom = '10px';
            mobileControls.style.transform = 'translateX(-50%) scale(0.8)';
        } else {
            mobileControls.style.bottom = '20px';
            mobileControls.style.transform = 'translateX(-50%) scale(1)';
        }
    }

    /**
     * Setup mobile-specific touch zones
     */
    setupMobileTouchZones() {
        const gameCanvas = document.getElementById('game-canvas');
        if (!gameCanvas) return;
        
        // Create invisible touch zones for better mobile interaction
        this.createTouchZones(gameCanvas);
    }

    /**
     * Create touch zones for mobile interaction
     */
    createTouchZones(canvas) {
        // Remove existing touch zones
        const existingZones = document.querySelectorAll('.touch-zone-overlay');
        existingZones.forEach(zone => zone.remove());
        
        // Create overlay container
        const overlay = document.createElement('div');
        overlay.className = 'touch-zone-overlay';
        overlay.style.cssText = `
            position: absolute;
            top: 0;
            left: 0;
            width: 100%;
            height: 100%;
            pointer-events: none;
            z-index: 10;
        `;
        
        // Position overlay relative to canvas
        const canvasContainer = canvas.parentElement;
        if (canvasContainer) {
            canvasContainer.style.position = 'relative';
            canvasContainer.appendChild(overlay);
        }
        
        // Create touch zones
        this.createLeftTouchZone(overlay);
        this.createRightTouchZone(overlay);
        this.createCenterTouchZone(overlay);
    }

    /**
     * Create left touch zone for left movement
     */
    createLeftTouchZone(overlay) {
        const leftZone = document.createElement('div');
        leftZone.className = 'touch-zone touch-zone-left';
        leftZone.style.cssText = `
            position: absolute;
            top: 0;
            left: 0;
            width: 30%;
            height: 100%;
            pointer-events: auto;
            background: rgba(255, 0, 0, 0.1);
            opacity: 0;
            transition: opacity 0.2s;
        `;
        
        // Add touch feedback
        leftZone.addEventListener('touchstart', (e) => {
            e.preventDefault();
            leftZone.style.opacity = '0.3';
            this.gameEngine?.handleInput({ type: 'move', direction: 'left' });
            this.triggerHapticFeedback('light');
        });
        
        leftZone.addEventListener('touchend', () => {
            leftZone.style.opacity = '0';
        });
        
        overlay.appendChild(leftZone);
    }

    /**
     * Create right touch zone for right movement
     */
    createRightTouchZone(overlay) {
        const rightZone = document.createElement('div');
        rightZone.className = 'touch-zone touch-zone-right';
        rightZone.style.cssText = `
            position: absolute;
            top: 0;
            right: 0;
            width: 30%;
            height: 100%;
            pointer-events: auto;
            background: rgba(0, 255, 0, 0.1);
            opacity: 0;
            transition: opacity 0.2s;
        `;
        
        // Add touch feedback
        rightZone.addEventListener('touchstart', (e) => {
            e.preventDefault();
            rightZone.style.opacity = '0.3';
            this.gameEngine?.handleInput({ type: 'move', direction: 'right' });
            this.triggerHapticFeedback('light');
        });
        
        rightZone.addEventListener('touchend', () => {
            rightZone.style.opacity = '0';
        });
        
        overlay.appendChild(rightZone);
    }

    /**
     * Create center touch zone for rotation
     */
    createCenterTouchZone(overlay) {
        const centerZone = document.createElement('div');
        centerZone.className = 'touch-zone touch-zone-center';
        centerZone.style.cssText = `
            position: absolute;
            top: 0;
            left: 30%;
            width: 40%;
            height: 100%;
            pointer-events: auto;
            background: rgba(0, 0, 255, 0.1);
            opacity: 0;
            transition: opacity 0.2s;
        `;
        
        // Add touch feedback
        centerZone.addEventListener('touchstart', (e) => {
            e.preventDefault();
            centerZone.style.opacity = '0.3';
            this.gameEngine?.handleInput({ type: 'rotate' });
            this.triggerHapticFeedback('medium');
        });
        
        centerZone.addEventListener('touchend', () => {
            centerZone.style.opacity = '0';
        });
        
        overlay.appendChild(centerZone);
    }

    /**
     * Enable haptic feedback if available
     */
    triggerHapticFeedback(type = 'light') {
        if (navigator.vibrate && this.isMobileDevice()) {
            switch (type) {
                case 'light':
                    navigator.vibrate(10);
                    break;
                case 'medium':
                    navigator.vibrate(20);
                    break;
                case 'heavy':
                    navigator.vibrate(50);
                    break;
            }
        }
    }

    /**
     * Get touch zone information for debugging
     */
    getTouchZoneInfo(touch) {
        const canvas = document.getElementById('game-canvas');
        if (!canvas) return null;
        
        const rect = canvas.getBoundingClientRect();
        const x = touch.clientX - rect.left;
        const y = touch.clientY - rect.top;
        
        const canvasWidth = rect.width;
        const canvasHeight = rect.height;
        
        const leftZone = canvasWidth * 0.35;
        const rightZone = canvasWidth * 0.65;
        
        let zone = 'center';
        if (x < leftZone) zone = 'left';
        else if (x > rightZone) zone = 'right';
        
        return {
            zone,
            x: x / canvasWidth,
            y: y / canvasHeight,
            gameX: (x / canvasWidth) * 6,
            gameY: (y / canvasHeight) * 12
        };
    }
}