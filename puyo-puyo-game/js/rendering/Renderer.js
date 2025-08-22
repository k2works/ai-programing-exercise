/**
 * Renderer - Handles all visual output using HTML5 Canvas
 * Manages rendering of game field, puyo, UI elements, and animations
 */

import { AnimationManager } from './Animation.js';

export class Renderer {
    constructor(gameCanvas, nextPuyoCanvas) {
        this.gameCanvas = gameCanvas;
        this.nextPuyoCanvas = nextPuyoCanvas;
        
        this.gameContext = gameCanvas.getContext('2d');
        this.nextPuyoContext = nextPuyoCanvas.getContext('2d');
        
        // Game field dimensions
        this.fieldWidth = 6;
        this.fieldHeight = 12;
        this.cellSize = 30;
        
        // Colors for different puyo types
        this.puyoColors = {
            red: '#FF4444',
            blue: '#4444FF',
            green: '#44FF44',
            yellow: '#FFFF44',
            purple: '#FF44FF'
        };
        
        // Animation manager
        this.animationManager = new AnimationManager();
        
        // Screen shake offset
        this.screenShakeOffset = { x: 0, y: 0 };
        
        // Initialize canvas settings
        this.initializeCanvas();
    }

    /**
     * Initialize canvas settings
     */
    initializeCanvas() {
        // Set up game canvas
        this.gameContext.imageSmoothingEnabled = true;
        this.gameContext.textAlign = 'center';
        this.gameContext.textBaseline = 'middle';
        
        // Set up next puyo canvas
        this.nextPuyoContext.imageSmoothingEnabled = true;
        this.nextPuyoContext.textAlign = 'center';
        this.nextPuyoContext.textBaseline = 'middle';
        
        // Calculate actual cell size based on canvas dimensions
        this.cellSize = Math.min(
            this.gameCanvas.width / this.fieldWidth,
            this.gameCanvas.height / this.fieldHeight
        );
    }

    /**
     * Clear all canvases
     */
    clear() {
        // Clear game canvas
        this.gameContext.clearRect(0, 0, this.gameCanvas.width, this.gameCanvas.height);
        
        // Clear next puyo canvas
        this.nextPuyoContext.clearRect(0, 0, this.nextPuyoCanvas.width, this.nextPuyoCanvas.height);
    }

    /**
     * Render the game field
     */
    renderField(field) {
        if (!field) return;
        
        const ctx = this.gameContext;
        
        // Draw field background
        ctx.fillStyle = '#2D3748';
        ctx.fillRect(0, 0, this.gameCanvas.width, this.gameCanvas.height);
        
        // Draw grid lines
        this.drawGrid(ctx);
        
        // Draw placed puyo
        for (let y = 0; y < this.fieldHeight; y++) {
            for (let x = 0; x < this.fieldWidth; x++) {
                const puyo = field[y] && field[y][x];
                if (puyo) {
                    this.renderPuyo(puyo, x, y, ctx);
                }
            }
        }
    }

    /**
     * Draw grid lines on the field
     */
    drawGrid(ctx) {
        ctx.strokeStyle = '#4A5568';
        ctx.lineWidth = 1;
        
        // Vertical lines
        for (let x = 0; x <= this.fieldWidth; x++) {
            const xPos = x * this.cellSize;
            ctx.beginPath();
            ctx.moveTo(xPos, 0);
            ctx.lineTo(xPos, this.fieldHeight * this.cellSize);
            ctx.stroke();
        }
        
        // Horizontal lines
        for (let y = 0; y <= this.fieldHeight; y++) {
            const yPos = y * this.cellSize;
            ctx.beginPath();
            ctx.moveTo(0, yPos);
            ctx.lineTo(this.fieldWidth * this.cellSize, yPos);
            ctx.stroke();
        }
    }

    /**
     * Render a single puyo
     */
    renderPuyo(puyo, x, y, ctx = this.gameContext, animatedPosition = null) {
        if (!puyo || !puyo.color) return;
        
        // Use animated position if provided
        const renderX = animatedPosition ? animatedPosition.x : x;
        const renderY = animatedPosition ? animatedPosition.y : y;
        
        const centerX = (renderX + 0.5) * this.cellSize;
        const centerY = (renderY + 0.5) * this.cellSize;
        const radius = this.cellSize * 0.4;
        
        // Get color
        const color = this.puyoColors[puyo.color] || '#CCCCCC';
        
        // Draw puyo body
        ctx.fillStyle = color;
        ctx.beginPath();
        ctx.arc(centerX, centerY, radius, 0, Math.PI * 2);
        ctx.fill();
        
        // Draw puyo outline
        ctx.strokeStyle = '#FFFFFF';
        ctx.lineWidth = 2;
        ctx.stroke();
        
        // Draw puyo highlight
        ctx.fillStyle = 'rgba(255, 255, 255, 0.3)';
        ctx.beginPath();
        ctx.arc(centerX - radius * 0.3, centerY - radius * 0.3, radius * 0.3, 0, Math.PI * 2);
        ctx.fill();
        
        // Draw state-specific effects
        if (puyo.state === 'clearing') {
            this.renderClearingEffect(centerX, centerY, radius, ctx);
        } else if (puyo.state === 'falling') {
            this.renderFallingEffect(centerX, centerY, radius, ctx);
        } else if (puyo.state === 'fixed') {
            this.renderFixedEffect(centerX, centerY, radius, ctx);
        }
    }

    /**
     * Render clearing effect for puyo
     */
    renderClearingEffect(x, y, radius, ctx) {
        ctx.fillStyle = 'rgba(255, 255, 255, 0.7)';
        ctx.beginPath();
        ctx.arc(x, y, radius * 1.2, 0, Math.PI * 2);
        ctx.fill();
    }

    /**
     * Render falling effect for puyo
     */
    renderFallingEffect(x, y, radius, ctx) {
        // Add a subtle shadow effect for falling puyo
        ctx.fillStyle = 'rgba(0, 0, 0, 0.2)';
        ctx.beginPath();
        ctx.arc(x + 2, y + 2, radius, 0, Math.PI * 2);
        ctx.fill();
        
        // Add motion blur effect
        ctx.fillStyle = 'rgba(255, 255, 255, 0.1)';
        ctx.beginPath();
        ctx.arc(x, y - 3, radius * 0.8, 0, Math.PI * 2);
        ctx.fill();
    }

    /**
     * Render fixed effect for puyo
     */
    renderFixedEffect(x, y, radius, ctx) {
        // Add a subtle border to indicate fixed state
        ctx.strokeStyle = 'rgba(255, 255, 255, 0.8)';
        ctx.lineWidth = 1;
        ctx.beginPath();
        ctx.arc(x, y, radius + 1, 0, Math.PI * 2);
        ctx.stroke();
        
        // Add inner shadow for depth
        ctx.fillStyle = 'rgba(0, 0, 0, 0.1)';
        ctx.beginPath();
        ctx.arc(x + 1, y + 1, radius * 0.2, 0, Math.PI * 2);
        ctx.fill();
    }

    /**
     * Render UI elements (score, chains, etc.)
     */
    renderUI(score, chains, nextPuyo) {
        // Update HTML UI elements
        this.updateScoreDisplay(score);
        this.updateChainDisplay(chains);
        
        // Render next puyo in canvas
        if (nextPuyo) {
            this.renderNextPuyo(nextPuyo);
        }
    }

    /**
     * Update score display in HTML UI
     */
    updateScoreDisplay(score) {
        const scoreElement = document.getElementById('score-display');
        if (scoreElement) {
            scoreElement.textContent = score.toLocaleString();
        }
    }

    /**
     * Update chain display in HTML UI
     */
    updateChainDisplay(chains) {
        const chainElement = document.getElementById('chain-display');
        if (chainElement) {
            chainElement.textContent = chains.toString();
            
            // Add visual feedback for high chains
            if (chains > 0) {
                chainElement.classList.add('chain-active');
                setTimeout(() => {
                    chainElement.classList.remove('chain-active');
                }, 1000);
            }
        }
    }

    /**
     * Render next puyo preview
     */
    renderNextPuyo(puyoPair) {
        if (!puyoPair) {
            this.renderEmptyNextPuyo();
            return;
        }
        
        const ctx = this.nextPuyoContext;
        const canvasWidth = this.nextPuyoCanvas.width;
        const canvasHeight = this.nextPuyoCanvas.height;
        
        // Clear canvas
        ctx.clearRect(0, 0, canvasWidth, canvasHeight);
        
        // Draw background with border
        this.renderNextPuyoBackground(ctx, canvasWidth, canvasHeight);
        
        // Calculate puyo size for preview
        const previewCellSize = Math.min(canvasWidth, canvasHeight) / 4;
        
        // Draw puyo pair in vertical arrangement
        const centerX = canvasWidth / 2;
        const puyo1Y = canvasHeight / 2 - previewCellSize / 2;
        const puyo2Y = canvasHeight / 2 + previewCellSize / 2;
        
        // Render first puyo
        if (puyoPair.puyo1) {
            this.renderPuyoPreview(puyoPair.puyo1, centerX, puyo1Y, previewCellSize * 0.4, ctx);
        }
        
        // Render second puyo
        if (puyoPair.puyo2) {
            this.renderPuyoPreview(puyoPair.puyo2, centerX, puyo2Y, previewCellSize * 0.4, ctx);
        }
    }

    /**
     * Render background for next puyo preview area
     */
    renderNextPuyoBackground(ctx, width, height) {
        // Draw background
        ctx.fillStyle = '#F7FAFC';
        ctx.fillRect(0, 0, width, height);
        
        // Draw border
        ctx.strokeStyle = '#E2E8F0';
        ctx.lineWidth = 2;
        ctx.strokeRect(1, 1, width - 2, height - 2);
        
        // Draw inner shadow
        ctx.fillStyle = 'rgba(0, 0, 0, 0.05)';
        ctx.fillRect(2, 2, width - 4, 4);
    }

    /**
     * Render empty next puyo preview
     */
    renderEmptyNextPuyo() {
        const ctx = this.nextPuyoContext;
        const canvasWidth = this.nextPuyoCanvas.width;
        const canvasHeight = this.nextPuyoCanvas.height;
        
        // Clear canvas
        ctx.clearRect(0, 0, canvasWidth, canvasHeight);
        
        // Draw background
        this.renderNextPuyoBackground(ctx, canvasWidth, canvasHeight);
        
        // Draw placeholder text
        ctx.fillStyle = '#A0AEC0';
        ctx.font = '12px Arial';
        ctx.textAlign = 'center';
        ctx.textBaseline = 'middle';
        ctx.fillText('次のぷよ', canvasWidth / 2, canvasHeight / 2);
    }

    /**
     * Render puyo in preview canvas
     */
    renderPuyoPreview(puyo, x, y, radius, ctx) {
        if (!puyo || !puyo.color) return;
        
        const color = this.puyoColors[puyo.color] || '#CCCCCC';
        
        // Draw puyo body
        ctx.fillStyle = color;
        ctx.beginPath();
        ctx.arc(x, y, radius, 0, Math.PI * 2);
        ctx.fill();
        
        // Draw puyo outline
        ctx.strokeStyle = '#FFFFFF';
        ctx.lineWidth = 1;
        ctx.stroke();
        
        // Draw puyo highlight
        ctx.fillStyle = 'rgba(255, 255, 255, 0.3)';
        ctx.beginPath();
        ctx.arc(x - radius * 0.3, y - radius * 0.3, radius * 0.3, 0, Math.PI * 2);
        ctx.fill();
    }

    /**
     * Render game over screen
     */
    renderGameOver() {
        const ctx = this.gameContext;
        
        // Draw semi-transparent overlay
        ctx.fillStyle = 'rgba(0, 0, 0, 0.7)';
        ctx.fillRect(0, 0, this.gameCanvas.width, this.gameCanvas.height);
        
        // Draw "Game Over" text
        ctx.fillStyle = '#FFFFFF';
        ctx.font = 'bold 32px Arial';
        ctx.fillText(
            'ゲームオーバー',
            this.gameCanvas.width / 2,
            this.gameCanvas.height / 2
        );
    }

    /**
     * Update and render all animations
     */
    updateAnimations(currentTime) {
        // Update all animations
        const completedAnimations = this.animationManager.update(currentTime);
        
        // Update screen shake offset
        const shakeAnimations = this.animationManager.getAnimationsByType('screenShake');
        if (shakeAnimations.length > 0) {
            this.screenShakeOffset = this.animationManager.getScreenShakeOffset(shakeAnimations[0]);
        } else {
            this.screenShakeOffset = { x: 0, y: 0 };
        }
        
        return completedAnimations;
    }

    /**
     * Render all active animations
     */
    renderAnimations() {
        const ctx = this.gameContext;
        
        // Save current context state
        ctx.save();
        
        // Apply screen shake
        ctx.translate(this.screenShakeOffset.x, this.screenShakeOffset.y);
        
        // Render chain highlights
        this.renderChainHighlights(ctx);
        
        // Render clearing animations
        this.renderClearingAnimations(ctx);
        
        // Restore context state
        ctx.restore();
    }

    /**
     * Render chain highlight animations
     */
    renderChainHighlights(ctx) {
        const chainAnimations = this.animationManager.getAnimationsByType('chainHighlight');
        
        chainAnimations.forEach(animation => {
            const values = this.animationManager.getChainHighlightValues(animation);
            const { x, y } = animation.properties;
            
            if (values) {
                ctx.save();
                ctx.globalAlpha = values.alpha;
                
                // Create radial gradient
                const gradient = ctx.createRadialGradient(
                    (x + 0.5) * this.cellSize, (y + 0.5) * this.cellSize, 0,
                    (x + 0.5) * this.cellSize, (y + 0.5) * this.cellSize, values.radius
                );
                gradient.addColorStop(0, values.color);
                gradient.addColorStop(1, 'transparent');
                
                ctx.fillStyle = gradient;
                ctx.beginPath();
                ctx.arc(
                    (x + 0.5) * this.cellSize,
                    (y + 0.5) * this.cellSize,
                    values.radius,
                    0,
                    Math.PI * 2
                );
                ctx.fill();
                
                ctx.restore();
            }
        });
    }

    /**
     * Render clearing animations
     */
    renderClearingAnimations(ctx) {
        const clearAnimations = this.animationManager.getAnimationsByType('clear');
        
        clearAnimations.forEach(animation => {
            const values = this.animationManager.getClearValues(animation);
            const { x, y } = animation.properties;
            
            if (values) {
                ctx.save();
                ctx.globalAlpha = values.alpha;
                
                const centerX = (x + 0.5) * this.cellSize;
                const centerY = (y + 0.5) * this.cellSize;
                const radius = this.cellSize * 0.4 * values.scale;
                
                // Draw expanding white circle
                ctx.fillStyle = 'white';
                ctx.beginPath();
                ctx.arc(centerX, centerY, radius, 0, Math.PI * 2);
                ctx.fill();
                
                // Draw sparkle effect
                this.renderSparkles(ctx, centerX, centerY, radius, values.alpha);
                
                ctx.restore();
            }
        });
    }

    /**
     * Render sparkle effects for clearing animation
     */
    renderSparkles(ctx, centerX, centerY, radius, alpha) {
        const sparkleCount = 8;
        const sparkleRadius = 2;
        
        for (let i = 0; i < sparkleCount; i++) {
            const angle = (i / sparkleCount) * Math.PI * 2;
            const distance = radius * 1.2;
            const sparkleX = centerX + Math.cos(angle) * distance;
            const sparkleY = centerY + Math.sin(angle) * distance;
            
            ctx.save();
            ctx.globalAlpha = alpha * 0.8;
            ctx.fillStyle = '#FFD700'; // Gold sparkles
            ctx.beginPath();
            ctx.arc(sparkleX, sparkleY, sparkleRadius, 0, Math.PI * 2);
            ctx.fill();
            ctx.restore();
        }
    }

    /**
     * Create animation for puyo clearing
     */
    startClearAnimation(x, y) {
        return this.animationManager.createClearAnimation(x, y);
    }

    /**
     * Create animation for chain highlight
     */
    startChainHighlight(x, y, chainLevel) {
        return this.animationManager.createChainHighlight(x, y, chainLevel);
    }

    /**
     * Create animation for screen shake
     */
    startScreenShake(intensity = 5) {
        return this.animationManager.createScreenShake(intensity);
    }

    /**
     * Create animation for puyo movement
     */
    startMoveAnimation(fromX, fromY, toX, toY) {
        return this.animationManager.createMoveAnimation(fromX, fromY, toX, toY);
    }

    /**
     * Create animation for puyo drop
     */
    startDropAnimation(x, fromY, toY) {
        return this.animationManager.createDropAnimation(x, fromY, toY);
    }

    /**
     * Get animated position for a puyo
     */
    getAnimatedPosition(puyoId, defaultX, defaultY) {
        // This would be used with a puyo ID system to track individual puyo animations
        // For now, return default position
        return { x: defaultX, y: defaultY };
    }

    /**
     * Check if any animations are currently running
     */
    hasActiveAnimations() {
        return this.animationManager.hasActiveAnimations();
    }

    /**
     * Clear all animations
     */
    clearAllAnimations() {
        this.animationManager.clearAll();
        this.screenShakeOffset = { x: 0, y: 0 };
    }

    /**
     * Render placeholder content for initial implementation
     */
    renderPlaceholder() {
        const ctx = this.gameContext;
        
        // Draw background
        ctx.fillStyle = '#2D3748';
        ctx.fillRect(0, 0, this.gameCanvas.width, this.gameCanvas.height);
        
        // Draw grid
        this.drawGrid(ctx);
        
        // Draw placeholder text
        ctx.fillStyle = '#FFFFFF';
        ctx.font = '24px Arial';
        ctx.fillText(
            'ぷよぷよゲーム',
            this.gameCanvas.width / 2,
            this.gameCanvas.height / 2 - 30
        );
        
        ctx.font = '16px Arial';
        ctx.fillText(
            '開発中...',
            this.gameCanvas.width / 2,
            this.gameCanvas.height / 2 + 10
        );
    }

    /**
     * Resize canvas to fit container
     */
    resize() {
        // Get container dimensions
        const container = this.gameCanvas.parentElement;
        if (!container) return;
        
        const containerRect = container.getBoundingClientRect();
        const aspectRatio = this.fieldWidth / this.fieldHeight;
        
        let newWidth = containerRect.width;
        let newHeight = newWidth / aspectRatio;
        
        if (newHeight > containerRect.height) {
            newHeight = containerRect.height;
            newWidth = newHeight * aspectRatio;
        }
        
        // Update canvas size
        this.gameCanvas.width = newWidth;
        this.gameCanvas.height = newHeight;
        
        // Recalculate cell size
        this.cellSize = Math.min(newWidth / this.fieldWidth, newHeight / this.fieldHeight);
        
        // Reinitialize canvas settings
        this.initializeCanvas();
    }
}