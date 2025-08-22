/**
 * Renderer - Handles all visual output using HTML5 Canvas
 * Manages rendering of game field, puyo, UI elements, and animations
 */

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
    renderPuyo(puyo, x, y, ctx = this.gameContext) {
        if (!puyo || !puyo.color) return;
        
        const centerX = (x + 0.5) * this.cellSize;
        const centerY = (y + 0.5) * this.cellSize;
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
    }

    /**
     * Render UI elements (score, chains, etc.)
     */
    renderUI(score, chains, nextPuyo) {
        // UI is handled by HTML elements, but we can add canvas-based UI here if needed
        // For now, this is a placeholder for future canvas-based UI elements
    }

    /**
     * Render next puyo preview
     */
    renderNextPuyo(puyoPair) {
        if (!puyoPair) return;
        
        const ctx = this.nextPuyoContext;
        const canvasWidth = this.nextPuyoCanvas.width;
        const canvasHeight = this.nextPuyoCanvas.height;
        
        // Clear canvas
        ctx.clearRect(0, 0, canvasWidth, canvasHeight);
        
        // Draw background
        ctx.fillStyle = '#F7FAFC';
        ctx.fillRect(0, 0, canvasWidth, canvasHeight);
        
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
     * Render animation frame
     */
    renderAnimation(animation) {
        // Placeholder for animation rendering
        // Will be implemented in later tasks
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