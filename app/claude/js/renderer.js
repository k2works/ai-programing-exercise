class Renderer {
    constructor(canvas, nextCanvas) {
        this.canvas = canvas;
        this.ctx = canvas.getContext('2d');
        this.nextCanvas = nextCanvas;
        this.nextCtx = nextCanvas.getContext('2d');
        
        this.cellSize = 25;
        this.borderWidth = 1;
        
        this.canvas.width = 300;
        this.canvas.height = 600;
        this.nextCanvas.width = 80;
        this.nextCanvas.height = 80;
        
        this.fieldWidth = 6;
        this.fieldHeight = 12;
        
        this.offsetX = (this.canvas.width - (this.fieldWidth * this.cellSize)) / 2;
        this.offsetY = (this.canvas.height - (this.fieldHeight * this.cellSize)) / 2;
    }

    clear() {
        this.ctx.fillStyle = '#000';
        this.ctx.fillRect(0, 0, this.canvas.width, this.canvas.height);
        
        this.nextCtx.fillStyle = '#000';
        this.nextCtx.fillRect(0, 0, this.nextCanvas.width, this.nextCanvas.height);
    }

    drawGrid() {
        this.ctx.strokeStyle = '#333';
        this.ctx.lineWidth = 1;
        
        for (let x = 0; x <= this.fieldWidth; x++) {
            const xPos = this.offsetX + x * this.cellSize;
            this.ctx.beginPath();
            this.ctx.moveTo(xPos, this.offsetY);
            this.ctx.lineTo(xPos, this.offsetY + this.fieldHeight * this.cellSize);
            this.ctx.stroke();
        }
        
        for (let y = 0; y <= this.fieldHeight; y++) {
            const yPos = this.offsetY + y * this.cellSize;
            this.ctx.beginPath();
            this.ctx.moveTo(this.offsetX, yPos);
            this.ctx.lineTo(this.offsetX + this.fieldWidth * this.cellSize, yPos);
            this.ctx.stroke();
        }
    }

    drawPuyo(ctx, x, y, color, size = this.cellSize, offsetX = this.offsetX, offsetY = this.offsetY) {
        const xPos = offsetX + x * size + this.borderWidth;
        const yPos = offsetY + y * size + this.borderWidth;
        const puyoSize = size - this.borderWidth * 2;
        
        const gradient = ctx.createRadialGradient(
            xPos + puyoSize * 0.3, 
            yPos + puyoSize * 0.3, 
            0,
            xPos + puyoSize * 0.5, 
            yPos + puyoSize * 0.5, 
            puyoSize * 0.6
        );
        
        gradient.addColorStop(0, this.lightenColor(color, 0.3));
        gradient.addColorStop(1, color);
        
        ctx.fillStyle = gradient;
        ctx.beginPath();
        ctx.arc(
            xPos + puyoSize / 2, 
            yPos + puyoSize / 2, 
            puyoSize / 2 - 1, 
            0, 
            2 * Math.PI
        );
        ctx.fill();
        
        ctx.strokeStyle = this.darkenColor(color, 0.2);
        ctx.lineWidth = 2;
        ctx.stroke();
        
        ctx.fillStyle = 'rgba(255, 255, 255, 0.4)';
        ctx.beginPath();
        ctx.arc(
            xPos + puyoSize * 0.35, 
            yPos + puyoSize * 0.35, 
            puyoSize * 0.15, 
            0, 
            2 * Math.PI
        );
        ctx.fill();
    }

    lightenColor(color, factor) {
        const hex = color.replace('#', '');
        const r = Math.min(255, parseInt(hex.substr(0, 2), 16) + Math.floor(255 * factor));
        const g = Math.min(255, parseInt(hex.substr(2, 2), 16) + Math.floor(255 * factor));
        const b = Math.min(255, parseInt(hex.substr(4, 2), 16) + Math.floor(255 * factor));
        return `rgb(${r}, ${g}, ${b})`;
    }

    darkenColor(color, factor) {
        const hex = color.replace('#', '');
        const r = Math.max(0, parseInt(hex.substr(0, 2), 16) - Math.floor(255 * factor));
        const g = Math.max(0, parseInt(hex.substr(2, 2), 16) - Math.floor(255 * factor));
        const b = Math.max(0, parseInt(hex.substr(4, 2), 16) - Math.floor(255 * factor));
        return `rgb(${r}, ${g}, ${b})`;
    }

    drawField(field) {
        for (let y = 0; y < field.height; y++) {
            for (let x = 0; x < field.width; x++) {
                const puyo = field.getPuyo(x, y);
                if (puyo) {
                    const color = Puyo.getColorValue(puyo.color);
                    this.drawPuyo(this.ctx, x, y, color);
                }
            }
        }
    }

    drawFallingPuyo(puyoPair) {
        const positions = puyoPair.getPositions();
        positions.forEach(pos => {
            const color = Puyo.getColorValue(pos.puyo.color);
            this.drawPuyo(this.ctx, pos.x, pos.y, color);
        });
    }

    drawNextPuyo(puyoPair) {
        this.nextCtx.fillStyle = '#000';
        this.nextCtx.fillRect(0, 0, this.nextCanvas.width, this.nextCanvas.height);
        
        const cellSize = 20;
        const offsetX = (this.nextCanvas.width - cellSize * 2) / 2;
        const offsetY = (this.nextCanvas.height - cellSize * 2) / 2;
        
        const mainColor = Puyo.getColorValue(puyoPair.main.color);
        const subColor = Puyo.getColorValue(puyoPair.sub.color);
        
        this.drawPuyo(this.nextCtx, 0, 1, mainColor, cellSize, offsetX, offsetY);
        this.drawPuyo(this.nextCtx, 0, 0, subColor, cellSize, offsetX, offsetY);
    }

    drawScore(score) {
        document.getElementById('score').textContent = score.toLocaleString();
    }

    render(field, currentPuyo, nextPuyo, score) {
        this.clear();
        this.drawGrid();
        this.drawField(field);
        
        if (currentPuyo) {
            this.drawFallingPuyo(currentPuyo);
        }
        
        if (nextPuyo) {
            this.drawNextPuyo(nextPuyo);
        }
        
        this.drawScore(score);
    }
}