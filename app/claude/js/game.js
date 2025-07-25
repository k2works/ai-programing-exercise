class PuyoGame {
    constructor() {
        this.field = new Field();
        this.renderer = new Renderer(
            document.getElementById('game-canvas'),
            document.getElementById('next-canvas')
        );
        this.inputHandler = new InputHandler(this);
        
        this.currentPuyo = null;
        this.nextPuyo = new PuyoPair();
        this.score = 0;
        this.level = 1;
        this.lines = 0;
        
        this.gameOver = false;
        this.paused = false;
        this.lastTime = 0;
        this.fallTime = 0;
        this.fallSpeed = 1000;
        
        this.init();
    }

    init() {
        this.spawnNewPuyo();
        this.render();
        this.gameLoop();
    }

    spawnNewPuyo() {
        this.currentPuyo = this.nextPuyo;
        this.nextPuyo = new PuyoPair();
        
        if (!this.field.canPlacePuyoPair(this.currentPuyo)) {
            this.endGame();
            return false;
        }
        
        return true;
    }

    gameLoop(currentTime = 0) {
        if (this.gameOver) return;
        
        const deltaTime = currentTime - this.lastTime;
        this.lastTime = currentTime;
        
        if (!this.paused) {
            this.update(deltaTime);
            this.render();
        }
        
        requestAnimationFrame((time) => this.gameLoop(time));
    }

    update(deltaTime) {
        this.fallTime += deltaTime;
        
        if (this.fallTime >= this.fallSpeed) {
            this.fallTime = 0;
            this.moveDown(true);
        }
    }

    moveLeft() {
        if (!this.currentPuyo || this.gameOver || this.paused) return;
        
        const testPuyo = this.clonePuyoPair(this.currentPuyo);
        testPuyo.moveLeft();
        
        if (this.field.canPlacePuyoPair(testPuyo)) {
            this.currentPuyo.moveLeft();
        }
    }

    moveRight() {
        if (!this.currentPuyo || this.gameOver || this.paused) return;
        
        const testPuyo = this.clonePuyoPair(this.currentPuyo);
        testPuyo.moveRight();
        
        if (this.field.canPlacePuyoPair(testPuyo)) {
            this.currentPuyo.moveRight();
        }
    }

    moveDown(auto = false) {
        if (!this.currentPuyo || this.gameOver || this.paused) return;
        
        const testPuyo = this.clonePuyoPair(this.currentPuyo);
        testPuyo.moveDown();
        
        if (this.field.canPlacePuyoPair(testPuyo)) {
            this.currentPuyo.moveDown();
            if (!auto) {
                this.addScore(1);
            }
        } else {
            this.placePuyo();
        }
    }

    rotate() {
        if (!this.currentPuyo || this.gameOver || this.paused) return;
        
        const testPuyo = this.clonePuyoPair(this.currentPuyo);
        testPuyo.rotate();
        
        let canRotate = this.field.canPlacePuyoPair(testPuyo);
        
        if (!canRotate && testPuyo.x > 0) {
            testPuyo.x--;
            canRotate = this.field.canPlacePuyoPair(testPuyo);
        }
        
        if (!canRotate && testPuyo.x < this.field.width - 1) {
            testPuyo.x += 2;
            canRotate = this.field.canPlacePuyoPair(testPuyo);
        }
        
        if (canRotate) {
            this.currentPuyo.rotation = testPuyo.rotation;
            this.currentPuyo.x = testPuyo.x;
        }
    }

    placePuyo() {
        if (!this.currentPuyo) return;
        
        this.field.placePuyoPair(this.currentPuyo);
        this.currentPuyo = null;
        
        this.processChains();
        
        if (!this.spawnNewPuyo()) {
            this.endGame();
        }
    }

    processChains() {
        let chainCount = 0;
        let totalCleared = 0;
        
        while (true) {
            this.field.applyGravity();
            
            const clearedCount = this.field.clearPuyos();
            if (clearedCount === 0) break;
            
            chainCount++;
            totalCleared += clearedCount;
            
            this.addScore(clearedCount * 10 * chainCount);
            
            this.animateChain();
        }
        
        if (chainCount > 1) {
            this.addScore(chainCount * 50);
        }
    }

    animateChain() {
        // 簡単なアニメーション効果
        // 実際のゲームではより複雑なアニメーションを実装
    }

    addScore(points) {
        this.score += points;
        this.updateLevel();
    }

    updateLevel() {
        const newLevel = Math.floor(this.score / 1000) + 1;
        if (newLevel !== this.level) {
            this.level = newLevel;
            this.fallSpeed = Math.max(100, 1000 - (this.level - 1) * 50);
        }
    }

    clonePuyoPair(puyoPair) {
        const clone = new PuyoPair();
        clone.main = puyoPair.main.clone();
        clone.sub = puyoPair.sub.clone();
        clone.x = puyoPair.x;
        clone.y = puyoPair.y;
        clone.rotation = puyoPair.rotation;
        return clone;
    }

    pause() {
        this.paused = true;
    }

    resume() {
        this.paused = false;
    }

    togglePause() {
        this.paused = !this.paused;
    }

    endGame() {
        this.gameOver = true;
        this.showGameOver();
    }

    showGameOver() {
        document.getElementById('game-over').classList.remove('hidden');
    }

    hideGameOver() {
        document.getElementById('game-over').classList.add('hidden');
    }

    restart() {
        this.field.clear();
        this.currentPuyo = null;
        this.nextPuyo = new PuyoPair();
        this.score = 0;
        this.level = 1;
        this.fallSpeed = 1000;
        this.gameOver = false;
        this.paused = false;
        this.fallTime = 0;
        
        this.hideGameOver();
        this.spawnNewPuyo();
        this.render();
        
        if (!this.gameLoop) {
            this.gameLoop();
        }
    }

    render() {
        this.renderer.render(this.field, this.currentPuyo, this.nextPuyo, this.score);
    }
}

let game;

document.addEventListener('DOMContentLoaded', () => {
    game = new PuyoGame();
});