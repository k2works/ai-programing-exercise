class InputHandler {
    constructor(game) {
        this.game = game;
        this.keys = {};
        this.keyRepeatDelay = 150;
        this.keyRepeatRate = 50;
        this.keyTimers = {};
        
        this.bindEvents();
    }

    bindEvents() {
        document.addEventListener('keydown', (e) => this.handleKeyDown(e));
        document.addEventListener('keyup', (e) => this.handleKeyUp(e));
        
        document.getElementById('restart-btn').addEventListener('click', () => {
            this.game.restart();
        });
        
        document.addEventListener('visibilitychange', () => {
            if (document.hidden) {
                this.game.pause();
            }
        });
    }

    handleKeyDown(e) {
        if (this.game.gameOver) return;
        
        const key = e.code;
        
        if (!this.keys[key]) {
            this.keys[key] = true;
            this.processKeyPress(key);
            
            if (this.isRepeatableKey(key)) {
                this.keyTimers[key] = setTimeout(() => {
                    this.startKeyRepeat(key);
                }, this.keyRepeatDelay);
            }
        }
        
        if (['ArrowLeft', 'ArrowRight', 'ArrowDown', 'Space'].includes(key)) {
            e.preventDefault();
        }
    }

    handleKeyUp(e) {
        const key = e.code;
        this.keys[key] = false;
        
        if (this.keyTimers[key]) {
            clearTimeout(this.keyTimers[key]);
            clearInterval(this.keyTimers[key]);
            delete this.keyTimers[key];
        }
    }

    isRepeatableKey(key) {
        return ['ArrowLeft', 'ArrowRight', 'ArrowDown'].includes(key);
    }

    startKeyRepeat(key) {
        this.keyTimers[key] = setInterval(() => {
            if (this.keys[key]) {
                this.processKeyPress(key);
            } else {
                clearInterval(this.keyTimers[key]);
                delete this.keyTimers[key];
            }
        }, this.keyRepeatRate);
    }

    processKeyPress(key) {
        if (this.game.gameOver) return;
        
        switch (key) {
            case 'ArrowLeft':
                this.game.moveLeft();
                break;
            case 'ArrowRight':
                this.game.moveRight();
                break;
            case 'ArrowDown':
                this.game.moveDown();
                break;
            case 'Space':
                this.game.rotate();
                break;
            case 'KeyP':
                this.game.togglePause();
                break;
            case 'KeyR':
                if (this.game.gameOver) {
                    this.game.restart();
                }
                break;
            case 'Escape':
                this.game.togglePause();
                break;
        }
    }

    cleanup() {
        Object.values(this.keyTimers).forEach(timer => {
            clearTimeout(timer);
            clearInterval(timer);
        });
        this.keyTimers = {};
        this.keys = {};
    }
}