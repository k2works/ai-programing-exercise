class Puyo {
    static COLORS = {
        RED: '#ff6b6b',
        BLUE: '#4ecdc4',
        GREEN: '#45b7d1',
        YELLOW: '#feca57',
        PURPLE: '#a55eea'
    };

    static COLOR_NAMES = ['RED', 'BLUE', 'GREEN', 'YELLOW', 'PURPLE'];

    constructor(color, x = 0, y = 0) {
        this.color = color;
        this.x = x;
        this.y = y;
        this.falling = false;
        this.connected = false;
    }

    static getRandomColor() {
        const randomIndex = Math.floor(Math.random() * this.COLOR_NAMES.length);
        return this.COLOR_NAMES[randomIndex];
    }

    static getColorValue(colorName) {
        return this.COLORS[colorName];
    }

    clone() {
        return new Puyo(this.color, this.x, this.y);
    }

    equals(other) {
        return this.color === other.color;
    }
}

class PuyoPair {
    constructor() {
        this.main = new Puyo(Puyo.getRandomColor());
        this.sub = new Puyo(Puyo.getRandomColor());
        this.x = 2;
        this.y = 0;
        this.rotation = 0;
    }

    getPositions() {
        const positions = [];
        positions.push({ x: this.x, y: this.y, puyo: this.main });

        switch (this.rotation) {
            case 0:
                positions.push({ x: this.x, y: this.y - 1, puyo: this.sub });
                break;
            case 1:
                positions.push({ x: this.x + 1, y: this.y, puyo: this.sub });
                break;
            case 2:
                positions.push({ x: this.x, y: this.y + 1, puyo: this.sub });
                break;
            case 3:
                positions.push({ x: this.x - 1, y: this.y, puyo: this.sub });
                break;
        }

        return positions;
    }

    rotate(clockwise = true) {
        if (clockwise) {
            this.rotation = (this.rotation + 1) % 4;
        } else {
            this.rotation = (this.rotation + 3) % 4;
        }
    }

    moveLeft() {
        this.x--;
    }

    moveRight() {
        this.x++;
    }

    moveDown() {
        this.y++;
    }

    reset(x = 2, y = 0) {
        this.main = new Puyo(Puyo.getRandomColor());
        this.sub = new Puyo(Puyo.getRandomColor());
        this.x = x;
        this.y = y;
        this.rotation = 0;
    }
}