class Field {
    constructor(width = 6, height = 12) {
        this.width = width;
        this.height = height;
        this.grid = this.createEmptyGrid();
    }

    createEmptyGrid() {
        const grid = [];
        for (let y = 0; y < this.height; y++) {
            grid[y] = [];
            for (let x = 0; x < this.width; x++) {
                grid[y][x] = null;
            }
        }
        return grid;
    }

    isValidPosition(x, y) {
        return x >= 0 && x < this.width && y >= 0 && y < this.height;
    }

    isEmpty(x, y) {
        if (!this.isValidPosition(x, y)) return false;
        return this.grid[y][x] === null;
    }

    setPuyo(x, y, puyo) {
        if (this.isValidPosition(x, y)) {
            this.grid[y][x] = puyo ? puyo.clone() : null;
            if (this.grid[y][x]) {
                this.grid[y][x].x = x;
                this.grid[y][x].y = y;
            }
        }
    }

    getPuyo(x, y) {
        if (!this.isValidPosition(x, y)) return null;
        return this.grid[y][x];
    }

    canPlacePuyoPair(puyoPair) {
        const positions = puyoPair.getPositions();
        return positions.every(pos => 
            this.isValidPosition(pos.x, pos.y) && this.isEmpty(pos.x, pos.y)
        );
    }

    placePuyoPair(puyoPair) {
        if (!this.canPlacePuyoPair(puyoPair)) return false;
        
        const positions = puyoPair.getPositions();
        positions.forEach(pos => {
            this.setPuyo(pos.x, pos.y, pos.puyo);
        });
        return true;
    }

    applyGravity() {
        let moved = false;
        for (let x = 0; x < this.width; x++) {
            for (let y = this.height - 2; y >= 0; y--) {
                if (this.grid[y][x] !== null) {
                    let newY = y;
                    while (newY + 1 < this.height && this.grid[newY + 1][x] === null) {
                        newY++;
                        moved = true;
                    }
                    if (newY !== y) {
                        this.grid[newY][x] = this.grid[y][x];
                        this.grid[newY][x].y = newY;
                        this.grid[y][x] = null;
                    }
                }
            }
        }
        return moved;
    }

    findConnectedPuyos(startX, startY, visited = new Set()) {
        const puyo = this.getPuyo(startX, startY);
        if (!puyo || visited.has(`${startX},${startY}`)) return [];

        visited.add(`${startX},${startY}`);
        const connected = [{ x: startX, y: startY }];

        const directions = [
            { dx: 0, dy: -1 },
            { dx: 1, dy: 0 },
            { dx: 0, dy: 1 },
            { dx: -1, dy: 0 }
        ];

        directions.forEach(dir => {
            const newX = startX + dir.dx;
            const newY = startY + dir.dy;
            const neighborPuyo = this.getPuyo(newX, newY);

            if (neighborPuyo && neighborPuyo.equals(puyo) && !visited.has(`${newX},${newY}`)) {
                connected.push(...this.findConnectedPuyos(newX, newY, visited));
            }
        });

        return connected;
    }

    clearPuyos() {
        const allVisited = new Set();
        const toClear = [];

        for (let y = 0; y < this.height; y++) {
            for (let x = 0; x < this.width; x++) {
                if (this.grid[y][x] && !allVisited.has(`${x},${y}`)) {
                    const connected = this.findConnectedPuyos(x, y, new Set());
                    connected.forEach(pos => allVisited.add(`${pos.x},${pos.y}`));

                    if (connected.length >= 4) {
                        toClear.push(...connected);
                    }
                }
            }
        }

        toClear.forEach(pos => {
            this.grid[pos.y][pos.x] = null;
        });

        return toClear.length;
    }

    isGameOver() {
        for (let x = 0; x < this.width; x++) {
            if (this.grid[0][x] !== null) {
                return true;
            }
        }
        return false;
    }

    clear() {
        this.grid = this.createEmptyGrid();
    }

    clone() {
        const newField = new Field(this.width, this.height);
        for (let y = 0; y < this.height; y++) {
            for (let x = 0; x < this.width; x++) {
                if (this.grid[y][x]) {
                    newField.setPuyo(x, y, this.grid[y][x]);
                }
            }
        }
        return newField;
    }
}