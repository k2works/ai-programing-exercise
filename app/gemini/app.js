// DOM要素の取得
const gameCanvas = document.getElementById('game-canvas');
const nextCanvas = document.getElementById('next-canvas');
const scoreElement = document.getElementById('score');
const ctx = gameCanvas.getContext('2d');
const nextCtx = nextCanvas.getContext('2d');

// ゲーム設定
const COLS = 10;
const ROWS = 20;
const BLOCK_SIZE = 32;
const PUYO_TYPES = 2; // 軸ぷよ、操作ぷよ
const COLORS = ['#EF4444', '#22C55E', '#3B82F6', '#EAB308', '#8B5CF6']; // Red, Green, Blue, Yellow, Purple

// ゲームの状態
let grid;
let currentPuyo;
let nextPuyo;
let score;
let gameOver;
let fallInterval;

// 初期化処理
function init() {
    grid = Array.from({ length: ROWS }, () => Array(COLS).fill(null));
    score = 0;
    gameOver = false;
    updateScore();

    currentPuyo = createPuyoPair();
    nextPuyo = createPuyoPair();
    
    if (fallInterval) clearInterval(fallInterval);
    fallInterval = setInterval(gameLoop, 1000);
    
    draw();
}

// ぷよのペアを生成する
function createPuyoPair() {
    return {
        puyos: [
            { x: Math.floor(COLS / 2), y: 0, color: COLORS[Math.floor(Math.random() * COLORS.length)] },
            { x: Math.floor(COLS / 2), y: -1, color: COLORS[Math.floor(Math.random() * COLORS.length)] }
        ],
        rotation: 0 // 0:上, 1:右, 2:下, 3:左
    };
}

// ゲームループ
function gameLoop() {
    if (gameOver) {
        clearInterval(fallInterval);
        alert('ゲームオーバー！');
        return;
    }
    movePuyo('down');
}

// ぷよの移動
function movePuyo(direction) {
    let newPuyos = JSON.parse(JSON.stringify(currentPuyo.puyos));
    let moved = true;

    switch (direction) {
        case 'down':
            newPuyos.forEach(p => p.y++);
            break;
        case 'left':
            newPuyos.forEach(p => p.x--);
            break;
        case 'right':
            newPuyos.forEach(p => p.x++);
            break;
    }

    if (isValidMove(newPuyos)) {
        currentPuyo.puyos = newPuyos;
    } else {
        if (direction === 'down') {
            lockPuyo();
        }
        moved = false;
    }
    draw();
    return moved;
}

// ぷよの回転
function rotatePuyo() {
    let p = currentPuyo.puyos;
    let axis = p[0]; // 軸ぷよ
    let target = p[1]; // 操作ぷよ
    let newTargetPos = { x: target.x, y: target.y };
    
    const dx = target.x - axis.x;
    const dy = target.y - axis.y;

    newTargetPos.x = axis.x - dy;
    newTargetPos.y = axis.y + dx;

    let newPuyos = [axis, newTargetPos];

    if (isValidMove(newPuyos)) {
        currentPuyo.puyos = newPuyos;
    }
    draw();
}


// ぷよをグリッドに固定する
async function lockPuyo() {
    currentPuyo.puyos.forEach(p => {
        if (p.y >= 0) {
            grid[p.y][p.x] = p.color;
        }
    });

    let chain = 1;
    let totalScore = 0;
    while (true) {
        const clearedInfo = await findAndClearPuyos();
        if (clearedInfo.clearedCount === 0) {
            break;
        }
        totalScore += calculateScore(clearedInfo.clearedCount, chain);
        chain++;
        await applyGravity();
        draw();
    }
    score += totalScore;
    updateScore();

    currentPuyo = nextPuyo;
    nextPuyo = createPuyoPair();

    if (!isValidMove(currentPuyo.puyos)) {
        gameOver = true;
    }
    draw();
}

// 連結ぷよを探して消す
function findAndClearPuyos() {
    return new Promise(resolve => {
        setTimeout(() => {
            let clearedCount = 0;
            const toClear = new Set();
            
            for (let y = 0; y < ROWS; y++) {
                for (let x = 0; x < COLS; x++) {
                    if (grid[y][x]) {
                        const connected = findConnected(x, y);
                        if (connected.length >= 4) {
                            connected.forEach(p => toClear.add(`${p.x},${p.y}`));
                        }
                    }
                }
            }

            if (toClear.size > 0) {
                clearedCount = toClear.size;
                toClear.forEach(pStr => {
                    const [x, y] = pStr.split(',').map(Number);
                    grid[y][x] = null;
                });
            }
            resolve({ clearedCount });
        }, 300);
    });
}

// 連結しているぷよを探す (BFS)
function findConnected(startX, startY) {
    const color = grid[startY][startX];
    if (!color) return [];

    const queue = [{ x: startX, y: startY }];
    const visited = new Set([`${startX},${startY}`]);
    const connected = [];

    while (queue.length > 0) {
        const { x, y } = queue.shift();
        connected.push({ x, y });

        const neighbors = [
            { x: x + 1, y }, { x: x - 1, y },
            { x, y: y + 1 }, { x, y: y - 1 }
        ];

        for (const n of neighbors) {
            if (n.x >= 0 && n.x < COLS && n.y >= 0 && n.y < ROWS &&
                grid[n.y][n.x] === color && !visited.has(`${n.x},${n.y}`)) {
                visited.add(`${n.x},${n.y}`);
                queue.push(n);
            }
        }
    }
    return connected;
}

// 重力を適用してぷよを落とす
function applyGravity() {
     return new Promise(resolve => {
        setTimeout(() => {
            for (let x = 0; x < COLS; x++) {
                let emptyRow = ROWS - 1;
                for (let y = ROWS - 1; y >= 0; y--) {
                    if (grid[y][x]) {
                        [grid[emptyRow][x], grid[y][x]] = [grid[y][x], grid[emptyRow][x]];
                        emptyRow--;
                    }
                }
            }
            resolve();
        }, 300);
    });
}

// スコア計算
function calculateScore(cleared, chain) {
    const chainBonus = [0, 8, 16, 32, 64, 96, 128, 160, 192, 224, 256, 288, 320, 352, 384, 416, 448, 480, 512];
    let bonus = chainBonus[chain-1] || chainBonus[chainBonus.length-1];
    return cleared * 10 + bonus;
}

// スコア表示更新
function updateScore() {
    scoreElement.textContent = score;
}

// 移動が有効かチェック
function isValidMove(puyos) {
    return puyos.every(p =>
        p.x >= 0 && p.x < COLS && p.y < ROWS &&
        (p.y < 0 || !grid[p.y][p.x])
    );
}

// 描画処理
function draw() {
    ctx.clearRect(0, 0, gameCanvas.width, gameCanvas.height);
    drawGrid();
    if (currentPuyo) drawPuyoPair(currentPuyo, ctx);
    
    nextCtx.clearRect(0, 0, nextCanvas.width, nextCanvas.height);
    if (nextPuyo) {
        // ネクストぷよを中央に描画
        const offsetX = (nextCanvas.width / BLOCK_SIZE - 1) / 2;
        const offsetY = (nextCanvas.height / BLOCK_SIZE - 2) / 2;
        drawPuyoPair(nextPuyo, nextCtx, {x: offsetX, y: offsetY});
    }
}

// グリッドを描画
function drawGrid() {
    for (let y = 0; y < ROWS; y++) {
        for (let x = 0; x < COLS; x++) {
            if (grid[y][x]) {
                drawBlock(x, y, grid[y][x], ctx);
            }
        }
    }
}

// ぷよのペアを描画
function drawPuyoPair(puyoPair, context, offset = {x: 0, y: 0}) {
    puyoPair.puyos.forEach(p => {
        drawBlock(p.x + offset.x, p.y + offset.y, p.color, context);
    });
}

// ブロック単体を描画
function drawBlock(x, y, color, context) {
    if (y < 0) return;
    context.fillStyle = color;
    const radius = BLOCK_SIZE / 2;
    context.beginPath();
    context.arc(x * BLOCK_SIZE + radius, y * BLOCK_SIZE + radius, radius * 0.9, 0, Math.PI * 2);
    context.fill();
}

// キーボード操作
document.addEventListener('keydown', (e) => {
    if (gameOver) return;

    switch (e.key) {
        case 'ArrowLeft':
            movePuyo('left');
            break;
        case 'ArrowRight':
            movePuyo('right');
            break;
        case 'ArrowDown':
            movePuyo('down');
            break;
        case 'ArrowUp':
        case 'z':
            rotatePuyo();
            break;
    }
});

// ゲーム開始
init();
