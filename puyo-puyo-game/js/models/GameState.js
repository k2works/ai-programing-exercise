/**
 * GameState - Manages the overall game state and transitions
 * Tracks field, score, level, current/next puyo pairs, and game status
 */

import { Puyo } from './Puyo.js';
import { PuyoPair } from './PuyoPair.js';
import { ScoreManager } from './ScoreManager.js';
import { ZenkeshiManager } from './ZenkeshiManager.js';

export class GameState {
    // Game status constants
    static STATUS = {
        PLAYING: 'playing',
        PAUSED: 'paused',
        GAME_OVER: 'gameOver'
    };

    // Field dimensions
    static FIELD_WIDTH = 6;
    static FIELD_HEIGHT = 12;

    /**
     * Create a new GameState instance
     */
    constructor() {
        this.initializeState();
    }

    /**
     * Initialize the game state to default values
     */
    initializeState() {
        // Create empty field (12 rows x 6 columns)
        this.field = new Array(GameState.FIELD_HEIGHT)
            .fill(null)
            .map(() => new Array(GameState.FIELD_WIDTH).fill(null));
        
        this.score = 0;
        this.level = 1;
        this.currentPair = null;
        this.nextPair = null;
        this.gameStatus = GameState.STATUS.PLAYING;
        this.chainCount = 0;
        this.lastClearTime = 0;
        
        // Additional game state properties
        this.linesCleared = 0;
        this.totalPuyosCleared = 0;
        this.gameStartTime = Date.now();
        this.gameDuration = 0;
        this.highestChain = 0;
        this.zenkeshiCount = 0; // All-clear count
        
        // Initialize score manager and zenkeshi manager
        this.scoreManager = new ScoreManager();
        this.zenkeshiManager = new ZenkeshiManager();
    }

    /**
     * Reset the game state to initial values
     */
    reset() {
        this.initializeState();
    }

    /**
     * Get the current game status
     * @returns {string} Current game status
     */
    getStatus() {
        return this.gameStatus;
    }

    /**
     * Set the game status
     * @param {string} status - New game status ('playing', 'paused', 'gameOver')
     */
    setStatus(status) {
        if (!Object.values(GameState.STATUS).includes(status)) {
            throw new Error(`Invalid game status: ${status}. Valid statuses are: ${Object.values(GameState.STATUS).join(', ')}`);
        }
        
        const previousStatus = this.gameStatus;
        this.gameStatus = status;
        
        // Update game duration when transitioning to game over
        if (status === GameState.STATUS.GAME_OVER && previousStatus !== GameState.STATUS.GAME_OVER) {
            this.gameDuration = Date.now() - this.gameStartTime;
        }
    }

    /**
     * Check if the game is currently playing
     * @returns {boolean} True if game is playing
     */
    isPlaying() {
        return this.gameStatus === GameState.STATUS.PLAYING;
    }

    /**
     * Check if the game is paused
     * @returns {boolean} True if game is paused
     */
    isPaused() {
        return this.gameStatus === GameState.STATUS.PAUSED;
    }

    /**
     * Check if the game is over
     * @returns {boolean} True if game is over
     */
    isGameOver() {
        return this.gameStatus === GameState.STATUS.GAME_OVER;
    }

    /**
     * Pause the game
     */
    pause() {
        if (this.gameStatus === GameState.STATUS.PLAYING) {
            this.setStatus(GameState.STATUS.PAUSED);
        }
    }

    /**
     * Resume the game from pause
     */
    resume() {
        if (this.gameStatus === GameState.STATUS.PAUSED) {
            this.setStatus(GameState.STATUS.PLAYING);
        }
    }

    /**
     * End the game
     */
    gameOver() {
        this.setStatus(GameState.STATUS.GAME_OVER);
    }

    /**
     * Start a new game
     */
    startNewGame() {
        this.reset();
        this.setStatus(GameState.STATUS.PLAYING);
        
        // Start new game for zenkeshi manager
        if (this.zenkeshiManager) {
            this.zenkeshiManager.startNewGame();
        }
    }

    /**
     * Get field cell value
     * @param {number} x - X coordinate (0-5)
     * @param {number} y - Y coordinate (0-11)
     * @returns {Puyo|null} Puyo at the position or null if empty
     */
    getFieldCell(x, y) {
        this.validateFieldCoordinates(x, y);
        return this.field[y][x];
    }

    /**
     * Set field cell value
     * @param {number} x - X coordinate (0-5)
     * @param {number} y - Y coordinate (0-11)
     * @param {Puyo|null} puyo - Puyo to place or null to clear
     */
    setFieldCell(x, y, puyo) {
        this.validateFieldCoordinates(x, y);
        this.field[y][x] = puyo;
    }

    /**
     * Check if field coordinates are valid
     * @param {number} x - X coordinate
     * @param {number} y - Y coordinate
     * @returns {boolean} True if coordinates are valid
     */
    isValidFieldPosition(x, y) {
        return Number.isInteger(x) && Number.isInteger(y) &&
               x >= 0 && x < GameState.FIELD_WIDTH &&
               y >= 0 && y < GameState.FIELD_HEIGHT;
    }

    /**
     * Validate field coordinates
     * @param {number} x - X coordinate
     * @param {number} y - Y coordinate
     * @throws {Error} If coordinates are invalid
     */
    validateFieldCoordinates(x, y) {
        if (!this.isValidFieldPosition(x, y)) {
            throw new Error(`Invalid field coordinates: (${x}, ${y}). Valid range is (0-${GameState.FIELD_WIDTH-1}, 0-${GameState.FIELD_HEIGHT-1})`);
        }
    }

    /**
     * Check if a field position is empty
     * @param {number} x - X coordinate
     * @param {number} y - Y coordinate
     * @returns {boolean} True if position is empty
     */
    isFieldPositionEmpty(x, y) {
        if (!this.isValidFieldPosition(x, y)) {
            return false;
        }
        return this.field[y][x] === null;
    }

    /**
     * Clear a field position
     * @param {number} x - X coordinate
     * @param {number} y - Y coordinate
     */
    clearFieldPosition(x, y) {
        this.setFieldCell(x, y, null);
    }

    /**
     * Get a copy of the entire field
     * @returns {Array<Array<Puyo|null>>} Copy of the field array
     */
    getField() {
        return this.field.map(row => [...row]);
    }

    /**
     * Set the current falling puyo pair
     * @param {PuyoPair|null} pair - Puyo pair to set as current
     */
    setCurrentPair(pair) {
        if (pair !== null && !(pair instanceof PuyoPair)) {
            throw new Error('Current pair must be a PuyoPair instance or null');
        }
        this.currentPair = pair;
    }

    /**
     * Get the current falling puyo pair
     * @returns {PuyoPair|null} Current puyo pair
     */
    getCurrentPair() {
        return this.currentPair;
    }

    /**
     * Set the next puyo pair
     * @param {PuyoPair|null} pair - Puyo pair to set as next
     */
    setNextPair(pair) {
        if (pair !== null && !(pair instanceof PuyoPair)) {
            throw new Error('Next pair must be a PuyoPair instance or null');
        }
        this.nextPair = pair;
    }

    /**
     * Get the next puyo pair
     * @returns {PuyoPair|null} Next puyo pair
     */
    getNextPair() {
        return this.nextPair;
    }

    /**
     * Add points to the score
     * @param {number} points - Points to add
     */
    addScore(points) {
        if (typeof points !== 'number' || points < 0) {
            throw new Error('Points must be a non-negative number');
        }
        this.score += points;
        
        // Keep score manager in sync
        if (this.scoreManager) {
            this.scoreManager.totalScore = this.score;
        }
    }

    /**
     * Get the current score
     * @returns {number} Current score
     */
    getScore() {
        return this.score;
    }

    /**
     * Set the score directly
     * @param {number} score - New score value
     */
    setScore(score) {
        if (typeof score !== 'number' || score < 0) {
            throw new Error('Score must be a non-negative number');
        }
        this.score = score;
    }

    /**
     * Get the current level
     * @returns {number} Current level
     */
    getLevel() {
        return this.level;
    }

    /**
     * Set the level
     * @param {number} level - New level value
     */
    setLevel(level) {
        if (typeof level !== 'number' || level < 1) {
            throw new Error('Level must be a positive number');
        }
        this.level = level;
    }

    /**
     * Increase the level by 1
     */
    levelUp() {
        this.level++;
    }

    /**
     * Update chain count
     * @param {number} chainCount - New chain count
     */
    setChainCount(chainCount) {
        if (typeof chainCount !== 'number' || chainCount < 0) {
            throw new Error('Chain count must be a non-negative number');
        }
        this.chainCount = chainCount;
        
        // Update highest chain if this is a new record
        if (chainCount > this.highestChain) {
            this.highestChain = chainCount;
        }
    }

    /**
     * Get the current chain count
     * @returns {number} Current chain count
     */
    getChainCount() {
        return this.chainCount;
    }

    /**
     * Reset chain count to 0
     */
    resetChainCount() {
        this.chainCount = 0;
    }

    /**
     * Set the last clear time
     * @param {number} timestamp - Timestamp of last clear
     */
    setLastClearTime(timestamp) {
        if (typeof timestamp !== 'number') {
            throw new Error('Timestamp must be a number');
        }
        this.lastClearTime = timestamp;
    }

    /**
     * Get the last clear time
     * @returns {number} Last clear timestamp
     */
    getLastClearTime() {
        return this.lastClearTime;
    }

    /**
     * Increment the number of lines cleared
     * @param {number} lines - Number of lines cleared
     */
    addLinesCleared(lines) {
        if (typeof lines !== 'number' || lines < 0) {
            throw new Error('Lines must be a non-negative number');
        }
        this.linesCleared += lines;
    }

    /**
     * Get total lines cleared
     * @returns {number} Total lines cleared
     */
    getLinesCleared() {
        return this.linesCleared;
    }

    /**
     * Increment the number of puyos cleared
     * @param {number} puyos - Number of puyos cleared
     */
    addPuyosCleared(puyos) {
        if (typeof puyos !== 'number' || puyos < 0) {
            throw new Error('Puyos must be a non-negative number');
        }
        this.totalPuyosCleared += puyos;
    }

    /**
     * Get total puyos cleared
     * @returns {number} Total puyos cleared
     */
    getTotalPuyosCleared() {
        return this.totalPuyosCleared;
    }

    /**
     * Increment zenkeshi (all-clear) count
     */
    incrementZenkeshiCount() {
        this.zenkeshiCount++;
    }

    /**
     * Get zenkeshi count
     * @returns {number} Number of all-clears achieved
     */
    getZenkeshiCount() {
        return this.zenkeshiCount;
    }

    /**
     * Get the highest chain achieved
     * @returns {number} Highest chain count
     */
    getHighestChain() {
        return this.highestChain;
    }

    /**
     * Get game duration in milliseconds
     * @returns {number} Game duration
     */
    getGameDuration() {
        if (this.isGameOver()) {
            return this.gameDuration;
        } else {
            return Date.now() - this.gameStartTime;
        }
    }

    /**
     * Get game statistics
     * @returns {Object} Game statistics object
     */
    getStatistics() {
        return {
            score: this.score,
            level: this.level,
            linesCleared: this.linesCleared,
            totalPuyosCleared: this.totalPuyosCleared,
            chainCount: this.chainCount,
            highestChain: this.highestChain,
            zenkeshiCount: this.zenkeshiCount,
            gameDuration: this.getGameDuration(),
            gameStatus: this.gameStatus
        };
    }

    /**
     * Check if the field is completely empty (zenkeshi condition)
     * @returns {boolean} True if field is empty
     */
    isFieldEmpty() {
        for (let y = 0; y < GameState.FIELD_HEIGHT; y++) {
            for (let x = 0; x < GameState.FIELD_WIDTH; x++) {
                if (this.field[y][x] !== null) {
                    return false;
                }
            }
        }
        return true;
    }

    /**
     * Get the score manager
     * @returns {ScoreManager} Score manager instance
     */
    getScoreManager() {
        return this.scoreManager;
    }

    /**
     * Start a new chain sequence
     */
    startChain() {
        if (this.scoreManager) {
            this.scoreManager.startChain();
        }
        this.resetChainCount();
    }

    /**
     * Process cleared groups and update score
     * @param {Array<Array<Object>>} clearedGroups - Groups of cleared puyos
     * @param {boolean} isZenkeshi - Whether this is an all-clear
     * @returns {Object} Scoring results
     */
    processClearedGroups(clearedGroups, isZenkeshi = false) {
        if (!this.scoreManager) {
            return { score: 0, chainLink: null, zenkeshiBonus: 0 };
        }

        // Process zenkeshi if detected
        let zenkeshiResult = null;
        if (isZenkeshi && this.zenkeshiManager) {
            const clearData = {
                groups: clearedGroups,
                groupCount: clearedGroups.length,
                totalCleared: clearedGroups.reduce((sum, group) => sum + group.length, 0)
            };
            zenkeshiResult = this.zenkeshiManager.processZenkeshi(clearData, 0);
        } else if (this.zenkeshiManager) {
            // Reset consecutive zenkeshi if no zenkeshi occurred
            this.zenkeshiManager.resetConsecutiveZenkeshi();
        }

        const result = this.scoreManager.processClearingSequence(clearedGroups, isZenkeshi);
        
        // Update game state
        this.addScore(result.score);
        this.addPuyosCleared(result.totalPuyosCleared);
        this.setChainCount(this.scoreManager.getCurrentChainLevel());
        
        if (isZenkeshi) {
            this.incrementZenkeshiCount();
        }
        
        this.setLastClearTime(Date.now());
        
        // Add zenkeshi information to result
        if (zenkeshiResult) {
            result.zenkeshiResult = zenkeshiResult;
            result.zenkeshiDisplayInfo = this.zenkeshiManager.getDisplayInfo();
        }
        
        return result;
    }

    /**
     * End the current chain sequence
     * @returns {Object} Chain summary
     */
    endChain() {
        if (!this.scoreManager) {
            return { totalChainLinks: 0, totalScore: 0 };
        }

        const chainSummary = this.scoreManager.endChain();
        
        // Update highest chain if this was a record
        if (chainSummary.maxChainLevel > this.highestChain) {
            this.highestChain = chainSummary.maxChainLevel;
        }
        
        this.resetChainCount();
        
        return chainSummary;
    }

    /**
     * Get current chain level from score manager
     * @returns {number} Current chain level
     */
    getCurrentChainLevel() {
        return this.scoreManager ? this.scoreManager.getCurrentChainLevel() : 0;
    }

    /**
     * Check if chain is currently active
     * @returns {boolean} True if chain is active
     */
    isChainActive() {
        return this.scoreManager ? this.scoreManager.isChainActive() : false;
    }

    /**
     * Get the zenkeshi manager
     * @returns {ZenkeshiManager} Zenkeshi manager instance
     */
    getZenkeshiManager() {
        return this.zenkeshiManager;
    }

    /**
     * Check if zenkeshi effect is currently active
     * @returns {boolean} True if zenkeshi effect is active
     */
    isZenkeshiEffectActive() {
        return this.zenkeshiManager ? this.zenkeshiManager.isEffectActive() : false;
    }

    /**
     * Get zenkeshi display information
     * @returns {Object} Zenkeshi display info
     */
    getZenkeshiDisplayInfo() {
        return this.zenkeshiManager ? this.zenkeshiManager.getDisplayInfo() : null;
    }

    /**
     * Detect zenkeshi from current field state
     * @returns {boolean} True if field is empty (zenkeshi condition)
     */
    detectZenkeshi() {
        if (!this.zenkeshiManager) {
            return this.isFieldEmpty();
        }
        return this.zenkeshiManager.detectZenkeshiFromField(this.field);
    }

    /**
     * Check if the top row has any puyos (game over condition)
     * @returns {boolean} True if top row has puyos
     */
    isTopRowOccupied() {
        for (let x = 0; x < GameState.FIELD_WIDTH; x++) {
            if (this.field[0][x] !== null) {
                return true;
            }
        }
        return false;
    }

    /**
     * Create a copy of this game state
     * @returns {GameState} New game state instance with same properties
     */
    clone() {
        const cloned = new GameState();
        
        // Copy primitive properties
        cloned.score = this.score;
        cloned.level = this.level;
        cloned.gameStatus = this.gameStatus;
        cloned.chainCount = this.chainCount;
        cloned.lastClearTime = this.lastClearTime;
        cloned.linesCleared = this.linesCleared;
        cloned.totalPuyosCleared = this.totalPuyosCleared;
        cloned.gameStartTime = this.gameStartTime;
        cloned.gameDuration = this.gameDuration;
        cloned.highestChain = this.highestChain;
        cloned.zenkeshiCount = this.zenkeshiCount;
        
        // Deep copy field
        cloned.field = this.field.map(row => 
            row.map(puyo => puyo ? puyo.clone() : null)
        );
        
        // Copy puyo pairs
        cloned.currentPair = this.currentPair ? this.currentPair.clone() : null;
        cloned.nextPair = this.nextPair ? this.nextPair.clone() : null;
        
        // Copy score manager and zenkeshi manager
        cloned.scoreManager = this.scoreManager ? this.scoreManager.clone() : new ScoreManager();
        cloned.zenkeshiManager = this.zenkeshiManager ? this.zenkeshiManager.clone() : new ZenkeshiManager();
        
        return cloned;
    }

    /**
     * Convert game state to JSON object
     * @returns {Object} JSON representation
     */
    toJSON() {
        return {
            field: this.field.map(row => 
                row.map(puyo => puyo ? puyo.toJSON() : null)
            ),
            score: this.score,
            level: this.level,
            currentPair: this.currentPair ? this.currentPair.toJSON() : null,
            nextPair: this.nextPair ? this.nextPair.toJSON() : null,
            gameStatus: this.gameStatus,
            chainCount: this.chainCount,
            lastClearTime: this.lastClearTime,
            linesCleared: this.linesCleared,
            totalPuyosCleared: this.totalPuyosCleared,
            gameStartTime: this.gameStartTime,
            gameDuration: this.gameDuration,
            highestChain: this.highestChain,
            zenkeshiCount: this.zenkeshiCount,
            scoreManager: this.scoreManager ? this.scoreManager.toJSON() : null,
            zenkeshiManager: this.zenkeshiManager ? this.zenkeshiManager.toJSON() : null
        };
    }

    /**
     * Create game state from JSON object
     * @param {Object} json - JSON object with game state data
     * @returns {GameState} New game state instance
     */
    static fromJSON(json) {
        const gameState = new GameState();
        
        // Import field
        gameState.field = json.field.map(row =>
            row.map(puyoData => puyoData ? Puyo.fromJSON(puyoData) : null)
        );
        
        // Import primitive properties
        gameState.score = json.score;
        gameState.level = json.level;
        gameState.gameStatus = json.gameStatus;
        gameState.chainCount = json.chainCount;
        gameState.lastClearTime = json.lastClearTime;
        gameState.linesCleared = json.linesCleared || 0;
        gameState.totalPuyosCleared = json.totalPuyosCleared || 0;
        gameState.gameStartTime = json.gameStartTime || Date.now();
        gameState.gameDuration = json.gameDuration || 0;
        gameState.highestChain = json.highestChain || 0;
        gameState.zenkeshiCount = json.zenkeshiCount || 0;
        
        // Import puyo pairs
        gameState.currentPair = json.currentPair ? PuyoPair.fromJSON(json.currentPair) : null;
        gameState.nextPair = json.nextPair ? PuyoPair.fromJSON(json.nextPair) : null;
        
        // Import score manager and zenkeshi manager
        gameState.scoreManager = json.scoreManager ? ScoreManager.fromJSON(json.scoreManager) : new ScoreManager();
        gameState.zenkeshiManager = json.zenkeshiManager ? ZenkeshiManager.fromJSON(json.zenkeshiManager) : new ZenkeshiManager();
        
        return gameState;
    }
}