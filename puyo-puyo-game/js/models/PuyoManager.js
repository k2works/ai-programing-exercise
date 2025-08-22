/**
 * PuyoManager - Manages puyo lifecycle including spawning, movement, and placement
 * Handles the creation and management of falling puyo pairs
 */

import { Puyo } from './Puyo.js';
import { PuyoPair } from './PuyoPair.js';

export class PuyoManager {
    /**
     * Create a new PuyoManager instance
     * @param {Object} fieldManager - Field manager instance for collision detection
     */
    constructor(fieldManager) {
        if (!fieldManager) {
            throw new Error('FieldManager is required');
        }
        
        this.fieldManager = fieldManager;
        this.currentPair = null;
        this.nextPair = null;
        this.lastFallTime = 0;
        
        // Initialize with first pairs
        this.nextPair = this.createRandomPuyoPair();
        this.spawnNextPair();
    }

    /**
     * Create a random puyo pair with random colors
     * @param {number} x - Initial x position (default: 2, center of 6-wide field)
     * @param {number} y - Initial y position (default: 1)
     * @returns {PuyoPair} New random puyo pair
     */
    createRandomPuyoPair(x = 2, y = 1) {
        const colors = Puyo.COLORS;
        const color1 = colors[Math.floor(Math.random() * colors.length)];
        const color2 = colors[Math.floor(Math.random() * colors.length)];
        
        const puyo1 = new Puyo(color1);
        const puyo2 = new Puyo(color2);
        const pair = new PuyoPair(puyo1, puyo2);
        
        pair.setPosition(x, y);
        return pair;
    }

    /**
     * Spawn a new puyo pair at the top of the field
     * @returns {PuyoPair} The spawned puyo pair
     */
    spawnPuyoPair() {
        // Move next pair to current and generate new next pair
        this.currentPair = this.nextPair;
        this.nextPair = this.createRandomPuyoPair();
        
        // Reset current pair position to spawn location
        if (this.currentPair) {
            this.currentPair.setPosition(2, 1); // Center of field, y=1 to allow rotation
            this.currentPair.setFalling();
        }
        
        return this.currentPair;
    }

    /**
     * Spawn the next pair (used during initialization)
     */
    spawnNextPair() {
        this.spawnPuyoPair();
    }

    /**
     * Get the current falling puyo pair
     * @returns {PuyoPair|null} Current puyo pair
     */
    getCurrentPair() {
        return this.currentPair;
    }

    /**
     * Get the next puyo pair for preview
     * @returns {PuyoPair|null} Next puyo pair
     */
    getNextPair() {
        return this.nextPair;
    }

    /**
     * Check if there is a current falling pair
     * @returns {boolean} True if there is a current pair
     */
    hasCurrentPair() {
        return this.currentPair !== null;
    }

    /**
     * Move the current pair in a direction if valid
     * @param {string} direction - Direction to move ('left', 'right', 'down')
     * @returns {boolean} True if move was successful
     */
    movePair(direction) {
        if (!this.currentPair) {
            return false;
        }

        // Get potential new positions
        const potentialPositions = this.currentPair.getPotentialPositions(direction);
        
        // Check if move is valid
        if (this.isValidMove(potentialPositions)) {
            this.currentPair.move(direction);
            return true;
        }
        
        return false;
    }

    /**
     * Rotate the current pair clockwise if valid
     * @returns {boolean} True if rotation was successful
     */
    rotatePair() {
        if (!this.currentPair) {
            return false;
        }

        // Get potential positions after rotation
        const potentialPositions = this.currentPair.getPotentialRotationPositions();
        
        // Check if rotation is valid
        if (this.isValidMove(potentialPositions)) {
            this.currentPair.rotate();
            return true;
        }
        
        // Try wall kick - attempt to move left or right to make rotation valid
        const wallKickOffsets = [-1, 1]; // Try left first, then right
        
        for (const offset of wallKickOffsets) {
            const testPair = this.currentPair.clone();
            testPair.setPosition(testPair.x + offset, testPair.y);
            const testPositions = testPair.getPotentialRotationPositions();
            
            if (this.isValidMove(testPositions)) {
                // Wall kick successful - apply both move and rotation
                this.currentPair.setPosition(testPair.x, testPair.y);
                this.currentPair.rotate();
                return true;
            }
        }
        
        return false;
    }

    /**
     * Enable fast drop for the current pair
     * @returns {boolean} True if fast drop was enabled
     */
    dropPair() {
        if (!this.currentPair) {
            return false;
        }
        
        this.currentPair.enableFastDrop();
        return true;
    }

    /**
     * Disable fast drop for the current pair
     * @returns {boolean} True if fast drop was disabled
     */
    stopDrop() {
        if (!this.currentPair) {
            return false;
        }
        
        this.currentPair.disableFastDrop();
        return true;
    }

    /**
     * Check if the current pair can move down (for natural falling)
     * @returns {boolean} True if pair can fall
     */
    canPairFall() {
        if (!this.currentPair) {
            return false;
        }
        
        const potentialPositions = this.currentPair.getPotentialPositions('down');
        return this.isValidMove(potentialPositions);
    }

    /**
     * Make the current pair fall one step down
     * @returns {boolean} True if pair fell successfully
     */
    fallPair() {
        return this.movePair('down');
    }

    /**
     * Fix the current pair to the field when it can't fall anymore
     * @returns {boolean} True if pair was fixed successfully
     */
    fixPairToField() {
        if (!this.currentPair) {
            return false;
        }

        // Set puyos to fixed state
        this.currentPair.setFixed();
        
        // Place puyos in the field
        const positions = this.currentPair.getPuyoPositions();
        
        // Place puyo1
        if (this.fieldManager.isValidPosition(positions.puyo1.x, positions.puyo1.y)) {
            this.fieldManager.setCell(positions.puyo1.x, positions.puyo1.y, this.currentPair.puyo1);
        }
        
        // Place puyo2
        if (this.fieldManager.isValidPosition(positions.puyo2.x, positions.puyo2.y)) {
            this.fieldManager.setCell(positions.puyo2.x, positions.puyo2.y, this.currentPair.puyo2);
        }
        
        // Clear current pair and spawn next
        this.currentPair = null;
        this.spawnPuyoPair();
        
        return true;
    }

    /**
     * Check if a set of positions represents a valid move
     * @param {Object} positions - Positions object with puyo1 and puyo2 positions
     * @returns {boolean} True if all positions are valid and unoccupied
     */
    isValidMove(positions) {
        // Check puyo1 position
        if (!this.fieldManager.isValidPosition(positions.puyo1.x, positions.puyo1.y)) {
            return false;
        }
        
        if (!this.fieldManager.isEmpty(positions.puyo1.x, positions.puyo1.y)) {
            return false;
        }
        
        // Check puyo2 position
        if (!this.fieldManager.isValidPosition(positions.puyo2.x, positions.puyo2.y)) {
            return false;
        }
        
        if (!this.fieldManager.isEmpty(positions.puyo2.x, positions.puyo2.y)) {
            return false;
        }
        
        return true;
    }

    /**
     * Check if spawning a new pair would cause game over
     * @returns {boolean} True if game over condition is met
     */
    isGameOver() {
        if (!this.nextPair) {
            return false;
        }
        
        // Create a test pair at spawn position
        const testPair = this.nextPair.clone();
        testPair.setPosition(2, 1);
        
        const spawnPositions = testPair.getPuyoPositions();
        return !this.isValidMove(spawnPositions);
    }

    /**
     * Update puyo manager (called each frame)
     * @param {number} deltaTime - Time elapsed since last update in milliseconds
     * @returns {boolean} True if pair was fixed this update
     */
    update(deltaTime) {
        if (!this.currentPair) {
            return false;
        }

        // Initialize lastFallTime if not set
        if (!this.lastFallTime) {
            this.lastFallTime = Date.now();
        }

        // Calculate fall speed based on fast drop state
        const baseSpeed = this.currentPair.getFallSpeed(); // cells per second
        const effectiveSpeed = this.currentPair.getEffectiveFallSpeed();
        
        // Convert to milliseconds per cell
        const fallInterval = 1000 / effectiveSpeed;
        
        const currentTime = Date.now();
        
        if (currentTime - this.lastFallTime >= fallInterval) {
            if (this.canPairFall()) {
                this.fallPair();
                this.lastFallTime = currentTime;
            } else {
                // Pair can't fall anymore, fix it to field
                this.fixPairToField();
                this.lastFallTime = currentTime;
                return true; // Indicate that pair was fixed
            }
        }
        
        return false;
    }

    /**
     * Reset the puyo manager to initial state
     */
    reset() {
        this.currentPair = null;
        this.nextPair = null;
        
        // Initialize with first pairs
        this.nextPair = this.createRandomPuyoPair();
        this.spawnNextPair();
    }

    /**
     * Get manager state for debugging
     * @returns {Object} Manager state information
     */
    getState() {
        return {
            hasCurrentPair: this.hasCurrentPair(),
            currentPairPosition: this.currentPair ? this.currentPair.getPosition() : null,
            currentPairRotation: this.currentPair ? this.currentPair.getRotation() : null,
            currentPairColors: this.currentPair ? {
                puyo1: this.currentPair.puyo1.color,
                puyo2: this.currentPair.puyo2.color
            } : null,
            nextPairColors: this.nextPair ? {
                puyo1: this.nextPair.puyo1.color,
                puyo2: this.nextPair.puyo2.color
            } : null,
            canFall: this.canPairFall(),
            isGameOver: this.isGameOver()
        };
    }

    /**
     * Convert manager to JSON object
     * @returns {Object} JSON representation
     */
    toJSON() {
        return {
            currentPair: this.currentPair ? this.currentPair.toJSON() : null,
            nextPair: this.nextPair ? this.nextPair.toJSON() : null
        };
    }

    /**
     * Create manager from JSON object
     * @param {Object} json - JSON object with manager data
     * @param {Object} fieldManager - Field manager instance
     * @returns {PuyoManager} New manager instance
     */
    static fromJSON(json, fieldManager) {
        const manager = new PuyoManager(fieldManager);
        
        manager.currentPair = json.currentPair ? PuyoPair.fromJSON(json.currentPair) : null;
        manager.nextPair = json.nextPair ? PuyoPair.fromJSON(json.nextPair) : null;
        
        return manager;
    }
}