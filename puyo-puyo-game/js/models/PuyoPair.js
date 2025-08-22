/**
 * PuyoPair - Represents a pair of falling puyo pieces
 * Manages two puyo instances with rotation and movement logic
 */

import { Puyo } from './Puyo.js';

export class PuyoPair {
    // Rotation states (in degrees)
    static ROTATIONS = {
        DEGREE_0: 0,
        DEGREE_90: 1,
        DEGREE_180: 2,
        DEGREE_270: 3
    };

    // Default fall speed (cells per second)
    static DEFAULT_FALL_SPEED = 1;

    /**
     * Create a new PuyoPair instance
     * @param {Puyo} puyo1 - Primary puyo (pivot point for rotation)
     * @param {Puyo} puyo2 - Secondary puyo (rotates around primary)
     */
    constructor(puyo1, puyo2) {
        this.validatePuyos(puyo1, puyo2);
        
        this.puyo1 = puyo1; // Primary puyo (pivot)
        this.puyo2 = puyo2; // Secondary puyo (rotates around primary)
        this.rotation = PuyoPair.ROTATIONS.DEGREE_0; // 0°, 90°, 180°, 270°
        this.x = 2; // Center position (grid coordinates)
        this.y = 1; // Start at y=1 so that rotation 0 (secondary above) is at y=0
        this.fallSpeed = PuyoPair.DEFAULT_FALL_SPEED; // Cells per second
        this.fastDrop = false; // Accelerated falling state
        
        // Update puyo positions based on initial pair position
        this.updatePuyoPositions();
    }

    /**
     * Validate puyo parameters
     * @param {Puyo} puyo1 - First puyo to validate
     * @param {Puyo} puyo2 - Second puyo to validate
     * @throws {Error} If puyos are invalid
     */
    validatePuyos(puyo1, puyo2) {
        if (!(puyo1 instanceof Puyo)) {
            throw new Error('puyo1 must be an instance of Puyo');
        }
        
        if (!(puyo2 instanceof Puyo)) {
            throw new Error('puyo2 must be an instance of Puyo');
        }
    }

    /**
     * Set the pair's position
     * @param {number} x - New x coordinate
     * @param {number} y - New y coordinate
     */
    setPosition(x, y) {
        if (typeof x !== 'number' || typeof y !== 'number') {
            throw new Error('Position coordinates must be numbers');
        }
        
        if (!Number.isInteger(x) || !Number.isInteger(y)) {
            throw new Error('Position coordinates must be integers');
        }
        
        this.x = x;
        this.y = y;
        this.updatePuyoPositions();
    }

    /**
     * Get the pair's position
     * @returns {{x: number, y: number}} Position object
     */
    getPosition() {
        return { x: this.x, y: this.y };
    }

    /**
     * Rotate the pair clockwise (90 degrees)
     */
    rotate() {
        this.rotation = (this.rotation + 1) % 4;
        this.updatePuyoPositions();
    }

    /**
     * Rotate the pair counter-clockwise (270 degrees / -90 degrees)
     */
    rotateCounterClockwise() {
        this.rotation = (this.rotation + 3) % 4; // +3 is equivalent to -1 in mod 4
        this.updatePuyoPositions();
    }

    /**
     * Set specific rotation
     * @param {number} rotation - Rotation value (0, 1, 2, or 3)
     */
    setRotation(rotation) {
        if (!Number.isInteger(rotation) || rotation < 0 || rotation > 3) {
            throw new Error('Rotation must be an integer between 0 and 3');
        }
        
        this.rotation = rotation;
        this.updatePuyoPositions();
    }

    /**
     * Get current rotation
     * @returns {number} Current rotation (0, 1, 2, or 3)
     */
    getRotation() {
        return this.rotation;
    }

    /**
     * Get rotation in degrees
     * @returns {number} Rotation in degrees (0, 90, 180, or 270)
     */
    getRotationDegrees() {
        return this.rotation * 90;
    }

    /**
     * Update individual puyo positions based on pair position and rotation
     */
    updatePuyoPositions() {
        // Primary puyo is always at the pair's position
        this.puyo1.setPosition(this.x, this.y);
        
        // Secondary puyo position depends on rotation
        let secondaryX = this.x;
        let secondaryY = this.y;
        
        switch (this.rotation) {
            case PuyoPair.ROTATIONS.DEGREE_0: // 0° - secondary puyo above primary
                secondaryX = this.x;
                secondaryY = this.y - 1;
                break;
            case PuyoPair.ROTATIONS.DEGREE_90: // 90° - secondary puyo to the right
                secondaryX = this.x + 1;
                secondaryY = this.y;
                break;
            case PuyoPair.ROTATIONS.DEGREE_180: // 180° - secondary puyo below primary
                secondaryX = this.x;
                secondaryY = this.y + 1;
                break;
            case PuyoPair.ROTATIONS.DEGREE_270: // 270° - secondary puyo to the left
                secondaryX = this.x - 1;
                secondaryY = this.y;
                break;
        }
        
        this.puyo2.setPosition(secondaryX, secondaryY);
    }

    /**
     * Get positions of both puyos
     * @returns {{puyo1: {x: number, y: number}, puyo2: {x: number, y: number}}} Positions of both puyos
     */
    getPuyoPositions() {
        return {
            puyo1: this.puyo1.getPosition(),
            puyo2: this.puyo2.getPosition()
        };
    }

    /**
     * Move the pair in a direction
     * @param {string} direction - Direction to move ('left', 'right', 'down')
     */
    move(direction) {
        switch (direction) {
            case 'left':
                this.setPosition(this.x - 1, this.y);
                break;
            case 'right':
                this.setPosition(this.x + 1, this.y);
                break;
            case 'down':
                this.setPosition(this.x, this.y + 1);
                break;
            default:
                throw new Error(`Invalid direction: ${direction}. Valid directions are: left, right, down`);
        }
    }

    /**
     * Set fall speed
     * @param {number} speed - Fall speed in cells per second
     */
    setFallSpeed(speed) {
        if (typeof speed !== 'number' || speed <= 0) {
            throw new Error('Fall speed must be a positive number');
        }
        
        this.fallSpeed = speed;
    }

    /**
     * Get current fall speed
     * @returns {number} Current fall speed
     */
    getFallSpeed() {
        return this.fallSpeed;
    }

    /**
     * Enable fast drop mode
     */
    enableFastDrop() {
        this.fastDrop = true;
    }

    /**
     * Disable fast drop mode
     */
    disableFastDrop() {
        this.fastDrop = false;
    }

    /**
     * Check if fast drop is enabled
     * @returns {boolean} True if fast drop is enabled
     */
    isFastDropEnabled() {
        return this.fastDrop;
    }

    /**
     * Get effective fall speed (considering fast drop)
     * @param {number} fastDropMultiplier - Multiplier for fast drop speed (default: 10)
     * @returns {number} Effective fall speed
     */
    getEffectiveFallSpeed(fastDropMultiplier = 10) {
        return this.fastDrop ? this.fallSpeed * fastDropMultiplier : this.fallSpeed;
    }

    /**
     * Set both puyos to falling state
     */
    setFalling() {
        this.puyo1.setFalling();
        this.puyo2.setFalling();
    }

    /**
     * Set both puyos to fixed state
     */
    setFixed() {
        this.puyo1.setFixed();
        this.puyo2.setFixed();
    }

    /**
     * Check if both puyos are falling
     * @returns {boolean} True if both puyos are falling
     */
    isFalling() {
        return this.puyo1.isFalling() && this.puyo2.isFalling();
    }

    /**
     * Check if both puyos are fixed
     * @returns {boolean} True if both puyos are fixed
     */
    isFixed() {
        return this.puyo1.isFixed() && this.puyo2.isFixed();
    }

    /**
     * Get all positions that this pair would occupy after a potential move
     * @param {string} direction - Direction to check ('left', 'right', 'down')
     * @returns {{puyo1: {x: number, y: number}, puyo2: {x: number, y: number}}} Potential positions
     */
    getPotentialPositions(direction) {
        let newX = this.x;
        let newY = this.y;
        
        switch (direction) {
            case 'left':
                newX = this.x - 1;
                break;
            case 'right':
                newX = this.x + 1;
                break;
            case 'down':
                newY = this.y + 1;
                break;
            default:
                throw new Error(`Invalid direction: ${direction}`);
        }
        
        // Calculate positions for both puyos at the new location
        const tempPair = this.clone();
        tempPair.setPosition(newX, newY);
        
        return tempPair.getPuyoPositions();
    }

    /**
     * Get all positions that this pair would occupy after a potential rotation
     * @returns {{puyo1: {x: number, y: number}, puyo2: {x: number, y: number}}} Potential positions after rotation
     */
    getPotentialRotationPositions() {
        const tempPair = this.clone();
        tempPair.rotate();
        return tempPair.getPuyoPositions();
    }

    /**
     * Create a copy of this puyo pair
     * @returns {PuyoPair} New puyo pair instance with same properties
     */
    clone() {
        const clonedPuyo1 = this.puyo1.clone();
        const clonedPuyo2 = this.puyo2.clone();
        const clonedPair = new PuyoPair(clonedPuyo1, clonedPuyo2);
        
        clonedPair.setPosition(this.x, this.y);
        clonedPair.setRotation(this.rotation);
        clonedPair.setFallSpeed(this.fallSpeed);
        
        if (this.fastDrop) {
            clonedPair.enableFastDrop();
        }
        
        return clonedPair;
    }

    /**
     * Check if this pair is equal to another pair
     * @param {PuyoPair} other - Other pair to compare
     * @returns {boolean} True if equal
     */
    equals(other) {
        if (!(other instanceof PuyoPair)) {
            return false;
        }
        
        return this.puyo1.equals(other.puyo1) &&
               this.puyo2.equals(other.puyo2) &&
               this.x === other.x &&
               this.y === other.y &&
               this.rotation === other.rotation &&
               this.fallSpeed === other.fallSpeed &&
               this.fastDrop === other.fastDrop;
    }

    /**
     * Get string representation of the pair
     * @returns {string} String representation
     */
    toString() {
        return `PuyoPair(${this.puyo1.color}+${this.puyo2.color}, ${this.x}, ${this.y}, ${this.getRotationDegrees()}°)`;
    }

    /**
     * Convert pair to JSON object
     * @returns {Object} JSON representation
     */
    toJSON() {
        return {
            puyo1: this.puyo1.toJSON(),
            puyo2: this.puyo2.toJSON(),
            x: this.x,
            y: this.y,
            rotation: this.rotation,
            fallSpeed: this.fallSpeed,
            fastDrop: this.fastDrop
        };
    }

    /**
     * Create pair from JSON object
     * @param {Object} json - JSON object with pair data
     * @returns {PuyoPair} New pair instance
     */
    static fromJSON(json) {
        const puyo1 = Puyo.fromJSON(json.puyo1);
        const puyo2 = Puyo.fromJSON(json.puyo2);
        const pair = new PuyoPair(puyo1, puyo2);
        
        pair.setPosition(json.x, json.y);
        pair.setRotation(json.rotation);
        pair.setFallSpeed(json.fallSpeed);
        
        if (json.fastDrop) {
            pair.enableFastDrop();
        }
        
        return pair;
    }

    /**
     * Create a random puyo pair with random colors
     * @param {number} x - Initial x position (default: 2)
     * @param {number} y - Initial y position (default: 1)
     * @returns {PuyoPair} New random puyo pair
     */
    static createRandom(x = 2, y = 1) {
        const colors = Puyo.COLORS;
        const color1 = colors[Math.floor(Math.random() * colors.length)];
        const color2 = colors[Math.floor(Math.random() * colors.length)];
        
        const puyo1 = new Puyo(color1);
        const puyo2 = new Puyo(color2);
        const pair = new PuyoPair(puyo1, puyo2);
        
        pair.setPosition(x, y);
        return pair;
    }
}