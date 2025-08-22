/**
 * Puyo - Represents a single puyo piece with color, position, and state
 * Core data model for individual puyo pieces in the game
 */

export class Puyo {
    // Valid colors for puyo pieces
    static COLORS = ['red', 'blue', 'green', 'yellow', 'purple'];
    
    // Valid states for puyo pieces
    static STATES = {
        FALLING: 'falling',
        FIXED: 'fixed',
        CLEARING: 'clearing'
    };

    /**
     * Create a new Puyo instance
     * @param {string} color - The color of the puyo ('red', 'blue', 'green', 'yellow', 'purple')
     * @param {number} x - Grid x position (default: 0)
     * @param {number} y - Grid y position (default: 0)
     */
    constructor(color, x = 0, y = 0) {
        this.validateColor(color);
        this.validatePosition(x, y);
        
        this.color = color;
        this.x = x;
        this.y = y;
        this.state = Puyo.STATES.FALLING;
        this.animationFrame = 0;
    }

    /**
     * Validate color parameter
     * @param {string} color - Color to validate
     * @throws {Error} If color is invalid
     */
    validateColor(color) {
        if (!color || typeof color !== 'string') {
            throw new Error('Color must be a non-empty string');
        }
        
        if (!Puyo.COLORS.includes(color)) {
            throw new Error(`Invalid color: ${color}. Valid colors are: ${Puyo.COLORS.join(', ')}`);
        }
    }

    /**
     * Validate position parameters
     * @param {number} x - X coordinate to validate
     * @param {number} y - Y coordinate to validate
     * @throws {Error} If position is invalid
     */
    validatePosition(x, y) {
        if (typeof x !== 'number' || typeof y !== 'number') {
            throw new Error('Position coordinates must be numbers');
        }
        
        if (!Number.isInteger(x) || !Number.isInteger(y)) {
            throw new Error('Position coordinates must be integers');
        }
        
        if (x < 0 || y < 0) {
            throw new Error('Position coordinates must be non-negative');
        }
    }

    /**
     * Set the puyo's position
     * @param {number} x - New x coordinate
     * @param {number} y - New y coordinate
     */
    setPosition(x, y) {
        this.validatePosition(x, y);
        this.x = x;
        this.y = y;
    }

    /**
     * Get the puyo's position as an object
     * @returns {{x: number, y: number}} Position object
     */
    getPosition() {
        return { x: this.x, y: this.y };
    }

    /**
     * Set the puyo's state
     * @param {string} state - New state ('falling', 'fixed', 'clearing')
     */
    setState(state) {
        if (!Object.values(Puyo.STATES).includes(state)) {
            throw new Error(`Invalid state: ${state}. Valid states are: ${Object.values(Puyo.STATES).join(', ')}`);
        }
        this.state = state;
    }

    /**
     * Get the puyo's current state
     * @returns {string} Current state
     */
    getState() {
        return this.state;
    }

    /**
     * Check if puyo is in falling state
     * @returns {boolean} True if falling
     */
    isFalling() {
        return this.state === Puyo.STATES.FALLING;
    }

    /**
     * Check if puyo is in fixed state
     * @returns {boolean} True if fixed
     */
    isFixed() {
        return this.state === Puyo.STATES.FIXED;
    }

    /**
     * Check if puyo is in clearing state
     * @returns {boolean} True if clearing
     */
    isClearing() {
        return this.state === Puyo.STATES.CLEARING;
    }

    /**
     * Set puyo to falling state
     */
    setFalling() {
        this.setState(Puyo.STATES.FALLING);
    }

    /**
     * Set puyo to fixed state
     */
    setFixed() {
        this.setState(Puyo.STATES.FIXED);
    }

    /**
     * Set puyo to clearing state
     */
    setClearing() {
        this.setState(Puyo.STATES.CLEARING);
    }

    /**
     * Update animation frame for visual effects
     * @param {number} frame - Animation frame number
     */
    setAnimationFrame(frame) {
        if (typeof frame !== 'number' || frame < 0) {
            throw new Error('Animation frame must be a non-negative number');
        }
        this.animationFrame = frame;
    }

    /**
     * Get current animation frame
     * @returns {number} Current animation frame
     */
    getAnimationFrame() {
        return this.animationFrame;
    }

    /**
     * Create a copy of this puyo
     * @returns {Puyo} New puyo instance with same properties
     */
    clone() {
        const cloned = new Puyo(this.color, this.x, this.y);
        cloned.setState(this.state);
        cloned.setAnimationFrame(this.animationFrame);
        return cloned;
    }

    /**
     * Check if this puyo is equal to another puyo
     * @param {Puyo} other - Other puyo to compare
     * @returns {boolean} True if equal
     */
    equals(other) {
        if (!(other instanceof Puyo)) {
            return false;
        }
        
        return this.color === other.color &&
               this.x === other.x &&
               this.y === other.y &&
               this.state === other.state;
    }

    /**
     * Get string representation of the puyo
     * @returns {string} String representation
     */
    toString() {
        return `Puyo(${this.color}, ${this.x}, ${this.y}, ${this.state})`;
    }

    /**
     * Convert puyo to JSON object
     * @returns {Object} JSON representation
     */
    toJSON() {
        return {
            color: this.color,
            x: this.x,
            y: this.y,
            state: this.state,
            animationFrame: this.animationFrame
        };
    }

    /**
     * Create puyo from JSON object
     * @param {Object} json - JSON object with puyo data
     * @returns {Puyo} New puyo instance
     */
    static fromJSON(json) {
        const puyo = new Puyo(json.color, json.x, json.y);
        puyo.setState(json.state);
        puyo.setAnimationFrame(json.animationFrame || 0);
        return puyo;
    }
}