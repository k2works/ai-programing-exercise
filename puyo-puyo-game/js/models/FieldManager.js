/**
 * FieldManager - Manages the 12x6 game field grid
 * Handles puyo placement, collision detection, gravity, and connected group detection
 */

import { Puyo } from './Puyo.js';

export class FieldManager {
    // Field dimensions (height x width)
    static FIELD_HEIGHT = 12;
    static FIELD_WIDTH = 6;

    /**
     * Create a new FieldManager instance
     * @param {number} width - Field width (default: 6)
     * @param {number} height - Field height (default: 12)
     */
    constructor(width = FieldManager.FIELD_WIDTH, height = FieldManager.FIELD_HEIGHT) {
        this.width = width;
        this.height = height;
        this.field = this.initializeField();
    }

    /**
     * Initialize empty field as 2D array
     * @returns {Array<Array<Puyo|null>>} Empty field grid
     */
    initializeField() {
        const field = [];
        for (let y = 0; y < this.height; y++) {
            field[y] = [];
            for (let x = 0; x < this.width; x++) {
                field[y][x] = null;
            }
        }
        return field;
    }

    /**
     * Get puyo at specified position
     * @param {number} x - X coordinate (0-based)
     * @param {number} y - Y coordinate (0-based)
     * @returns {Puyo|null} Puyo at position or null if empty
     */
    getCell(x, y) {
        if (!this.isValidPosition(x, y)) {
            return null;
        }
        return this.field[y][x];
    }

    /**
     * Set puyo at specified position
     * @param {number} x - X coordinate (0-based)
     * @param {number} y - Y coordinate (0-based)
     * @param {Puyo|null} puyo - Puyo to place or null to clear
     */
    setCell(x, y, puyo) {
        if (!this.isValidPosition(x, y)) {
            throw new Error(`Invalid position: (${x}, ${y}). Field bounds: ${this.width}x${this.height}`);
        }
        
        if (puyo !== null && !(puyo instanceof Puyo)) {
            throw new Error('Cell value must be a Puyo instance or null');
        }
        
        this.field[y][x] = puyo;
        
        // Update puyo position if it's not null
        if (puyo) {
            puyo.setPosition(x, y);
        }
    }

    /**
     * Check if position is within field bounds
     * @param {number} x - X coordinate
     * @param {number} y - Y coordinate
     * @returns {boolean} True if position is valid
     */
    isValidPosition(x, y) {
        return x >= 0 && x < this.width && y >= 0 && y < this.height;
    }

    /**
     * Clear cell at specified position
     * @param {number} x - X coordinate
     * @param {number} y - Y coordinate
     */
    clearCell(x, y) {
        this.setCell(x, y, null);
    }

    /**
     * Check if position is occupied by a puyo
     * @param {number} x - X coordinate
     * @param {number} y - Y coordinate
     * @returns {boolean} True if position is occupied
     */
    isOccupied(x, y) {
        return this.getCell(x, y) !== null;
    }

    /**
     * Check if position is empty
     * @param {number} x - X coordinate
     * @param {number} y - Y coordinate
     * @returns {boolean} True if position is empty
     */
    isEmpty(x, y) {
        return this.getCell(x, y) === null;
    }

    /**
     * Check for collision at specified position
     * @param {number} x - X coordinate
     * @param {number} y - Y coordinate
     * @returns {boolean} True if collision detected (out of bounds or occupied)
     */
    hasCollision(x, y) {
        return !this.isValidPosition(x, y) || this.isOccupied(x, y);
    }

    /**
     * Check if game is over (top row has fixed puyo)
     * @returns {boolean} True if game over condition met
     */
    isGameOver() {
        // Check top row for any fixed puyo
        for (let x = 0; x < this.width; x++) {
            const puyo = this.getCell(x, 0);
            if (puyo && puyo.isFixed()) {
                return true;
            }
        }
        return false;
    }

    /**
     * Get all puyo in the field
     * @returns {Array<{puyo: Puyo, x: number, y: number}>} Array of puyo with positions
     */
    getAllPuyo() {
        const puyo = [];
        for (let y = 0; y < this.height; y++) {
            for (let x = 0; x < this.width; x++) {
                const cell = this.getCell(x, y);
                if (cell) {
                    puyo.push({ puyo: cell, x, y });
                }
            }
        }
        return puyo;
    }

    /**
     * Get field as 2D array (for debugging/testing)
     * @returns {Array<Array<Puyo|null>>} Field grid
     */
    getField() {
        return this.field;
    }

    /**
     * Clear entire field
     */
    clear() {
        this.field = this.initializeField();
    }

    /**
     * Get field dimensions
     * @returns {{width: number, height: number}} Field dimensions
     */
    getDimensions() {
        return { width: this.width, height: this.height };
    }

    /**
     * Create a copy of the field
     * @returns {FieldManager} New FieldManager instance with copied state
     */
    clone() {
        const cloned = new FieldManager(this.width, this.height);
        for (let y = 0; y < this.height; y++) {
            for (let x = 0; x < this.width; x++) {
                const puyo = this.getCell(x, y);
                if (puyo) {
                    cloned.setCell(x, y, puyo.clone());
                }
            }
        }
        return cloned;
    }

    /**
     * Apply gravity to all puyo in the field
     * Makes puyo fall down to fill empty spaces below them
     * @returns {boolean} True if any puyo moved, false if no changes
     */
    applyGravity() {
        let anyMoved = false;
        
        // Process from bottom to top to avoid moving the same puyo multiple times
        for (let x = 0; x < this.width; x++) {
            // Collect all non-null puyo in this column
            const columnPuyo = [];
            for (let y = this.height - 1; y >= 0; y--) {
                const puyo = this.getCell(x, y);
                if (puyo) {
                    columnPuyo.push(puyo);
                    this.clearCell(x, y);
                }
            }
            
            // Place puyo at the bottom of the column
            for (let i = 0; i < columnPuyo.length; i++) {
                const newY = this.height - 1 - i;
                const puyo = columnPuyo[i];
                
                // Check if puyo moved
                if (puyo.y !== newY) {
                    anyMoved = true;
                }
                
                this.setCell(x, newY, puyo);
            }
        }
        
        return anyMoved;
    }

    /**
     * Apply gravity repeatedly until no more movement occurs
     * Handles cascading falls when multiple puyo need to drop
     * @returns {number} Number of gravity iterations performed
     */
    applyCascadingGravity() {
        let iterations = 0;
        let maxIterations = this.height; // Prevent infinite loops
        
        while (iterations < maxIterations) {
            const moved = this.applyGravity();
            iterations++;
            
            if (!moved) {
                break;
            }
        }
        
        return iterations;
    }

    /**
     * Check if any puyo can fall further
     * @returns {boolean} True if gravity would cause movement
     */
    needsGravity() {
        for (let x = 0; x < this.width; x++) {
            for (let y = this.height - 2; y >= 0; y--) { // Start from second-to-bottom row
                const puyo = this.getCell(x, y);
                if (puyo && this.isEmpty(x, y + 1)) {
                    return true;
                }
            }
        }
        return false;
    }

    /**
     * Get the lowest empty position in a column
     * @param {number} x - Column to check
     * @returns {number} Y coordinate of lowest empty position, or -1 if column is full
     */
    getLowestEmptyPosition(x) {
        if (!this.isValidPosition(x, 0)) {
            return -1;
        }
        
        for (let y = this.height - 1; y >= 0; y--) {
            if (this.isEmpty(x, y)) {
                return y;
            }
        }
        
        return -1; // Column is full
    }

    /**
     * Find all connected groups of puyo with 4 or more pieces
     * Uses flood-fill algorithm to detect horizontal/vertical connections
     * @returns {Array<Array<{x: number, y: number, puyo: Puyo}>>} Array of groups, each group is an array of positions
     */
    findConnectedGroups() {
        const visited = new Set();
        const groups = [];
        
        for (let y = 0; y < this.height; y++) {
            for (let x = 0; x < this.width; x++) {
                const puyo = this.getCell(x, y);
                const key = `${x},${y}`;
                
                // Skip empty cells, already visited cells, or clearing puyo
                if (!puyo || visited.has(key) || puyo.isClearing()) {
                    continue;
                }
                
                // Find connected group starting from this position
                const group = this.floodFill(x, y, puyo.color, visited);
                
                // Only include groups with 4 or more puyo
                if (group.length >= 4) {
                    groups.push(group);
                }
            }
        }
        
        return groups;
    }

    /**
     * Flood-fill algorithm to find all connected puyo of the same color
     * @param {number} startX - Starting X coordinate
     * @param {number} startY - Starting Y coordinate
     * @param {string} color - Color to match
     * @param {Set<string>} visited - Set of already visited positions
     * @returns {Array<{x: number, y: number, puyo: Puyo}>} Array of connected positions
     */
    floodFill(startX, startY, color, visited) {
        const group = [];
        const stack = [{ x: startX, y: startY }];
        
        while (stack.length > 0) {
            const { x, y } = stack.pop();
            const key = `${x},${y}`;
            
            // Skip if already visited or out of bounds
            if (visited.has(key) || !this.isValidPosition(x, y)) {
                continue;
            }
            
            const puyo = this.getCell(x, y);
            
            // Skip if no puyo, wrong color, or clearing state
            if (!puyo || puyo.color !== color || puyo.isClearing()) {
                continue;
            }
            
            // Mark as visited and add to group
            visited.add(key);
            group.push({ x, y, puyo });
            
            // Add adjacent cells to stack (4-directional connectivity)
            stack.push(
                { x: x - 1, y }, // Left
                { x: x + 1, y }, // Right
                { x, y: y - 1 }, // Up
                { x, y: y + 1 }  // Down
            );
        }
        
        return group;
    }

    /**
     * Clear specified groups of puyo from the field
     * @param {Array<Array<{x: number, y: number, puyo: Puyo}>>} groups - Groups to clear
     * @returns {number} Total number of puyo cleared
     */
    clearGroups(groups) {
        let totalCleared = 0;
        
        for (const group of groups) {
            for (const { x, y, puyo } of group) {
                // Set puyo to clearing state first (for animation)
                puyo.setClearing();
                
                // Remove from field
                this.clearCell(x, y);
                totalCleared++;
            }
        }
        
        return totalCleared;
    }

    /**
     * Find and clear all connected groups, then apply gravity
     * @returns {{groups: Array, clearedCount: number, gravityApplied: boolean}} Results of the clear operation
     */
    findAndClearGroups() {
        const groups = this.findConnectedGroups();
        const clearedCount = this.clearGroups(groups);
        const gravityApplied = clearedCount > 0 ? this.applyGravity() : false;
        
        return {
            groups,
            clearedCount,
            gravityApplied
        };
    }

    /**
     * Check if there are any clearable groups (4+ connected puyo)
     * @returns {boolean} True if there are groups that can be cleared
     */
    hasClearableGroups() {
        const groups = this.findConnectedGroups();
        return groups.length > 0;
    }

    /**
     * Get all puyo of a specific color
     * @param {string} color - Color to search for
     * @returns {Array<{x: number, y: number, puyo: Puyo}>} Array of positions with matching color
     */
    getPuyoByColor(color) {
        const matches = [];
        
        for (let y = 0; y < this.height; y++) {
            for (let x = 0; x < this.width; x++) {
                const puyo = this.getCell(x, y);
                if (puyo && puyo.color === color) {
                    matches.push({ x, y, puyo });
                }
            }
        }
        
        return matches;
    }

    /**
     * Count connected puyo starting from a position (without clearing)
     * @param {number} startX - Starting X coordinate
     * @param {number} startY - Starting Y coordinate
     * @returns {number} Number of connected puyo of the same color
     */
    countConnectedPuyo(startX, startY) {
        const puyo = this.getCell(startX, startY);
        if (!puyo) {
            return 0;
        }
        
        const visited = new Set();
        const group = this.floodFill(startX, startY, puyo.color, visited);
        return group.length;
    }

    /**
     * Convert field to string representation (for debugging)
     * @returns {string} String representation of field
     */
    toString() {
        let result = '';
        for (let y = 0; y < this.height; y++) {
            for (let x = 0; x < this.width; x++) {
                const puyo = this.getCell(x, y);
                if (puyo) {
                    result += puyo.color.charAt(0).toUpperCase();
                } else {
                    result += '.';
                }
            }
            result += '\n';
        }
        return result;
    }
}