/**
 * Unit tests for FieldManager class
 * Tests field operations, collision detection, and boundary checking
 */

import { FieldManager } from '../../models/FieldManager.js';
import { Puyo } from '../../models/Puyo.js';

// Simple test framework for browser environment
class TestRunner {
    constructor() {
        this.tests = [];
        this.results = [];
    }

    test(name, testFn) {
        this.tests.push({ name, testFn });
    }

    async run() {
        console.log('Running FieldManager tests...');
        
        for (const { name, testFn } of this.tests) {
            try {
                await testFn();
                this.results.push({ name, status: 'PASS', error: null });
                console.log(`✓ ${name}`);
            } catch (error) {
                this.results.push({ name, status: 'FAIL', error: error.message });
                console.error(`✗ ${name}: ${error.message}`);
            }
        }
        
        this.printSummary();
        return this.results;
    }

    printSummary() {
        const passed = this.results.filter(r => r.status === 'PASS').length;
        const failed = this.results.filter(r => r.status === 'FAIL').length;
        
        console.log(`\nTest Summary: ${passed} passed, ${failed} failed`);
        
        if (failed > 0) {
            console.log('\nFailed tests:');
            this.results.filter(r => r.status === 'FAIL').forEach(r => {
                console.log(`  - ${r.name}: ${r.error}`);
            });
        }
    }
}

// Helper functions for assertions
function expect(actual) {
    return {
        toBe: (expected) => {
            if (actual !== expected) {
                throw new Error(`Expected ${expected}, but got ${actual}`);
            }
        },
        toBeNull: () => {
            if (actual !== null) {
                throw new Error(`Expected null, but got ${actual}`);
            }
        },
        toHaveLength: (expected) => {
            if (!actual || actual.length !== expected) {
                throw new Error(`Expected length ${expected}, but got ${actual ? actual.length : 'undefined'}`);
            }
        },
        toContainEqual: (expected) => {
            if (!actual || !Array.isArray(actual)) {
                throw new Error('Expected an array');
            }
            const found = actual.some(item => 
                JSON.stringify(item) === JSON.stringify(expected)
            );
            if (!found) {
                throw new Error(`Expected array to contain ${JSON.stringify(expected)}`);
            }
        },
        not: {
            toBe: (expected) => {
                if (actual === expected) {
                    throw new Error(`Expected not to be ${expected}, but got ${actual}`);
                }
            },
            toBeNull: () => {
                if (actual === null) {
                    throw new Error('Expected not to be null');
                }
            }
        }
    };
}

function toThrow(fn, expectedMessage) {
    try {
        fn();
        throw new Error('Expected function to throw an error');
    } catch (error) {
        if (expectedMessage && !error.message.includes(expectedMessage)) {
            throw new Error(`Expected error message to contain "${expectedMessage}", but got "${error.message}"`);
        }
    }
}

// Test suite for FieldManager
const runner = new TestRunner();

// Constructor and Initialization tests
runner.test('should create field with default dimensions', () => {
    const fieldManager = new FieldManager();
    expect(fieldManager.width).toBe(6);
    expect(fieldManager.height).toBe(12);
});

runner.test('should create field with custom dimensions', () => {
    const customField = new FieldManager(8, 10);
    expect(customField.width).toBe(8);
    expect(customField.height).toBe(10);
});

runner.test('should initialize empty field', () => {
    const fieldManager = new FieldManager();
    for (let y = 0; y < fieldManager.height; y++) {
        for (let x = 0; x < fieldManager.width; x++) {
            expect(fieldManager.getCell(x, y)).toBeNull();
        }
    }
});

// Position Validation tests
runner.test('should validate positions within bounds', () => {
    const fieldManager = new FieldManager();
    expect(fieldManager.isValidPosition(0, 0)).toBe(true);
    expect(fieldManager.isValidPosition(5, 11)).toBe(true);
    expect(fieldManager.isValidPosition(2, 5)).toBe(true);
});

runner.test('should invalidate positions outside bounds', () => {
    const fieldManager = new FieldManager();
    expect(fieldManager.isValidPosition(-1, 0)).toBe(false);
    expect(fieldManager.isValidPosition(0, -1)).toBe(false);
    expect(fieldManager.isValidPosition(6, 0)).toBe(false);
    expect(fieldManager.isValidPosition(0, 12)).toBe(false);
    expect(fieldManager.isValidPosition(10, 15)).toBe(false);
});

// Cell Operations tests
runner.test('should get and set cells correctly', () => {
    const fieldManager = new FieldManager();
    const puyo = new Puyo('red', 2, 3);
    fieldManager.setCell(2, 3, puyo);
    
    const retrieved = fieldManager.getCell(2, 3);
    expect(retrieved).toBe(puyo);
    expect(retrieved.x).toBe(2);
    expect(retrieved.y).toBe(3);
});

runner.test('should return null for empty cells', () => {
    const fieldManager = new FieldManager();
    expect(fieldManager.getCell(1, 1)).toBeNull();
});

runner.test('should return null for invalid positions', () => {
    const fieldManager = new FieldManager();
    expect(fieldManager.getCell(-1, 0)).toBeNull();
    expect(fieldManager.getCell(10, 15)).toBeNull();
});

runner.test('should throw error when setting cell at invalid position', () => {
    const fieldManager = new FieldManager();
    const puyo = new Puyo('blue');
    toThrow(() => {
        fieldManager.setCell(-1, 0, puyo);
    }, 'Invalid position');
    
    toThrow(() => {
        fieldManager.setCell(6, 0, puyo);
    }, 'Invalid position');
});

runner.test('should throw error when setting non-puyo value', () => {
    const fieldManager = new FieldManager();
    toThrow(() => {
        fieldManager.setCell(0, 0, 'invalid');
    }, 'Cell value must be a Puyo instance or null');
});

runner.test('should update puyo position when setting cell', () => {
    const fieldManager = new FieldManager();
    const puyo = new Puyo('green', 0, 0);
    fieldManager.setCell(3, 5, puyo);
    
    expect(puyo.x).toBe(3);
    expect(puyo.y).toBe(5);
});

runner.test('should clear cells correctly', () => {
    const fieldManager = new FieldManager();
    const puyo = new Puyo('yellow', 1, 2);
    fieldManager.setCell(1, 2, puyo);
    expect(fieldManager.getCell(1, 2)).toBe(puyo);
    
    fieldManager.clearCell(1, 2);
    expect(fieldManager.getCell(1, 2)).toBeNull();
});

// Occupancy Checks tests
runner.test('should detect occupied cells', () => {
    const fieldManager = new FieldManager();
    const puyo = new Puyo('purple', 2, 4);
    fieldManager.setCell(2, 4, puyo);
    
    expect(fieldManager.isOccupied(2, 4)).toBe(true);
    expect(fieldManager.isEmpty(2, 4)).toBe(false);
});

runner.test('should detect empty cells', () => {
    const fieldManager = new FieldManager();
    expect(fieldManager.isOccupied(1, 1)).toBe(false);
    expect(fieldManager.isEmpty(1, 1)).toBe(true);
});

runner.test('should handle invalid positions in occupancy checks', () => {
    const fieldManager = new FieldManager();
    expect(fieldManager.isOccupied(-1, 0)).toBe(false);
    expect(fieldManager.isEmpty(-1, 0)).toBe(false);
});

// Collision Detection tests
runner.test('should detect collision with occupied cells', () => {
    const fieldManager = new FieldManager();
    const puyo = new Puyo('red', 3, 6);
    fieldManager.setCell(3, 6, puyo);
    
    expect(fieldManager.hasCollision(3, 6)).toBe(true);
});

runner.test('should detect collision with field boundaries', () => {
    const fieldManager = new FieldManager();
    expect(fieldManager.hasCollision(-1, 0)).toBe(true);
    expect(fieldManager.hasCollision(0, -1)).toBe(true);
    expect(fieldManager.hasCollision(6, 0)).toBe(true);
    expect(fieldManager.hasCollision(0, 12)).toBe(true);
});

runner.test('should not detect collision in empty valid positions', () => {
    const fieldManager = new FieldManager();
    expect(fieldManager.hasCollision(2, 5)).toBe(false);
    expect(fieldManager.hasCollision(0, 0)).toBe(false);
    expect(fieldManager.hasCollision(5, 11)).toBe(false);
});

// Game Over Detection tests
runner.test('should detect game over when top row has fixed puyo', () => {
    const fieldManager = new FieldManager();
    const puyo = new Puyo('blue', 2, 0);
    puyo.setFixed();
    fieldManager.setCell(2, 0, puyo);
    
    expect(fieldManager.isGameOver()).toBe(true);
});

runner.test('should not detect game over with falling puyo in top row', () => {
    const fieldManager = new FieldManager();
    const puyo = new Puyo('green', 1, 0);
    puyo.setFalling();
    fieldManager.setCell(1, 0, puyo);
    
    expect(fieldManager.isGameOver()).toBe(false);
});

runner.test('should not detect game over with empty top row', () => {
    const fieldManager = new FieldManager();
    // Place some puyo in lower rows
    const puyo1 = new Puyo('red', 2, 5);
    const puyo2 = new Puyo('blue', 3, 8);
    puyo1.setFixed();
    puyo2.setFixed();
    fieldManager.setCell(2, 5, puyo1);
    fieldManager.setCell(3, 8, puyo2);
    
    expect(fieldManager.isGameOver()).toBe(false);
});

runner.test('should detect game over with multiple fixed puyo in top row', () => {
    const fieldManager = new FieldManager();
    const puyo1 = new Puyo('red', 0, 0);
    const puyo2 = new Puyo('blue', 4, 0);
    puyo1.setFixed();
    puyo2.setFixed();
    fieldManager.setCell(0, 0, puyo1);
    fieldManager.setCell(4, 0, puyo2);
    
    expect(fieldManager.isGameOver()).toBe(true);
});

// Field Utilities tests
runner.test('should get all puyo in field', () => {
    const fieldManager = new FieldManager();
    const puyo1 = new Puyo('red', 1, 2);
    const puyo2 = new Puyo('blue', 3, 5);
    const puyo3 = new Puyo('green', 0, 8);
    
    fieldManager.setCell(1, 2, puyo1);
    fieldManager.setCell(3, 5, puyo2);
    fieldManager.setCell(0, 8, puyo3);
    
    const allPuyo = fieldManager.getAllPuyo();
    expect(allPuyo).toHaveLength(3);
    
    expect(allPuyo).toContainEqual({ puyo: puyo1, x: 1, y: 2 });
    expect(allPuyo).toContainEqual({ puyo: puyo2, x: 3, y: 5 });
    expect(allPuyo).toContainEqual({ puyo: puyo3, x: 0, y: 8 });
});

runner.test('should return empty array when no puyo in field', () => {
    const fieldManager = new FieldManager();
    const allPuyo = fieldManager.getAllPuyo();
    expect(allPuyo).toHaveLength(0);
});

runner.test('should get field dimensions', () => {
    const fieldManager = new FieldManager();
    const dimensions = fieldManager.getDimensions();
    expect(dimensions.width).toBe(6);
    expect(dimensions.height).toBe(12);
});

runner.test('should clear entire field', () => {
    const fieldManager = new FieldManager();
    // Add some puyo
    fieldManager.setCell(1, 1, new Puyo('red'));
    fieldManager.setCell(2, 3, new Puyo('blue'));
    fieldManager.setCell(4, 7, new Puyo('green'));
    
    expect(fieldManager.getAllPuyo()).toHaveLength(3);
    
    fieldManager.clear();
    expect(fieldManager.getAllPuyo()).toHaveLength(0);
    
    // Verify all cells are null
    for (let y = 0; y < fieldManager.height; y++) {
        for (let x = 0; x < fieldManager.width; x++) {
            expect(fieldManager.getCell(x, y)).toBeNull();
        }
    }
});

// Field Cloning tests
runner.test('should create independent copy of field', () => {
    const fieldManager = new FieldManager();
    const puyo1 = new Puyo('red', 1, 2);
    const puyo2 = new Puyo('blue', 3, 4);
    fieldManager.setCell(1, 2, puyo1);
    fieldManager.setCell(3, 4, puyo2);
    
    const cloned = fieldManager.clone();
    
    // Verify dimensions match
    expect(cloned.width).toBe(fieldManager.width);
    expect(cloned.height).toBe(fieldManager.height);
    
    // Verify puyo are copied
    const clonedPuyo1 = cloned.getCell(1, 2);
    const clonedPuyo2 = cloned.getCell(3, 4);
    
    expect(clonedPuyo1).not.toBe(puyo1); // Different instances
    expect(clonedPuyo2).not.toBe(puyo2);
    
    expect(clonedPuyo1.color).toBe(puyo1.color);
    expect(clonedPuyo1.x).toBe(puyo1.x);
    expect(clonedPuyo1.y).toBe(puyo1.y);
    
    // Verify independence
    fieldManager.clearCell(1, 2);
    expect(cloned.getCell(1, 2)).not.toBeNull();
});

// String Representation tests
runner.test('should generate string representation of field', () => {
    const fieldManager = new FieldManager();
    fieldManager.setCell(0, 0, new Puyo('red'));
    fieldManager.setCell(1, 0, new Puyo('blue'));
    fieldManager.setCell(2, 1, new Puyo('green'));
    
    const str = fieldManager.toString();
    const lines = str.split('\n');
    
    expect(lines[0]).toBe('RB....');
    expect(lines[1]).toBe('..G...');
    expect(lines.length).toBe(fieldManager.height + 1); // +1 for final newline
});

runner.test('should show empty field correctly', () => {
    const fieldManager = new FieldManager();
    const str = fieldManager.toString();
    const lines = str.split('\n').filter(line => line.length > 0);
    
    expect(lines).toHaveLength(fieldManager.height);
    lines.forEach(line => {
        expect(line).toBe('......');
    });
});

// Gravity System tests
runner.test('should apply gravity to single puyo', () => {
    const fieldManager = new FieldManager();
    const puyo = new Puyo('red', 2, 5);
    fieldManager.setCell(2, 5, puyo);
    
    const moved = fieldManager.applyGravity();
    expect(moved).toBe(true);
    
    // Puyo should have fallen to the bottom
    expect(fieldManager.getCell(2, 5)).toBeNull();
    expect(fieldManager.getCell(2, 11)).toBe(puyo);
    expect(puyo.y).toBe(11);
});

runner.test('should apply gravity to multiple puyo in same column', () => {
    const fieldManager = new FieldManager();
    const puyo1 = new Puyo('red', 1, 3);
    const puyo2 = new Puyo('blue', 1, 7);
    fieldManager.setCell(1, 3, puyo1);
    fieldManager.setCell(1, 7, puyo2);
    
    fieldManager.applyGravity();
    
    // Both puyo should stack at bottom
    expect(fieldManager.getCell(1, 10)).toBe(puyo1); // Top of stack
    expect(fieldManager.getCell(1, 11)).toBe(puyo2); // Bottom of stack
    expect(puyo1.y).toBe(10);
    expect(puyo2.y).toBe(11);
});

runner.test('should not move puyo that are already at bottom', () => {
    const fieldManager = new FieldManager();
    const puyo = new Puyo('green', 3, 11);
    fieldManager.setCell(3, 11, puyo);
    
    const moved = fieldManager.applyGravity();
    expect(moved).toBe(false);
    
    expect(fieldManager.getCell(3, 11)).toBe(puyo);
    expect(puyo.y).toBe(11);
});

runner.test('should handle cascading gravity correctly', () => {
    const fieldManager = new FieldManager();
    const puyo1 = new Puyo('red', 2, 2);
    const puyo2 = new Puyo('blue', 2, 4);
    const puyo3 = new Puyo('green', 2, 8);
    
    fieldManager.setCell(2, 2, puyo1);
    fieldManager.setCell(2, 4, puyo2);
    fieldManager.setCell(2, 8, puyo3);
    
    const iterations = fieldManager.applyCascadingGravity();
    expect(iterations).toBe(1); // Should complete in one iteration
    
    // All puyo should stack at bottom
    expect(fieldManager.getCell(2, 9)).toBe(puyo1);
    expect(fieldManager.getCell(2, 10)).toBe(puyo2);
    expect(fieldManager.getCell(2, 11)).toBe(puyo3);
});

runner.test('should detect when gravity is needed', () => {
    const fieldManager = new FieldManager();
    
    // Empty field doesn't need gravity
    expect(fieldManager.needsGravity()).toBe(false);
    
    // Puyo at bottom doesn't need gravity
    fieldManager.setCell(1, 11, new Puyo('red'));
    expect(fieldManager.needsGravity()).toBe(false);
    
    // Floating puyo needs gravity
    fieldManager.setCell(1, 5, new Puyo('blue'));
    expect(fieldManager.needsGravity()).toBe(true);
});

runner.test('should find lowest empty position in column', () => {
    const fieldManager = new FieldManager();
    
    // Empty column
    expect(fieldManager.getLowestEmptyPosition(0)).toBe(11);
    
    // Column with one puyo at bottom
    fieldManager.setCell(0, 11, new Puyo('red'));
    expect(fieldManager.getLowestEmptyPosition(0)).toBe(10);
    
    // Column with multiple puyo
    fieldManager.setCell(0, 10, new Puyo('blue'));
    fieldManager.setCell(0, 9, new Puyo('green'));
    expect(fieldManager.getLowestEmptyPosition(0)).toBe(8);
    
    // Full column
    for (let y = 0; y < fieldManager.height; y++) {
        fieldManager.setCell(1, y, new Puyo('yellow'));
    }
    expect(fieldManager.getLowestEmptyPosition(1)).toBe(-1);
    
    // Invalid column
    expect(fieldManager.getLowestEmptyPosition(-1)).toBe(-1);
    expect(fieldManager.getLowestEmptyPosition(6)).toBe(-1);
});

runner.test('should handle gravity with gaps in middle', () => {
    const fieldManager = new FieldManager();
    
    // Create a scenario with gaps
    fieldManager.setCell(2, 11, new Puyo('red'));   // Bottom
    fieldManager.setCell(2, 9, new Puyo('blue'));   // Gap at y=10
    fieldManager.setCell(2, 7, new Puyo('green'));  // Gap at y=8
    fieldManager.setCell(2, 3, new Puyo('yellow')); // Gap at y=4,5,6
    
    fieldManager.applyGravity();
    
    // All puyo should stack at bottom with no gaps
    expect(fieldManager.getCell(2, 8)).toBe(new Puyo('yellow'));
    expect(fieldManager.getCell(2, 9)).toBe(new Puyo('green'));
    expect(fieldManager.getCell(2, 10)).toBe(new Puyo('blue'));
    expect(fieldManager.getCell(2, 11)).toBe(new Puyo('red'));
    
    // Check that gaps are filled
    expect(fieldManager.getCell(2, 7)).toBeNull();
    expect(fieldManager.getCell(2, 6)).toBeNull();
    expect(fieldManager.getCell(2, 5)).toBeNull();
});

runner.test('should preserve puyo properties during gravity', () => {
    const fieldManager = new FieldManager();
    const puyo = new Puyo('purple', 1, 3);
    puyo.setFixed();
    puyo.setAnimationFrame(5);
    
    fieldManager.setCell(1, 3, puyo);
    fieldManager.applyGravity();
    
    const movedPuyo = fieldManager.getCell(1, 11);
    expect(movedPuyo.color).toBe('purple');
    expect(movedPuyo.getState()).toBe('fixed');
    expect(movedPuyo.getAnimationFrame()).toBe(5);
});

// Connected Group Detection tests
runner.test('should find horizontal connected group of 4', () => {
    const fieldManager = new FieldManager();
    
    // Create horizontal line of 4 red puyo
    for (let x = 1; x < 5; x++) {
        fieldManager.setCell(x, 10, new Puyo('red'));
    }
    
    const groups = fieldManager.findConnectedGroups();
    expect(groups).toHaveLength(1);
    expect(groups[0]).toHaveLength(4);
});

runner.test('should find vertical connected group of 4', () => {
    const fieldManager = new FieldManager();
    
    // Create vertical line of 4 blue puyo
    for (let y = 8; y < 12; y++) {
        fieldManager.setCell(2, y, new Puyo('blue'));
    }
    
    const groups = fieldManager.findConnectedGroups();
    expect(groups).toHaveLength(1);
    expect(groups[0]).toHaveLength(4);
});

runner.test('should find L-shaped connected group', () => {
    const fieldManager = new FieldManager();
    
    // Create L-shaped group of green puyo
    fieldManager.setCell(1, 10, new Puyo('green'));
    fieldManager.setCell(1, 11, new Puyo('green'));
    fieldManager.setCell(2, 11, new Puyo('green'));
    fieldManager.setCell(3, 11, new Puyo('green'));
    
    const groups = fieldManager.findConnectedGroups();
    expect(groups).toHaveLength(1);
    expect(groups[0]).toHaveLength(4);
});

runner.test('should not find groups with less than 4 puyo', () => {
    const fieldManager = new FieldManager();
    
    // Create group of only 3 puyo
    fieldManager.setCell(0, 10, new Puyo('yellow'));
    fieldManager.setCell(0, 11, new Puyo('yellow'));
    fieldManager.setCell(1, 11, new Puyo('yellow'));
    
    const groups = fieldManager.findConnectedGroups();
    expect(groups).toHaveLength(0);
});

runner.test('should find multiple separate groups', () => {
    const fieldManager = new FieldManager();
    
    // Create first group (horizontal red)
    for (let x = 0; x < 4; x++) {
        fieldManager.setCell(x, 11, new Puyo('red'));
    }
    
    // Create second group (vertical blue)
    for (let y = 7; y < 11; y++) {
        fieldManager.setCell(5, y, new Puyo('blue'));
    }
    
    const groups = fieldManager.findConnectedGroups();
    expect(groups).toHaveLength(2);
});

runner.test('should not connect different colored puyo', () => {
    const fieldManager = new FieldManager();
    
    // Create mixed color line
    fieldManager.setCell(1, 11, new Puyo('red'));
    fieldManager.setCell(2, 11, new Puyo('red'));
    fieldManager.setCell(3, 11, new Puyo('blue')); // Different color breaks connection
    fieldManager.setCell(4, 11, new Puyo('red'));
    
    const groups = fieldManager.findConnectedGroups();
    expect(groups).toHaveLength(0); // No group has 4+ of same color
});

runner.test('should ignore clearing puyo in group detection', () => {
    const fieldManager = new FieldManager();
    
    // Create group with one clearing puyo
    fieldManager.setCell(0, 11, new Puyo('purple'));
    fieldManager.setCell(1, 11, new Puyo('purple'));
    fieldManager.setCell(2, 11, new Puyo('purple'));
    const clearingPuyo = new Puyo('purple');
    clearingPuyo.setClearing();
    fieldManager.setCell(3, 11, clearingPuyo);
    
    const groups = fieldManager.findConnectedGroups();
    expect(groups).toHaveLength(0); // Only 3 non-clearing puyo
});

runner.test('should clear groups correctly', () => {
    const fieldManager = new FieldManager();
    
    // Create a group to clear
    const puyo1 = new Puyo('red');
    const puyo2 = new Puyo('red');
    const puyo3 = new Puyo('red');
    const puyo4 = new Puyo('red');
    
    fieldManager.setCell(0, 11, puyo1);
    fieldManager.setCell(1, 11, puyo2);
    fieldManager.setCell(2, 11, puyo3);
    fieldManager.setCell(3, 11, puyo4);
    
    const groups = fieldManager.findConnectedGroups();
    const clearedCount = fieldManager.clearGroups(groups);
    
    expect(clearedCount).toBe(4);
    expect(fieldManager.getCell(0, 11)).toBeNull();
    expect(fieldManager.getCell(1, 11)).toBeNull();
    expect(fieldManager.getCell(2, 11)).toBeNull();
    expect(fieldManager.getCell(3, 11)).toBeNull();
});

runner.test('should detect if field has clearable groups', () => {
    const fieldManager = new FieldManager();
    
    // Empty field has no clearable groups
    expect(fieldManager.hasClearableGroups()).toBe(false);
    
    // Small group is not clearable
    fieldManager.setCell(0, 11, new Puyo('red'));
    fieldManager.setCell(1, 11, new Puyo('red'));
    fieldManager.setCell(2, 11, new Puyo('red'));
    expect(fieldManager.hasClearableGroups()).toBe(false);
    
    // Group of 4 is clearable
    fieldManager.setCell(3, 11, new Puyo('red'));
    expect(fieldManager.hasClearableGroups()).toBe(true);
});

runner.test('should find and clear groups with gravity', () => {
    const fieldManager = new FieldManager();
    
    // Create floating group that will need gravity after clearing
    fieldManager.setCell(1, 8, new Puyo('red'));
    fieldManager.setCell(1, 9, new Puyo('red'));
    fieldManager.setCell(1, 10, new Puyo('red'));
    fieldManager.setCell(1, 11, new Puyo('red'));
    
    // Add puyo above that should fall
    fieldManager.setCell(1, 5, new Puyo('blue'));
    
    const result = fieldManager.findAndClearGroups();
    
    expect(result.clearedCount).toBe(4);
    expect(result.gravityApplied).toBe(true);
    expect(fieldManager.getCell(1, 11)).toBe(fieldManager.getAllPuyo()[0].puyo); // Blue puyo fell
});

runner.test('should count connected puyo correctly', () => {
    const fieldManager = new FieldManager();
    
    // Create T-shaped group
    fieldManager.setCell(1, 10, new Puyo('green'));
    fieldManager.setCell(2, 10, new Puyo('green'));
    fieldManager.setCell(3, 10, new Puyo('green'));
    fieldManager.setCell(2, 9, new Puyo('green'));
    fieldManager.setCell(2, 11, new Puyo('green'));
    
    const count = fieldManager.countConnectedPuyo(2, 10);
    expect(count).toBe(5);
    
    // Count from empty position
    expect(fieldManager.countConnectedPuyo(0, 0)).toBe(0);
});

runner.test('should get puyo by color', () => {
    const fieldManager = new FieldManager();
    
    fieldManager.setCell(0, 10, new Puyo('red'));
    fieldManager.setCell(1, 10, new Puyo('blue'));
    fieldManager.setCell(2, 10, new Puyo('red'));
    fieldManager.setCell(3, 10, new Puyo('green'));
    
    const redPuyo = fieldManager.getPuyoByColor('red');
    expect(redPuyo).toHaveLength(2);
    
    const yellowPuyo = fieldManager.getPuyoByColor('yellow');
    expect(yellowPuyo).toHaveLength(0);
});

runner.test('should handle complex connected group scenarios', () => {
    const fieldManager = new FieldManager();
    
    // Create a complex shape: plus sign
    fieldManager.setCell(2, 8, new Puyo('purple'));  // Top
    fieldManager.setCell(1, 9, new Puyo('purple'));  // Left
    fieldManager.setCell(2, 9, new Puyo('purple'));  // Center
    fieldManager.setCell(3, 9, new Puyo('purple'));  // Right
    fieldManager.setCell(2, 10, new Puyo('purple')); // Bottom
    
    const groups = fieldManager.findConnectedGroups();
    expect(groups).toHaveLength(1);
    expect(groups[0]).toHaveLength(5);
    
    // Verify all positions are in the group
    const positions = groups[0].map(item => `${item.x},${item.y}`);
    expect(positions).toContainEqual('2,8');
    expect(positions).toContainEqual('1,9');
    expect(positions).toContainEqual('2,9');
    expect(positions).toContainEqual('3,9');
    expect(positions).toContainEqual('2,10');
});

// Export test runner
export const FieldManagerTestRunner = runner;