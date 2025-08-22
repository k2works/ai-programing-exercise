/**
 * Unit tests for ScoreManager class
 * Tests basic scoring, chain calculations, and zenkeshi bonuses
 */

import { ScoreManager } from '../../models/ScoreManager.js';

// Test runner setup
const tests = [];
let testResults = { passed: 0, failed: 0, total: 0 };

function test(name, testFn) {
    tests.push({ name, testFn });
}

function expect(actual) {
    return {
        toBe: (expected) => {
            if (actual !== expected) {
                throw new Error(`Expected ${expected}, but got ${actual}`);
            }
        },
        toBeCloseTo: (expected, precision = 2) => {
            const diff = Math.abs(actual - expected);
            const tolerance = Math.pow(10, -precision);
            if (diff > tolerance) {
                throw new Error(`Expected ${actual} to be close to ${expected} (within ${tolerance})`);
            }
        },
        toEqual: (expected) => {
            if (JSON.stringify(actual) !== JSON.stringify(expected)) {
                throw new Error(`Expected ${JSON.stringify(expected)}, but got ${JSON.stringify(actual)}`);
            }
        },
        toBeGreaterThan: (expected) => {
            if (actual <= expected) {
                throw new Error(`Expected ${actual} to be greater than ${expected}`);
            }
        },
        toBeLessThan: (expected) => {
            if (actual >= expected) {
                throw new Error(`Expected ${actual} to be less than ${expected}`);
            }
        }
    };
}

// Basic scoring tests
test('should calculate base score correctly for minimum group', () => {
    const scoreManager = new ScoreManager();
    const baseScore = scoreManager.calculateBaseScore(4, 1);
    expect(baseScore).toBe(40); // 4 * 10 base points
});

test('should return 0 for groups smaller than 4', () => {
    const scoreManager = new ScoreManager();
    expect(scoreManager.calculateBaseScore(3, 1)).toBe(0);
    expect(scoreManager.calculateBaseScore(2, 1)).toBe(0);
    expect(scoreManager.calculateBaseScore(1, 1)).toBe(0);
});

test('should add bonus for larger groups', () => {
    const scoreManager = new ScoreManager();
    const baseScore4 = scoreManager.calculateBaseScore(4, 1);
    const baseScore5 = scoreManager.calculateBaseScore(5, 1);
    const baseScore6 = scoreManager.calculateBaseScore(6, 1);
    
    expect(baseScore5).toBeGreaterThan(baseScore4);
    expect(baseScore6).toBeGreaterThan(baseScore5);
});

test('should add bonus for multiple groups cleared simultaneously', () => {
    const scoreManager = new ScoreManager();
    const singleGroup = scoreManager.calculateBaseScore(4, 1);
    const doubleGroup = scoreManager.calculateBaseScore(4, 2);
    const tripleGroup = scoreManager.calculateBaseScore(4, 3);
    
    expect(doubleGroup).toBe(singleGroup + 50); // 50 point bonus for second group
    expect(tripleGroup).toBe(singleGroup + 100); // 100 point bonus for third group
});

// Chain multiplier tests
test('should return correct chain multipliers', () => {
    const scoreManager = new ScoreManager();
    
    expect(scoreManager.getChainMultiplier(0)).toBe(1);  // No chain
    expect(scoreManager.getChainMultiplier(1)).toBe(1);  // 1 chain
    expect(scoreManager.getChainMultiplier(2)).toBe(8);  // 2 chain
    expect(scoreManager.getChainMultiplier(3)).toBe(16); // 3 chain
    expect(scoreManager.getChainMultiplier(4)).toBe(32); // 4 chain
});

test('should handle chain levels beyond table', () => {
    const scoreManager = new ScoreManager();
    const maxTableMultiplier = scoreManager.getChainMultiplier(12);
    const beyondTableMultiplier = scoreManager.getChainMultiplier(20);
    
    expect(beyondTableMultiplier).toBe(maxTableMultiplier);
});

test('should handle negative chain levels', () => {
    const scoreManager = new ScoreManager();
    expect(scoreManager.getChainMultiplier(-1)).toBe(1);
    expect(scoreManager.getChainMultiplier(-5)).toBe(1);
});

// Chain scoring tests
test('should calculate chain score correctly', () => {
    const scoreManager = new ScoreManager();
    const baseScore = scoreManager.calculateBaseScore(4, 1); // 40 points
    const chainScore = scoreManager.calculateChainScore(4, 1, 2); // 2-chain multiplier = 8
    
    expect(chainScore).toBe(baseScore * 8); // 40 * 8 = 320
});

test('should handle zero chain level', () => {
    const scoreManager = new ScoreManager();
    const baseScore = scoreManager.calculateBaseScore(4, 1);
    const chainScore = scoreManager.calculateChainScore(4, 1, 0);
    
    expect(chainScore).toBe(baseScore); // No multiplier for 0 chain
});

// Chain sequence tests
test('should start chain correctly', () => {
    const scoreManager = new ScoreManager();
    scoreManager.startChain();
    
    expect(scoreManager.getCurrentChainLevel()).toBe(0);
    expect(scoreManager.getChainHistory()).toEqual([]);
    expect(scoreManager.isChainActive()).toBe(false);
});

test('should add chain links correctly', () => {
    const scoreManager = new ScoreManager();
    scoreManager.startChain();
    
    const link1 = scoreManager.addChainLink(4, 1);
    expect(link1.chainLevel).toBe(0);
    expect(link1.puyoCount).toBe(4);
    expect(link1.multiplier).toBe(1);
    expect(scoreManager.getCurrentChainLevel()).toBe(1);
    expect(scoreManager.isChainActive()).toBe(true);
    
    const link2 = scoreManager.addChainLink(5, 1);
    expect(link2.chainLevel).toBe(1);
    expect(link2.multiplier).toBe(1);
    expect(scoreManager.getCurrentChainLevel()).toBe(2);
    
    const link3 = scoreManager.addChainLink(4, 1);
    expect(link3.chainLevel).toBe(2);
    expect(link3.multiplier).toBe(8);
    expect(scoreManager.getCurrentChainLevel()).toBe(3);
});

test('should track total score across chain links', () => {
    const scoreManager = new ScoreManager();
    scoreManager.startChain();
    
    const initialScore = scoreManager.getTotalScore();
    
    const link1 = scoreManager.addChainLink(4, 1); // 40 * 1 = 40
    expect(scoreManager.getTotalScore()).toBe(initialScore + 40);
    
    const link2 = scoreManager.addChainLink(4, 1); // 40 * 1 = 40
    expect(scoreManager.getTotalScore()).toBe(initialScore + 80);
    
    const link3 = scoreManager.addChainLink(4, 1); // 40 * 8 = 320
    expect(scoreManager.getTotalScore()).toBe(initialScore + 400);
});

test('should end chain correctly', () => {
    const scoreManager = new ScoreManager();
    scoreManager.startChain();
    
    scoreManager.addChainLink(4, 1);
    scoreManager.addChainLink(4, 1);
    scoreManager.addChainLink(4, 1);
    
    const chainSummary = scoreManager.endChain();
    
    expect(chainSummary.totalChainLinks).toBe(3);
    expect(chainSummary.maxChainLevel).toBe(2);
    expect(chainSummary.chainHistory.length).toBe(3);
    expect(scoreManager.getCurrentChainLevel()).toBe(0);
    expect(scoreManager.isChainActive()).toBe(false);
});

// Zenkeshi bonus tests
test('should calculate zenkeshi bonus correctly', () => {
    const scoreManager = new ScoreManager();
    
    expect(scoreManager.calculateZenkeshiBonus(true)).toBe(ScoreManager.ZENKESHI_BONUS);
    expect(scoreManager.calculateZenkeshiBonus(false)).toBe(0);
});

// Complete clearing sequence tests
test('should process clearing sequence without zenkeshi', () => {
    const scoreManager = new ScoreManager();
    scoreManager.startChain();
    
    const clearedGroups = [
        [{ x: 0, y: 0 }, { x: 1, y: 0 }, { x: 2, y: 0 }, { x: 3, y: 0 }] // 4 puyos
    ];
    
    const result = scoreManager.processClearingSequence(clearedGroups, false);
    
    expect(result.totalPuyosCleared).toBe(4);
    expect(result.groupCount).toBe(1);
    expect(result.zenkeshiBonus).toBe(0);
    expect(result.chainLevel).toBe(0);
    expect(result.score).toBe(40); // Base score only
});

test('should process clearing sequence with zenkeshi', () => {
    const scoreManager = new ScoreManager();
    scoreManager.startChain();
    
    const clearedGroups = [
        [{ x: 0, y: 0 }, { x: 1, y: 0 }, { x: 2, y: 0 }, { x: 3, y: 0 }] // 4 puyos
    ];
    
    const result = scoreManager.processClearingSequence(clearedGroups, true);
    
    expect(result.zenkeshiBonus).toBe(ScoreManager.ZENKESHI_BONUS);
    expect(result.score).toBe(40 + ScoreManager.ZENKESHI_BONUS);
});

test('should process multiple groups in one clearing', () => {
    const scoreManager = new ScoreManager();
    scoreManager.startChain();
    
    const clearedGroups = [
        [{ x: 0, y: 0 }, { x: 1, y: 0 }, { x: 2, y: 0 }, { x: 3, y: 0 }], // 4 puyos
        [{ x: 0, y: 1 }, { x: 1, y: 1 }, { x: 2, y: 1 }, { x: 3, y: 1 }]  // 4 puyos
    ];
    
    const result = scoreManager.processClearingSequence(clearedGroups, false);
    
    expect(result.totalPuyosCleared).toBe(8);
    expect(result.groupCount).toBe(2);
    expect(result.score).toBeGreaterThan(80); // Should include group bonus
});

test('should handle empty clearing sequence', () => {
    const scoreManager = new ScoreManager();
    
    const result = scoreManager.processClearingSequence([], false);
    
    expect(result.score).toBe(0);
    expect(result.totalPuyosCleared).toBe(0);
    expect(result.chainLink).toBe(null);
});

// Statistics tests
test('should provide correct statistics', () => {
    const scoreManager = new ScoreManager();
    scoreManager.startChain();
    
    scoreManager.addChainLink(4, 1);
    scoreManager.addChainLink(5, 1);
    
    const stats = scoreManager.getStatistics();
    
    expect(stats.currentChainLevel).toBe(2);
    expect(stats.totalChainLinks).toBe(2);
    expect(stats.totalScore).toBeGreaterThan(0);
});

// Clone and serialization tests
test('should clone correctly', () => {
    const scoreManager = new ScoreManager();
    scoreManager.startChain();
    scoreManager.addChainLink(4, 1);
    
    const cloned = scoreManager.clone();
    
    expect(cloned.getCurrentChainLevel()).toBe(scoreManager.getCurrentChainLevel());
    expect(cloned.getTotalScore()).toBe(scoreManager.getTotalScore());
    expect(cloned.getChainHistory().length).toBe(scoreManager.getChainHistory().length);
});

test('should serialize and deserialize correctly', () => {
    const scoreManager = new ScoreManager();
    scoreManager.startChain();
    scoreManager.addChainLink(4, 1);
    scoreManager.addChainLink(5, 2);
    
    const json = scoreManager.toJSON();
    const restored = ScoreManager.fromJSON(json);
    
    expect(restored.getCurrentChainLevel()).toBe(scoreManager.getCurrentChainLevel());
    expect(restored.getTotalScore()).toBe(scoreManager.getTotalScore());
    expect(restored.getChainHistory().length).toBe(scoreManager.getChainHistory().length);
});

test('should reset correctly', () => {
    const scoreManager = new ScoreManager();
    scoreManager.startChain();
    scoreManager.addChainLink(4, 1);
    scoreManager.addChainLink(5, 1);
    
    scoreManager.reset();
    
    expect(scoreManager.getCurrentChainLevel()).toBe(0);
    expect(scoreManager.getTotalScore()).toBe(0);
    expect(scoreManager.getChainHistory()).toEqual([]);
    expect(scoreManager.isChainActive()).toBe(false);
});

// Run all tests
function runTests() {
    console.log('Running ScoreManager tests...\n');
    
    tests.forEach(({ name, testFn }) => {
        testResults.total++;
        try {
            testFn();
            testResults.passed++;
            console.log(`✓ ${name}`);
        } catch (error) {
            testResults.failed++;
            console.log(`✗ ${name}`);
            console.log(`  Error: ${error.message}`);
        }
    });
    
    console.log(`\nTest Results: ${testResults.passed}/${testResults.total} passed`);
    
    if (testResults.failed > 0) {
        console.log(`${testResults.failed} tests failed`);
        return false;
    } else {
        console.log('All tests passed!');
        return true;
    }
}

// Export for use in test runner
if (typeof window !== 'undefined') {
    window.ScoreManagerTests = { runTests, testResults };
} else {
    export { runTests, testResults };
}