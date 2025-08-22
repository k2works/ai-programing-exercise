/**
 * Unit tests for Chain class
 * Tests chain sequence tracking, timing, and state management
 */

import { Chain } from '../../models/Chain.js';

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
        toBeTruthy: () => {
            if (!actual) {
                throw new Error(`Expected ${actual} to be truthy`);
            }
        },
        toBeFalsy: () => {
            if (actual) {
                throw new Error(`Expected ${actual} to be falsy`);
            }
        }
    };
}

// Basic chain state tests
test('should initialize with inactive state', () => {
    const chain = new Chain();
    expect(chain.isInactive()).toBeTruthy();
    expect(chain.isActive()).toBeFalsy();
    expect(chain.isCompleted()).toBeFalsy();
    expect(chain.getCurrentLevel()).toBe(0);
    expect(chain.getLinkCount()).toBe(0);
});

test('should start chain correctly', () => {
    const chain = new Chain();
    const startTime = Date.now();
    
    chain.start(startTime);
    
    expect(chain.isActive()).toBeTruthy();
    expect(chain.isInactive()).toBeFalsy();
    expect(chain.getCurrentLevel()).toBe(0);
    expect(chain.getLinkCount()).toBe(0);
});

test('should reset chain state correctly', () => {
    const chain = new Chain();
    chain.start();
    chain.addLink({ groups: [], totalCleared: 4 }, 100, 1);
    
    chain.reset();
    
    expect(chain.isInactive()).toBeTruthy();
    expect(chain.getCurrentLevel()).toBe(0);
    expect(chain.getLinkCount()).toBe(0);
    expect(chain.totalScore).toBe(0);
});

// Chain link tests
test('should add chain links correctly', () => {
    const chain = new Chain();
    chain.start();
    
    const clearData = {
        groups: [{ x: 0, y: 0 }, { x: 1, y: 0 }, { x: 2, y: 0 }, { x: 3, y: 0 }],
        totalCleared: 4,
        groupCount: 1
    };
    
    const link = chain.addLink(clearData, 100, 1);
    
    expect(link.linkNumber).toBe(0);
    expect(link.chainLevel).toBe(0);
    expect(link.puyosCleared).toBe(4);
    expect(link.score).toBe(100);
    expect(link.multiplier).toBe(1);
    expect(chain.getCurrentLevel()).toBe(1);
    expect(chain.getLinkCount()).toBe(1);
    expect(chain.totalScore).toBe(100);
});

test('should track multiple chain links', () => {
    const chain = new Chain();
    chain.start();
    
    const link1 = chain.addLink({ totalCleared: 4 }, 100, 1);
    const link2 = chain.addLink({ totalCleared: 4 }, 200, 2);
    const link3 = chain.addLink({ totalCleared: 5 }, 400, 4);
    
    expect(chain.getLinkCount()).toBe(3);
    expect(chain.getCurrentLevel()).toBe(3);
    expect(chain.totalScore).toBe(700);
    expect(chain.getMaxLevel()).toBe(2); // Max chain level achieved
    
    const lastLink = chain.getLastLink();
    expect(lastLink.chainLevel).toBe(2);
    expect(lastLink.score).toBe(400);
});

test('should not allow adding links to inactive chain', () => {
    const chain = new Chain();
    
    try {
        chain.addLink({ totalCleared: 4 }, 100, 1);
        expect(false).toBeTruthy(); // Should not reach here
    } catch (error) {
        expect(error.message).toBe('Cannot add link to inactive chain');
    }
});

// Chain completion tests
test('should complete chain correctly', () => {
    const chain = new Chain();
    const startTime = Date.now();
    chain.start(startTime);
    
    chain.addLink({ totalCleared: 4 }, 100, 1);
    chain.addLink({ totalCleared: 4 }, 200, 2);
    
    const endTime = startTime + 1000;
    const summary = chain.complete(endTime);
    
    expect(chain.isCompleted()).toBeTruthy();
    expect(chain.isActive()).toBeFalsy();
    expect(summary.totalLinks).toBe(2);
    expect(summary.maxChainLevel).toBe(1);
    expect(summary.totalScore).toBe(300);
    expect(summary.duration).toBe(1000);
});

// Chain statistics tests
test('should provide correct statistics', () => {
    const chain = new Chain();
    chain.start();
    
    chain.addLink({ totalCleared: 4 }, 100, 1);
    chain.addLink({ totalCleared: 5 }, 250, 2);
    
    const stats = chain.getStatistics();
    
    expect(stats.totalLinks).toBe(2);
    expect(stats.currentLevel).toBe(2);
    expect(stats.maxLevel).toBe(1);
    expect(stats.totalScore).toBe(350);
    expect(stats.totalPuyosCleared).toBe(9);
    expect(stats.averageScore).toBe(175);
    expect(stats.averageMultiplier).toBe(1.5);
    expect(stats.isActive).toBeTruthy();
});

// Chain display info tests
test('should provide display information', () => {
    const chain = new Chain();
    chain.start();
    
    chain.addLink({ totalCleared: 4 }, 100, 1);
    chain.addLink({ totalCleared: 4 }, 320, 8);
    
    const displayInfo = chain.getDisplayInfo();
    
    expect(displayInfo.chainLevel).toBe(1);
    expect(displayInfo.currentScore).toBe(420);
    expect(displayInfo.lastLinkScore).toBe(320);
    expect(displayInfo.lastMultiplier).toBe(8);
    expect(displayInfo.totalLinks).toBe(2);
    expect(displayInfo.isActive).toBeTruthy();
    expect(displayInfo.puyosCleared).toBe(8);
});

// Processing state tests
test('should handle processing state correctly', () => {
    const chain = new Chain();
    chain.start();
    
    expect(chain.isCurrentlyProcessing()).toBeFalsy();
    
    chain.setProcessing(true);
    expect(chain.isCurrentlyProcessing()).toBeTruthy();
    
    chain.setProcessing(false);
    expect(chain.isCurrentlyProcessing()).toBeFalsy();
});

// Pending clears tests
test('should handle pending clears', () => {
    const chain = new Chain();
    
    expect(chain.hasPendingClears()).toBeFalsy();
    
    chain.addPendingClear({ groups: [], totalCleared: 4 });
    chain.addPendingClear({ groups: [], totalCleared: 5 });
    
    expect(chain.hasPendingClears()).toBeTruthy();
    
    const pending = chain.getPendingClears();
    expect(pending.length).toBe(2);
    expect(chain.hasPendingClears()).toBeFalsy(); // Should be cleared after getting
});

// Timing info tests
test('should provide timing information', () => {
    const chain = new Chain();
    const timing = chain.getTimingInfo();
    
    expect(timing.clearDelay).toBe(Chain.TIMING.CLEAR_DELAY);
    expect(timing.gravityDelay).toBe(Chain.TIMING.GRAVITY_DELAY);
    expect(timing.chainDelay).toBe(Chain.TIMING.CHAIN_DELAY);
    expect(timing.animationDuration).toBe(Chain.TIMING.ANIMATION_DURATION);
    expect(timing.totalDelay).toBeGreaterThan(0);
});

// Chain level name and color tests
test('should provide correct chain level names', () => {
    expect(Chain.getChainLevelName(0)).toBe('Clear');
    expect(Chain.getChainLevelName(1)).toBe('1 Chain');
    expect(Chain.getChainLevelName(2)).toBe('2 Chain');
    expect(Chain.getChainLevelName(5)).toBe('5 Chain');
});

test('should provide chain level colors', () => {
    const color0 = Chain.getChainLevelColor(0);
    const color1 = Chain.getChainLevelColor(1);
    const color5 = Chain.getChainLevelColor(5);
    
    expect(color0).toBe('#FFFFFF'); // White for no chain
    expect(color1).toBe('#FFFF00'); // Yellow for 1 chain
    expect(typeof color5).toBe('string');
    expect(color5.startsWith('#')).toBeTruthy();
});

// Clone and serialization tests
test('should clone correctly', () => {
    const chain = new Chain();
    chain.start();
    chain.addLink({ totalCleared: 4 }, 100, 1);
    chain.addLink({ totalCleared: 5 }, 250, 2);
    
    const cloned = chain.clone();
    
    expect(cloned.getCurrentLevel()).toBe(chain.getCurrentLevel());
    expect(cloned.getLinkCount()).toBe(chain.getLinkCount());
    expect(cloned.totalScore).toBe(chain.totalScore);
    expect(cloned.isActive()).toBe(chain.isActive());
});

test('should serialize and deserialize correctly', () => {
    const chain = new Chain();
    chain.start();
    chain.addLink({ totalCleared: 4 }, 100, 1);
    chain.addLink({ totalCleared: 5 }, 250, 2);
    chain.setProcessing(true);
    
    const json = chain.toJSON();
    const restored = Chain.fromJSON(json);
    
    expect(restored.getCurrentLevel()).toBe(chain.getCurrentLevel());
    expect(restored.getLinkCount()).toBe(chain.getLinkCount());
    expect(restored.totalScore).toBe(chain.totalScore);
    expect(restored.isCurrentlyProcessing()).toBe(chain.isCurrentlyProcessing());
    expect(restored.getMaxLevel()).toBe(chain.getMaxLevel());
});

// Edge cases
test('should handle empty chain completion', () => {
    const chain = new Chain();
    chain.start();
    
    const summary = chain.complete();
    
    expect(summary.totalLinks).toBe(0);
    expect(summary.maxChainLevel).toBe(0);
    expect(summary.totalScore).toBe(0);
    expect(summary.averageMultiplier).toBe(0);
});

test('should handle getting links from empty chain', () => {
    const chain = new Chain();
    
    expect(chain.getLink(0)).toBe(null);
    expect(chain.getLastLink()).toBe(null);
    expect(chain.getAllLinks()).toEqual([]);
});

// Run all tests
function runTests() {
    console.log('Running Chain tests...\n');
    
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
    window.ChainTests = { runTests, testResults };
} else {
    export { runTests, testResults };
}