/**
 * Unit tests for ZenkeshiManager class
 * Tests zenkeshi detection, bonus calculation, and visual feedback
 */

import { ZenkeshiManager } from '../../models/ZenkeshiManager.js';

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

// Basic initialization tests
test('should initialize with default values', () => {
    const manager = new ZenkeshiManager();
    
    expect(manager.totalZenkeshiCount).toBe(0);
    expect(manager.currentGameZenkeshiCount).toBe(0);
    expect(manager.consecutiveZenkeshi).toBe(0);
    expect(manager.maxConsecutiveZenkeshi).toBe(0);
    expect(manager.isZenkeshiActive).toBeFalsy();
    expect(manager.zenkeshiHistory.length).toBe(0);
});

// Zenkeshi detection tests
test('should detect zenkeshi from empty field array', () => {
    const manager = new ZenkeshiManager();
    
    // Empty field (6x12)
    const emptyField = Array(12).fill(null).map(() => Array(6).fill(null));
    expect(manager.detectZenkeshiFromField(emptyField)).toBeTruthy();
    
    // Field with one puyo
    const fieldWithPuyo = Array(12).fill(null).map(() => Array(6).fill(null));
    fieldWithPuyo[11][0] = { color: 'red' };
    expect(manager.detectZenkeshiFromField(fieldWithPuyo)).toBeFalsy();
});

test('should handle invalid field input', () => {
    const manager = new ZenkeshiManager();
    
    expect(manager.detectZenkeshiFromField(null)).toBeFalsy();
    expect(manager.detectZenkeshiFromField(undefined)).toBeFalsy();
    expect(manager.detectZenkeshiFromField([])).toBeTruthy(); // Empty array is considered empty field
    expect(manager.detectZenkeshiFromField('invalid')).toBeFalsy();
});

// Bonus calculation tests
test('should calculate correct zenkeshi bonus', () => {
    const manager = new ZenkeshiManager();
    
    expect(manager.calculateZenkeshiBonus(1)).toBe(ZenkeshiManager.BONUS_POINTS);
    expect(manager.calculateZenkeshiBonus(2)).toBe(Math.floor(ZenkeshiManager.BONUS_POINTS * 1.5));
    expect(manager.calculateZenkeshiBonus(3)).toBe(Math.floor(ZenkeshiManager.BONUS_POINTS * 2.0));
    expect(manager.calculateZenkeshiBonus(4)).toBe(Math.floor(ZenkeshiManager.BONUS_POINTS * 2.5));
});

// Zenkeshi processing tests
test('should process first zenkeshi correctly', () => {
    const manager = new ZenkeshiManager();
    const clearData = { groups: [], totalCleared: 4 };
    const baseScore = 100;
    
    const result = manager.processZenkeshi(clearData, baseScore);
    
    expect(result.bonusPoints).toBe(ZenkeshiManager.BONUS_POINTS);
    expect(result.totalScore).toBe(baseScore + ZenkeshiManager.BONUS_POINTS);
    expect(result.consecutiveCount).toBe(1);
    expect(result.zenkeshiNumber).toBe(1);
    expect(manager.totalZenkeshiCount).toBe(1);
    expect(manager.currentGameZenkeshiCount).toBe(1);
    expect(manager.consecutiveZenkeshi).toBe(1);
    expect(manager.isZenkeshiActive).toBeTruthy();
});

test('should process consecutive zenkeshi correctly', () => {
    const manager = new ZenkeshiManager();
    const clearData = { groups: [], totalCleared: 4 };
    
    // First zenkeshi
    const result1 = manager.processZenkeshi(clearData, 100);
    expect(result1.consecutiveCount).toBe(1);
    expect(result1.bonusPoints).toBe(ZenkeshiManager.BONUS_POINTS);
    
    // Second consecutive zenkeshi
    const result2 = manager.processZenkeshi(clearData, 100);
    expect(result2.consecutiveCount).toBe(2);
    expect(result2.bonusPoints).toBe(Math.floor(ZenkeshiManager.BONUS_POINTS * 1.5));
    
    // Third consecutive zenkeshi
    const result3 = manager.processZenkeshi(clearData, 100);
    expect(result3.consecutiveCount).toBe(3);
    expect(result3.bonusPoints).toBe(Math.floor(ZenkeshiManager.BONUS_POINTS * 2.0));
    
    expect(manager.maxConsecutiveZenkeshi).toBe(3);
});

test('should reset consecutive count correctly', () => {
    const manager = new ZenkeshiManager();
    const clearData = { groups: [], totalCleared: 4 };
    
    // Build up consecutive count
    manager.processZenkeshi(clearData, 100);
    manager.processZenkeshi(clearData, 100);
    expect(manager.consecutiveZenkeshi).toBe(2);
    
    // Reset consecutive count
    manager.resetConsecutiveZenkeshi();
    expect(manager.consecutiveZenkeshi).toBe(0);
    
    // Next zenkeshi should start from 1 again
    const result = manager.processZenkeshi(clearData, 100);
    expect(result.consecutiveCount).toBe(1);
    expect(result.bonusPoints).toBe(ZenkeshiManager.BONUS_POINTS);
});

// Visual effect tests
test('should provide visual effect data', () => {
    const manager = new ZenkeshiManager();
    const effectData = manager.getVisualEffectData();
    
    expect(effectData.flashDuration).toBe(ZenkeshiManager.VISUAL_EFFECTS.FLASH_DURATION);
    expect(effectData.flashColor).toBe(ZenkeshiManager.VISUAL_EFFECTS.FLASH_COLOR);
    expect(effectData.textDuration).toBe(ZenkeshiManager.VISUAL_EFFECTS.TEXT_DURATION);
    expect(effectData.textColor).toBe(ZenkeshiManager.VISUAL_EFFECTS.TEXT_COLOR);
    expect(effectData.particleCount).toBe(ZenkeshiManager.VISUAL_EFFECTS.PARTICLE_COUNT);
    expect(effectData.isActive).toBeFalsy(); // Initially inactive
});

test('should track effect active state', () => {
    const manager = new ZenkeshiManager();
    const clearData = { groups: [], totalCleared: 4 };
    const currentTime = Date.now();
    
    // Process zenkeshi
    manager.processZenkeshi(clearData, 100, currentTime);
    expect(manager.isEffectActive(currentTime)).toBeTruthy();
    
    // Effect should still be active within duration
    const nearFuture = currentTime + ZenkeshiManager.VISUAL_EFFECTS.TEXT_DURATION / 2;
    expect(manager.isEffectActive(nearFuture)).toBeTruthy();
    
    // Effect should be inactive after duration
    const farFuture = currentTime + ZenkeshiManager.VISUAL_EFFECTS.TEXT_DURATION + 100;
    expect(manager.isEffectActive(farFuture)).toBeFalsy();
});

test('should update effect state correctly', () => {
    const manager = new ZenkeshiManager();
    const clearData = { groups: [], totalCleared: 4 };
    const currentTime = Date.now();
    
    manager.processZenkeshi(clearData, 100, currentTime);
    expect(manager.isZenkeshiActive).toBeTruthy();
    
    // Update within duration - should remain active
    manager.updateEffect(currentTime + 100);
    expect(manager.isZenkeshiActive).toBeTruthy();
    
    // Update after duration - should become inactive
    manager.updateEffect(currentTime + ZenkeshiManager.VISUAL_EFFECTS.TEXT_DURATION + 100);
    expect(manager.isZenkeshiActive).toBeFalsy();
});

// Display info tests
test('should provide correct display information', () => {
    const manager = new ZenkeshiManager();
    const clearData = { groups: [], totalCleared: 4 };
    
    // Initial display info
    let displayInfo = manager.getDisplayInfo();
    expect(displayInfo.totalCount).toBe(0);
    expect(displayInfo.currentGameCount).toBe(0);
    expect(displayInfo.consecutiveCount).toBe(0);
    expect(displayInfo.lastBonusPoints).toBe(0);
    
    // After first zenkeshi
    manager.processZenkeshi(clearData, 100);
    displayInfo = manager.getDisplayInfo();
    expect(displayInfo.totalCount).toBe(1);
    expect(displayInfo.currentGameCount).toBe(1);
    expect(displayInfo.consecutiveCount).toBe(1);
    expect(displayInfo.lastBonusPoints).toBe(ZenkeshiManager.BONUS_POINTS);
    expect(displayInfo.nextBonusPoints).toBe(Math.floor(ZenkeshiManager.BONUS_POINTS * 1.5));
});

// History and statistics tests
test('should track zenkeshi history', () => {
    const manager = new ZenkeshiManager();
    const clearData = { groups: [], totalCleared: 4 };
    
    expect(manager.getAllZenkeshi().length).toBe(0);
    expect(manager.getLastZenkeshi()).toBe(null);
    
    manager.processZenkeshi(clearData, 100);
    manager.processZenkeshi(clearData, 150);
    
    const history = manager.getAllZenkeshi();
    expect(history.length).toBe(2);
    expect(history[0].gameZenkeshiNumber).toBe(1);
    expect(history[1].gameZenkeshiNumber).toBe(2);
    
    const lastZenkeshi = manager.getLastZenkeshi();
    expect(lastZenkeshi.gameZenkeshiNumber).toBe(2);
    expect(lastZenkeshi.baseScore).toBe(150);
});

test('should provide correct statistics', () => {
    const manager = new ZenkeshiManager();
    const clearData = { groups: [], totalCleared: 4 };
    
    manager.processZenkeshi(clearData, 100);
    manager.processZenkeshi(clearData, 150);
    manager.resetConsecutiveZenkeshi();
    manager.processZenkeshi(clearData, 200);
    
    const stats = manager.getStatistics();
    expect(stats.totalZenkeshi).toBe(3);
    expect(stats.currentGameZenkeshi).toBe(3);
    expect(stats.maxConsecutiveZenkeshi).toBe(2);
    expect(stats.totalBonusPoints).toBeGreaterThan(0);
    expect(stats.averageBonusPoints).toBeGreaterThan(0);
});

// Achievement tests
test('should track achievements correctly', () => {
    const manager = new ZenkeshiManager();
    const clearData = { groups: [], totalCleared: 4 };
    
    // Initial achievements
    let achievements = manager.getAchievements();
    expect(achievements.firstZenkeshi).toBeFalsy();
    expect(achievements.consecutiveZenkeshi).toBeFalsy();
    
    // After first zenkeshi
    manager.processZenkeshi(clearData, 100);
    achievements = manager.getAchievements();
    expect(achievements.firstZenkeshi).toBeTruthy();
    
    // After consecutive zenkeshi
    manager.processZenkeshi(clearData, 100);
    achievements = manager.getAchievements();
    expect(achievements.consecutiveZenkeshi).toBeTruthy();
    
    // Check individual achievements
    const achievementList = achievements.achievements;
    const firstClear = achievementList.find(a => a.name === 'First Clear');
    const doubleClear = achievementList.find(a => a.name === 'Double Clear');
    
    expect(firstClear.unlocked).toBeTruthy();
    expect(doubleClear.unlocked).toBeTruthy();
});

// Game management tests
test('should handle new game correctly', () => {
    const manager = new ZenkeshiManager();
    const clearData = { groups: [], totalCleared: 4 };
    
    // Build up some state
    manager.processZenkeshi(clearData, 100);
    manager.processZenkeshi(clearData, 100);
    expect(manager.currentGameZenkeshiCount).toBe(2);
    expect(manager.consecutiveZenkeshi).toBe(2);
    
    // Start new game
    manager.startNewGame();
    expect(manager.currentGameZenkeshiCount).toBe(0);
    expect(manager.consecutiveZenkeshi).toBe(0);
    expect(manager.isZenkeshiActive).toBeFalsy();
    
    // Total count should be preserved
    expect(manager.totalZenkeshiCount).toBe(2);
});

// Static method tests
test('should provide celebration messages', () => {
    expect(ZenkeshiManager.getCelebrationMessage(1)).toBe('ZENKESHI!');
    expect(ZenkeshiManager.getCelebrationMessage(2)).toBe('DOUBLE ZENKESHI!');
    expect(ZenkeshiManager.getCelebrationMessage(3)).toBe('TRIPLE ZENKESHI!');
    expect(ZenkeshiManager.getCelebrationMessage(5)).toBe('PERFECT CLEAR!');
    expect(ZenkeshiManager.getCelebrationMessage(10)).toBe('PERFECT CLEAR!');
});

test('should provide sound effect names', () => {
    expect(ZenkeshiManager.getSoundEffect(1)).toBe('zenkeshi');
    expect(ZenkeshiManager.getSoundEffect(2)).toBe('double_zenkeshi');
    expect(ZenkeshiManager.getSoundEffect(3)).toBe('triple_zenkeshi');
    expect(ZenkeshiManager.getSoundEffect(4)).toBe('perfect_clear');
    expect(ZenkeshiManager.getSoundEffect(10)).toBe('perfect_clear');
});

// Serialization tests
test('should clone correctly', () => {
    const manager = new ZenkeshiManager();
    const clearData = { groups: [], totalCleared: 4 };
    
    manager.processZenkeshi(clearData, 100);
    manager.processZenkeshi(clearData, 150);
    
    const cloned = manager.clone();
    
    expect(cloned.totalZenkeshiCount).toBe(manager.totalZenkeshiCount);
    expect(cloned.currentGameZenkeshiCount).toBe(manager.currentGameZenkeshiCount);
    expect(cloned.consecutiveZenkeshi).toBe(manager.consecutiveZenkeshi);
    expect(cloned.maxConsecutiveZenkeshi).toBe(manager.maxConsecutiveZenkeshi);
    expect(cloned.zenkeshiHistory.length).toBe(manager.zenkeshiHistory.length);
});

test('should serialize and deserialize correctly', () => {
    const manager = new ZenkeshiManager();
    const clearData = { groups: [], totalCleared: 4 };
    
    manager.processZenkeshi(clearData, 100);
    manager.processZenkeshi(clearData, 150);
    
    const json = manager.toJSON();
    const restored = ZenkeshiManager.fromJSON(json);
    
    expect(restored.totalZenkeshiCount).toBe(manager.totalZenkeshiCount);
    expect(restored.currentGameZenkeshiCount).toBe(manager.currentGameZenkeshiCount);
    expect(restored.consecutiveZenkeshi).toBe(manager.consecutiveZenkeshi);
    expect(restored.maxConsecutiveZenkeshi).toBe(manager.maxConsecutiveZenkeshi);
    expect(restored.zenkeshiHistory.length).toBe(manager.zenkeshiHistory.length);
    expect(restored.isZenkeshiActive).toBe(manager.isZenkeshiActive);
});

// Run all tests
function runTests() {
    console.log('Running ZenkeshiManager tests...\n');
    
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
    window.ZenkeshiManagerTests = { runTests, testResults };
} else {
    export { runTests, testResults };
}