/**
 * Unit tests for Animation system
 * Tests animation timing, easing functions, and animation management
 */

import { Animation, AnimationManager } from '../../rendering/Animation.js';

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
        console.log('Running Animation tests...');
        
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
        
        console.log(`\nAnimation Test Summary: ${passed} passed, ${failed} failed`);
        
        if (failed > 0) {
            console.log('\nFailed tests:');
            this.results.filter(r => r.status === 'FAIL').forEach(r => {
                console.log(`  - ${r.name}: ${r.error}`);
            });
        }
    }
}

// Helper function for assertions
function assert(condition, message) {
    if (!condition) {
        throw new Error(message || 'Assertion failed');
    }
}

function assertEqual(actual, expected, message) {
    if (actual !== expected) {
        throw new Error(message || `Expected ${expected}, but got ${actual}`);
    }
}

function assertGreaterThan(actual, expected, message) {
    if (actual <= expected) {
        throw new Error(message || `Expected ${actual} to be greater than ${expected}`);
    }
}

function assertLessThan(actual, expected, message) {
    if (actual >= expected) {
        throw new Error(message || `Expected ${actual} to be less than ${expected}`);
    }
}

function assertAlmostEqual(actual, expected, tolerance = 0.01, message) {
    if (Math.abs(actual - expected) > tolerance) {
        throw new Error(message || `Expected ${actual} to be approximately ${expected} (tolerance: ${tolerance})`);
    }
}

// Create test runner instance
const testRunner = new TestRunner();

// Animation class tests
testRunner.test('should create animation with correct properties', () => {
    const startTime = Date.now();
    const animation = new Animation('test', startTime, 1000, { value: 42 });
    
    assertEqual(animation.type, 'test');
    assertEqual(animation.startTime, startTime);
    assertEqual(animation.duration, 1000);
    assertEqual(animation.properties.value, 42);
    assertEqual(animation.isComplete, false);
    assertEqual(animation.progress, 0);
});

testRunner.test('should update progress correctly', () => {
    const startTime = 1000;
    const animation = new Animation('test', startTime, 1000);
    
    // Test at 25% completion
    animation.update(1250);
    assertAlmostEqual(animation.progress, 0.25);
    assertEqual(animation.isComplete, false);
    
    // Test at 100% completion
    animation.update(2000);
    assertAlmostEqual(animation.progress, 1.0);
    assertEqual(animation.isComplete, true);
    
    // Test beyond completion
    animation.update(2500);
    assertAlmostEqual(animation.progress, 1.0);
    assertEqual(animation.isComplete, true);
});

testRunner.test('should interpolate values correctly with linear easing', () => {
    const animation = new Animation('test', 0, 1000);
    animation.progress = 0.5;
    
    const value = animation.getValue(0, 100, 'linear');
    assertAlmostEqual(value, 50);
});

testRunner.test('should apply easeIn easing correctly', () => {
    const animation = new Animation('test', 0, 1000);
    animation.progress = 0.5;
    
    const value = animation.getValue(0, 100, 'easeIn');
    assertAlmostEqual(value, 25); // 0.5^2 * 100 = 25
});

testRunner.test('should apply easeOut easing correctly', () => {
    const animation = new Animation('test', 0, 1000);
    animation.progress = 0.5;
    
    const value = animation.getValue(0, 100, 'easeOut');
    assertAlmostEqual(value, 75); // 1 - (1-0.5)^2 = 0.75, so 75
});

testRunner.test('should apply easeInOut easing correctly', () => {
    const animation = new Animation('test', 0, 1000);
    
    // Test first half (should be easeIn-like)
    animation.progress = 0.25;
    const value1 = animation.getValue(0, 100, 'easeInOut');
    assertLessThan(value1, 25); // Should be less than linear
    
    // Test second half (should be easeOut-like)
    animation.progress = 0.75;
    const value2 = animation.getValue(0, 100, 'easeInOut');
    assertGreaterThan(value2, 75); // Should be greater than linear
});

testRunner.test('should apply bounce easing', () => {
    const animation = new Animation('test', 0, 1000);
    animation.progress = 1.0;
    
    const value = animation.getValue(0, 100, 'bounce');
    assertAlmostEqual(value, 100, 0.1); // Should end at target value
});

testRunner.test('should apply elastic easing', () => {
    const animation = new Animation('test', 0, 1000);
    animation.progress = 1.0;
    
    const value = animation.getValue(0, 100, 'elastic');
    assertAlmostEqual(value, 100, 0.1); // Should end at target value
});

// AnimationManager tests
testRunner.test('should create animation manager', () => {
    const manager = new AnimationManager();
    
    assertEqual(manager.getAnimationCount(), 0);
    assertEqual(manager.hasActiveAnimations(), false);
});

testRunner.test('should create and track animations', () => {
    const manager = new AnimationManager();
    
    const id1 = manager.createAnimation('test1', 1000);
    const id2 = manager.createAnimation('test2', 500);
    
    assertEqual(manager.getAnimationCount(), 2);
    assertEqual(manager.hasActiveAnimations(), true);
    
    const anim1 = manager.getAnimation(id1);
    const anim2 = manager.getAnimation(id2);
    
    assertEqual(anim1.type, 'test1');
    assertEqual(anim2.type, 'test2');
});

testRunner.test('should create move animation with correct properties', () => {
    const manager = new AnimationManager();
    
    const id = manager.createMoveAnimation(0, 0, 5, 10, 300);
    const animation = manager.getAnimation(id);
    
    assertEqual(animation.type, 'move');
    assertEqual(animation.duration, 300);
    assertEqual(animation.properties.fromX, 0);
    assertEqual(animation.properties.fromY, 0);
    assertEqual(animation.properties.toX, 5);
    assertEqual(animation.properties.toY, 10);
    assertEqual(animation.properties.easing, 'easeOut');
});

testRunner.test('should create clear animation with correct properties', () => {
    const manager = new AnimationManager();
    
    const id = manager.createClearAnimation(2, 3, 500);
    const animation = manager.getAnimation(id);
    
    assertEqual(animation.type, 'clear');
    assertEqual(animation.duration, 500);
    assertEqual(animation.properties.x, 2);
    assertEqual(animation.properties.y, 3);
    assertEqual(animation.properties.startAlpha, 1);
    assertEqual(animation.properties.endAlpha, 0);
    assertEqual(animation.properties.startScale, 1);
    assertEqual(animation.properties.endScale, 1.5);
});

testRunner.test('should create chain highlight animation with correct properties', () => {
    const manager = new AnimationManager();
    
    const id = manager.createChainHighlight(1, 2, 3, 800);
    const animation = manager.getAnimation(id);
    
    assertEqual(animation.type, 'chainHighlight');
    assertEqual(animation.duration, 800);
    assertEqual(animation.properties.x, 1);
    assertEqual(animation.properties.y, 2);
    assertEqual(animation.properties.chainLevel, 3);
    assertEqual(animation.properties.startRadius, 10);
    assertEqual(animation.properties.endRadius, 40);
    assert(animation.properties.color); // Should have a color
});

testRunner.test('should create drop animation with correct properties', () => {
    const manager = new AnimationManager();
    
    const id = manager.createDropAnimation(3, 0, 5, 200);
    const animation = manager.getAnimation(id);
    
    assertEqual(animation.type, 'drop');
    assertEqual(animation.duration, 200);
    assertEqual(animation.properties.x, 3);
    assertEqual(animation.properties.fromY, 0);
    assertEqual(animation.properties.toY, 5);
    assertEqual(animation.properties.easing, 'bounce');
});

testRunner.test('should create rotation animation with correct properties', () => {
    const manager = new AnimationManager();
    
    const id = manager.createRotationAnimation(50, 50, 20, 0, Math.PI, 200);
    const animation = manager.getAnimation(id);
    
    assertEqual(animation.type, 'rotation');
    assertEqual(animation.duration, 200);
    assertEqual(animation.properties.centerX, 50);
    assertEqual(animation.properties.centerY, 50);
    assertEqual(animation.properties.radius, 20);
    assertEqual(animation.properties.startAngle, 0);
    assertEqual(animation.properties.endAngle, Math.PI);
});

testRunner.test('should create screen shake animation with correct properties', () => {
    const manager = new AnimationManager();
    
    const id = manager.createScreenShake(8, 300);
    const animation = manager.getAnimation(id);
    
    assertEqual(animation.type, 'screenShake');
    assertEqual(animation.duration, 300);
    assertEqual(animation.properties.intensity, 8);
    assertEqual(animation.properties.easing, 'easeOut');
});

testRunner.test('should update animations and remove completed ones', () => {
    const manager = new AnimationManager();
    
    // Create animations with different durations
    const id1 = manager.createAnimation('short', 100);
    const id2 = manager.createAnimation('long', 1000);
    
    assertEqual(manager.getAnimationCount(), 2);
    
    // Update at time that should complete the short animation
    const currentTime = Date.now() + 150;
    const completed = manager.update(currentTime);
    
    assertEqual(completed.length, 1);
    assertEqual(completed[0], id1);
    assertEqual(manager.getAnimationCount(), 1);
    
    // Verify the long animation is still there
    const longAnim = manager.getAnimation(id2);
    assert(longAnim);
    assertEqual(longAnim.type, 'long');
});

testRunner.test('should get animations by type', () => {
    const manager = new AnimationManager();
    
    manager.createAnimation('typeA', 1000);
    manager.createAnimation('typeB', 1000);
    manager.createAnimation('typeA', 1000);
    
    const typeAAnimations = manager.getAnimationsByType('typeA');
    const typeBAnimations = manager.getAnimationsByType('typeB');
    const typeCAnimations = manager.getAnimationsByType('typeC');
    
    assertEqual(typeAAnimations.length, 2);
    assertEqual(typeBAnimations.length, 1);
    assertEqual(typeCAnimations.length, 0);
});

testRunner.test('should remove animation by ID', () => {
    const manager = new AnimationManager();
    
    const id = manager.createAnimation('test', 1000);
    assertEqual(manager.getAnimationCount(), 1);
    
    const removed = manager.removeAnimation(id);
    assertEqual(removed, true);
    assertEqual(manager.getAnimationCount(), 0);
    
    const removedAgain = manager.removeAnimation(id);
    assertEqual(removedAgain, false);
});

testRunner.test('should clear all animations', () => {
    const manager = new AnimationManager();
    
    manager.createAnimation('test1', 1000);
    manager.createAnimation('test2', 1000);
    manager.createAnimation('test3', 1000);
    
    assertEqual(manager.getAnimationCount(), 3);
    
    manager.clearAll();
    
    assertEqual(manager.getAnimationCount(), 0);
    assertEqual(manager.hasActiveAnimations(), false);
});

testRunner.test('should get move position correctly', () => {
    const manager = new AnimationManager();
    
    const id = manager.createMoveAnimation(0, 0, 10, 20, 1000);
    const animation = manager.getAnimation(id);
    animation.progress = 0.5;
    
    const position = manager.getMovePosition(animation);
    
    // Move animation uses 'easeOut' by default, so at 50% progress:
    // easeOut(0.5) = 1 - (1-0.5)^2 = 1 - 0.25 = 0.75
    assertAlmostEqual(position.x, 7.5); // 75% between 0 and 10
    assertAlmostEqual(position.y, 15); // 75% between 0 and 20
});

testRunner.test('should get clear values correctly', () => {
    const manager = new AnimationManager();
    
    const id = manager.createClearAnimation(0, 0, 1000);
    const animation = manager.getAnimation(id);
    animation.progress = 0.5;
    
    const values = manager.getClearValues(animation);
    
    // Clear animation uses 'easeIn' by default, so at 50% progress:
    // easeIn(0.5) = 0.5^2 = 0.25
    // For alpha: 1 + (0 - 1) * 0.25 = 1 - 0.25 = 0.75
    // For scale: 1 + (1.5 - 1) * 0.25 = 1 + 0.125 = 1.125
    assertAlmostEqual(values.alpha, 0.75);
    assertAlmostEqual(values.scale, 1.125);
});

testRunner.test('should get chain highlight values correctly', () => {
    const manager = new AnimationManager();
    
    const id = manager.createChainHighlight(0, 0, 1, 1000);
    const animation = manager.getAnimation(id);
    animation.progress = 0.5;
    
    const values = manager.getChainHighlightValues(animation);
    
    // Chain highlight uses 'easeOut' by default, so at 50% progress:
    // easeOut(0.5) = 1 - (1-0.5)^2 = 1 - 0.25 = 0.75
    // For radius: 10 + (40 - 10) * 0.75 = 10 + 22.5 = 32.5
    // For alpha: 0.8 + (0 - 0.8) * 0.75 = 0.8 - 0.6 = 0.2
    assertAlmostEqual(values.radius, 32.5);
    assertAlmostEqual(values.alpha, 0.2);
    assert(values.color); // Should have a color
});

testRunner.test('should get drop position correctly', () => {
    const manager = new AnimationManager();
    
    const id = manager.createDropAnimation(5, 0, 10, 1000);
    const animation = manager.getAnimation(id);
    animation.progress = 0.5;
    
    const position = manager.getDropPosition(animation);
    
    assertEqual(position.x, 5); // X should remain constant
    // Y should be between 0 and 10, but with bounce easing it's complex
    assert(position.y >= 0 && position.y <= 10);
});

testRunner.test('should get rotation values correctly', () => {
    const manager = new AnimationManager();
    
    const id = manager.createRotationAnimation(0, 0, 10, 0, Math.PI, 1000);
    const animation = manager.getAnimation(id);
    animation.progress = 0.5;
    
    const values = manager.getRotationValues(animation);
    
    assertAlmostEqual(values.angle, Math.PI / 2, 0.1); // 50% between 0 and PI
    assertAlmostEqual(values.x, 0, 0.1); // cos(PI/2) * 10 ≈ 0
    assertAlmostEqual(values.y, 10, 0.1); // sin(PI/2) * 10 ≈ 10
});

testRunner.test('should get screen shake offset', () => {
    const manager = new AnimationManager();
    
    const id = manager.createScreenShake(10, 1000);
    const animation = manager.getAnimation(id);
    animation.progress = 0.5;
    
    const offset = manager.getScreenShakeOffset(animation);
    
    // Offset should be within the intensity range
    assert(Math.abs(offset.x) <= 10);
    assert(Math.abs(offset.y) <= 10);
});

testRunner.test('should return null for wrong animation types', () => {
    const manager = new AnimationManager();
    
    const id = manager.createAnimation('wrongType', 1000);
    const animation = manager.getAnimation(id);
    
    assertEqual(manager.getMovePosition(animation), null);
    assertEqual(manager.getClearValues(animation), null);
    assertEqual(manager.getChainHighlightValues(animation), null);
    assertEqual(manager.getDropPosition(animation), null);
    assertEqual(manager.getRotationValues(animation), null);
});

// Export test runner for use in main test runner
export const AnimationTestRunner = testRunner;