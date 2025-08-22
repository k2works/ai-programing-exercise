/**
 * Test runner for all model tests
 * Runs all unit tests for the core data models
 */

import { PuyoTestRunner } from './models/Puyo.test.js';
import { PuyoPairTestRunner } from './models/PuyoPair.test.js';
import { GameStateTestRunner } from './models/GameState.test.js';
import { FieldManagerTestRunner } from './models/FieldManager.test.js';
import { PuyoManagerTestRunner } from './models/PuyoManager.test.js';

/**
 * Run all model tests
 */
export async function runAllTests() {
    console.log('='.repeat(50));
    console.log('Running All Model Tests');
    console.log('='.repeat(50));
    
    const allResults = [];
    
    try {
        // Run Puyo tests
        console.log('\n' + '='.repeat(30));
        console.log('PUYO TESTS');
        console.log('='.repeat(30));
        const puyoResults = await PuyoTestRunner.run();
        allResults.push(...puyoResults);
        
        // Run PuyoPair tests
        console.log('\n' + '='.repeat(30));
        console.log('PUYO PAIR TESTS');
        console.log('='.repeat(30));
        const puyoPairResults = await PuyoPairTestRunner.run();
        allResults.push(...puyoPairResults);
        
        // Run GameState tests
        console.log('\n' + '='.repeat(30));
        console.log('GAME STATE TESTS');
        console.log('='.repeat(30));
        const gameStateResults = await GameStateTestRunner.run();
        allResults.push(...gameStateResults);
        
        // Run FieldManager tests
        console.log('\n' + '='.repeat(30));
        console.log('FIELD MANAGER TESTS');
        console.log('='.repeat(30));
        const fieldManagerResults = await FieldManagerTestRunner.run();
        allResults.push(...fieldManagerResults);
        
        // Run PuyoManager tests
        console.log('\n' + '='.repeat(30));
        console.log('PUYO MANAGER TESTS');
        console.log('='.repeat(30));
        const puyoManagerResults = await PuyoManagerTestRunner.run();
        allResults.push(...puyoManagerResults);
        
    } catch (error) {
        console.error('Error running tests:', error);
        allResults.push({
            name: 'Test Runner Error',
            status: 'FAIL',
            error: error.message
        });
    }
    
    // Print overall summary
    console.log('\n' + '='.repeat(50));
    console.log('OVERALL TEST SUMMARY');
    console.log('='.repeat(50));
    
    const totalPassed = allResults.filter(r => r.status === 'PASS').length;
    const totalFailed = allResults.filter(r => r.status === 'FAIL').length;
    const totalTests = allResults.length;
    
    console.log(`Total Tests: ${totalTests}`);
    console.log(`Passed: ${totalPassed}`);
    console.log(`Failed: ${totalFailed}`);
    console.log(`Success Rate: ${totalTests > 0 ? ((totalPassed / totalTests) * 100).toFixed(1) : 0}%`);
    
    if (totalFailed > 0) {
        console.log('\nFailed Tests:');
        allResults.filter(r => r.status === 'FAIL').forEach((result, index) => {
            console.log(`${index + 1}. ${result.name}: ${result.error}`);
        });
    } else {
        console.log('\n🎉 All tests passed!');
    }
    
    console.log('='.repeat(50));
    
    return {
        totalTests,
        totalPassed,
        totalFailed,
        results: allResults,
        success: totalFailed === 0
    };
}

// Auto-run tests if this module is loaded directly
if (typeof window !== 'undefined') {
    // Browser environment - make test runner available globally
    window.runAllTests = runAllTests;
    
    // Add a button to run tests if we're in a test page
    document.addEventListener('DOMContentLoaded', () => {
        // Check if we're in a test environment
        if (document.title.includes('テスト') || document.title.includes('Test')) {
            const button = document.createElement('button');
            button.textContent = 'Run All Model Tests';
            button.style.cssText = `
                position: fixed;
                top: 10px;
                right: 10px;
                z-index: 1000;
                padding: 10px 20px;
                background: #007bff;
                color: white;
                border: none;
                border-radius: 5px;
                cursor: pointer;
                font-weight: bold;
            `;
            
            button.addEventListener('click', async () => {
                button.disabled = true;
                button.textContent = 'Running Tests...';
                
                try {
                    await runAllTests();
                } catch (error) {
                    console.error('Failed to run tests:', error);
                } finally {
                    button.disabled = false;
                    button.textContent = 'Run All Model Tests';
                }
            });
            
            document.body.appendChild(button);
        }
    });
}