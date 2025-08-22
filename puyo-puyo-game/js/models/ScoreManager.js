/**
 * ScoreManager - Handles scoring calculations and chain reactions
 * Manages basic scoring, chain multipliers, and zenkeshi bonuses
 */

import { Chain } from './Chain.js';

export class ScoreManager {
    // Base scoring constants
    static BASE_POINTS_PER_PUYO = 10;
    static CHAIN_BASE_MULTIPLIER = 1;
    static ZENKESHI_BONUS = 2100; // All-clear bonus points
    
    // Chain multiplier table (exponential scaling)
    static CHAIN_MULTIPLIERS = [
        1,    // 0 chain (no multiplier)
        1,    // 1 chain
        8,    // 2 chain
        16,   // 3 chain
        32,   // 4 chain
        64,   // 5 chain
        96,   // 6 chain
        128,  // 7 chain
        160,  // 8 chain
        192,  // 9 chain
        224,  // 10 chain
        256,  // 11 chain
        288,  // 12 chain
        320,  // 13+ chains
    ];

    /**
     * Create a new ScoreManager instance
     */
    constructor() {
        this.currentChain = 0;
        this.totalScore = 0;
        this.lastChainScore = 0;
        this.chainHistory = [];
        this.activeChain = new Chain();
        this.completedChains = [];
    }

    /**
     * Calculate base score for cleared puyos
     * @param {number} puyoCount - Number of puyos cleared
     * @param {number} groupCount - Number of separate groups cleared simultaneously
     * @returns {number} Base score before chain multiplier
     */
    calculateBaseScore(puyoCount, groupCount = 1) {
        if (puyoCount < 4) {
            return 0; // No score for groups smaller than 4
        }

        // Base score calculation: (number of puyos) * base points
        let baseScore = puyoCount * ScoreManager.BASE_POINTS_PER_PUYO;
        
        // Bonus for multiple groups cleared simultaneously
        if (groupCount > 1) {
            const groupBonus = (groupCount - 1) * 50; // 50 points per additional group
            baseScore += groupBonus;
        }
        
        // Bonus for larger groups (exponential bonus for big clears)
        if (puyoCount > 4) {
            const sizeBonus = Math.pow(puyoCount - 4, 1.5) * 20;
            baseScore += Math.floor(sizeBonus);
        }

        return Math.floor(baseScore);
    }

    /**
     * Get chain multiplier for current chain level
     * @param {number} chainLevel - Current chain level (0-based)
     * @returns {number} Multiplier for this chain level
     */
    getChainMultiplier(chainLevel) {
        if (chainLevel < 0) {
            return 1;
        }
        
        if (chainLevel < ScoreManager.CHAIN_MULTIPLIERS.length) {
            return ScoreManager.CHAIN_MULTIPLIERS[chainLevel];
        }
        
        // For chains beyond the table, use the last value
        return ScoreManager.CHAIN_MULTIPLIERS[ScoreManager.CHAIN_MULTIPLIERS.length - 1];
    }

    /**
     * Calculate score for a chain link
     * @param {number} puyoCount - Number of puyos cleared in this link
     * @param {number} groupCount - Number of groups cleared simultaneously
     * @param {number} chainLevel - Current chain level (0 for first clear, 1 for first chain, etc.)
     * @returns {number} Score for this chain link
     */
    calculateChainScore(puyoCount, groupCount = 1, chainLevel = 0) {
        const baseScore = this.calculateBaseScore(puyoCount, groupCount);
        const multiplier = this.getChainMultiplier(chainLevel);
        
        return Math.floor(baseScore * multiplier);
    }

    /**
     * Start a new chain sequence
     */
    startChain() {
        this.currentChain = 0;
        this.lastChainScore = 0;
        this.chainHistory = [];
        this.activeChain = new Chain();
        this.activeChain.start();
    }

    /**
     * Add a chain link to the current chain
     * @param {number} puyoCount - Number of puyos cleared
     * @param {number} groupCount - Number of groups cleared simultaneously
     * @param {Object} clearData - Additional clear data for chain tracking
     * @returns {Object} Chain link information
     */
    addChainLink(puyoCount, groupCount = 1, clearData = {}) {
        const chainScore = this.calculateChainScore(puyoCount, groupCount, this.currentChain);
        const baseScore = this.calculateBaseScore(puyoCount, groupCount);
        const multiplier = this.getChainMultiplier(this.currentChain);
        
        const chainLink = {
            chainLevel: this.currentChain,
            puyoCount,
            groupCount,
            baseScore,
            multiplier,
            score: chainScore,
            timestamp: Date.now()
        };
        
        // Add to chain history (legacy)
        this.chainHistory.push(chainLink);
        
        // Add to active chain
        if (this.activeChain.isActive()) {
            const enhancedClearData = {
                ...clearData,
                totalCleared: puyoCount,
                groupCount,
                baseScore
            };
            this.activeChain.addLink(enhancedClearData, chainScore, multiplier);
        }
        
        this.lastChainScore = chainScore;
        this.totalScore += chainScore;
        this.currentChain++;
        
        return chainLink;
    }

    /**
     * End the current chain sequence
     * @returns {Object} Chain summary
     */
    endChain() {
        let chainSummary = {
            totalChainLinks: this.currentChain,
            totalScore: this.chainHistory.reduce((sum, link) => sum + link.score, 0),
            chainHistory: [...this.chainHistory],
            maxChainLevel: this.currentChain - 1,
            averageMultiplier: this.currentChain > 0 ? 
                this.chainHistory.reduce((sum, link) => sum + link.multiplier, 0) / this.currentChain : 0
        };
        
        // Complete active chain and add to completed chains
        if (this.activeChain.isActive()) {
            const completedChain = this.activeChain.complete();
            this.completedChains.push(this.activeChain.clone());
            chainSummary = { ...chainSummary, ...completedChain };
        }
        
        // Reset chain state
        this.currentChain = 0;
        this.chainHistory = [];
        this.activeChain = new Chain();
        
        return chainSummary;
    }

    /**
     * Calculate zenkeshi (all-clear) bonus
     * @param {boolean} isZenkeshi - Whether this is an all-clear
     * @returns {number} Zenkeshi bonus points
     */
    calculateZenkeshiBonus(isZenkeshi) {
        return isZenkeshi ? ScoreManager.ZENKESHI_BONUS : 0;
    }

    /**
     * Process a complete clearing sequence (groups cleared + potential chain)
     * @param {Array<Array<Object>>} clearedGroups - Groups of puyos that were cleared
     * @param {boolean} isZenkeshi - Whether this resulted in an all-clear
     * @param {Object} additionalData - Additional data for chain tracking
     * @returns {Object} Complete scoring information
     */
    processClearingSequence(clearedGroups, isZenkeshi = false, additionalData = {}) {
        if (!clearedGroups || clearedGroups.length === 0) {
            return {
                score: 0,
                chainLink: null,
                zenkeshiBonus: 0,
                totalPuyosCleared: 0
            };
        }

        // Calculate total puyos cleared
        const totalPuyosCleared = clearedGroups.reduce((total, group) => total + group.length, 0);
        const groupCount = clearedGroups.length;

        // Prepare clear data for chain tracking
        const clearData = {
            groups: clearedGroups,
            groupCount,
            totalCleared: totalPuyosCleared,
            isZenkeshi,
            ...additionalData
        };

        // Add chain link
        const chainLink = this.addChainLink(totalPuyosCleared, groupCount, clearData);
        
        // Calculate zenkeshi bonus
        const zenkeshiBonus = this.calculateZenkeshiBonus(isZenkeshi);
        
        // Add zenkeshi bonus to total score
        if (zenkeshiBonus > 0) {
            this.totalScore += zenkeshiBonus;
        }

        return {
            score: chainLink.score + zenkeshiBonus,
            chainLink,
            zenkeshiBonus,
            totalPuyosCleared,
            groupCount,
            chainLevel: chainLink.chainLevel,
            activeChain: this.activeChain.isActive() ? this.activeChain.getDisplayInfo() : null
        };
    }

    /**
     * Get current chain level
     * @returns {number} Current chain level
     */
    getCurrentChainLevel() {
        return this.currentChain;
    }

    /**
     * Get total score
     * @returns {number} Total accumulated score
     */
    getTotalScore() {
        return this.totalScore;
    }

    /**
     * Get last chain score
     * @returns {number} Score from the last chain link
     */
    getLastChainScore() {
        return this.lastChainScore;
    }

    /**
     * Get chain history
     * @returns {Array<Object>} Array of chain link objects
     */
    getChainHistory() {
        return [...this.chainHistory];
    }

    /**
     * Check if currently in a chain
     * @returns {boolean} True if chain is active
     */
    isChainActive() {
        return this.currentChain > 0;
    }

    /**
     * Reset score manager to initial state
     */
    reset() {
        this.currentChain = 0;
        this.totalScore = 0;
        this.lastChainScore = 0;
        this.chainHistory = [];
        this.activeChain = new Chain();
        this.completedChains = [];
    }

    /**
     * Get scoring statistics
     * @returns {Object} Scoring statistics
     */
    getStatistics() {
        const activeChainStats = this.activeChain.isActive() ? this.activeChain.getStatistics() : null;
        const allCompletedChains = [...this.completedChains];
        
        return {
            totalScore: this.totalScore,
            currentChainLevel: this.currentChain,
            lastChainScore: this.lastChainScore,
            totalChainLinks: this.chainHistory.length,
            maxChainLevel: this.chainHistory.length > 0 ? 
                Math.max(...this.chainHistory.map(link => link.chainLevel)) : 0,
            averageChainScore: this.chainHistory.length > 0 ?
                this.chainHistory.reduce((sum, link) => sum + link.score, 0) / this.chainHistory.length : 0,
            activeChain: activeChainStats,
            completedChainsCount: allCompletedChains.length,
            longestChain: allCompletedChains.length > 0 ? 
                Math.max(...allCompletedChains.map(chain => chain.getMaxLevel())) : 0
        };
    }

    /**
     * Create a copy of this score manager
     * @returns {ScoreManager} New ScoreManager instance with same state
     */
    clone() {
        const cloned = new ScoreManager();
        cloned.currentChain = this.currentChain;
        cloned.totalScore = this.totalScore;
        cloned.lastChainScore = this.lastChainScore;
        cloned.chainHistory = this.chainHistory.map(link => ({ ...link }));
        cloned.activeChain = this.activeChain.clone();
        cloned.completedChains = this.completedChains.map(chain => chain.clone());
        return cloned;
    }

    /**
     * Convert to JSON representation
     * @returns {Object} JSON object
     */
    toJSON() {
        return {
            currentChain: this.currentChain,
            totalScore: this.totalScore,
            lastChainScore: this.lastChainScore,
            chainHistory: this.chainHistory,
            activeChain: this.activeChain.toJSON(),
            completedChains: this.completedChains.map(chain => chain.toJSON())
        };
    }

    /**
     * Get active chain information
     * @returns {Object|null} Active chain display info or null
     */
    getActiveChainInfo() {
        return this.activeChain.isActive() ? this.activeChain.getDisplayInfo() : null;
    }

    /**
     * Get active chain instance
     * @returns {Chain} Active chain instance
     */
    getActiveChain() {
        return this.activeChain;
    }

    /**
     * Get all completed chains
     * @returns {Array<Chain>} Array of completed chains
     */
    getCompletedChains() {
        return [...this.completedChains];
    }

    /**
     * Get chain display name for current level
     * @returns {string} Chain display name
     */
    getCurrentChainDisplayName() {
        return Chain.getChainLevelName(this.currentChain);
    }

    /**
     * Get chain color for current level
     * @returns {string} Chain color code
     */
    getCurrentChainColor() {
        return Chain.getChainLevelColor(this.currentChain);
    }

    /**
     * Check if chain reaction is possible (for UI hints)
     * @param {Object} fieldState - Current field state
     * @returns {boolean} True if chain reaction might occur
     */
    isChainReactionPossible(fieldState) {
        // This would need field analysis - placeholder for now
        return this.activeChain.isActive();
    }

    /**
     * Create ScoreManager from JSON
     * @param {Object} json - JSON object
     * @returns {ScoreManager} New ScoreManager instance
     */
    static fromJSON(json) {
        const scoreManager = new ScoreManager();
        scoreManager.currentChain = json.currentChain || 0;
        scoreManager.totalScore = json.totalScore || 0;
        scoreManager.lastChainScore = json.lastChainScore || 0;
        scoreManager.chainHistory = json.chainHistory || [];
        scoreManager.activeChain = json.activeChain ? Chain.fromJSON(json.activeChain) : new Chain();
        scoreManager.completedChains = (json.completedChains || []).map(chainData => Chain.fromJSON(chainData));
        return scoreManager;
    }
}