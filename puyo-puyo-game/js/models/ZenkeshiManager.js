/**
 * ZenkeshiManager - Handles all-clear (zenkeshi) detection and bonus system
 * Manages zenkeshi detection, bonus calculation, and special visual feedback
 */

export class ZenkeshiManager {
    // Zenkeshi bonus constants
    static BONUS_POINTS = 2100;
    static BONUS_MULTIPLIER = 1.0; // Additional multiplier for zenkeshi
    
    // Visual feedback constants
    static VISUAL_EFFECTS = {
        FLASH_DURATION: 1000,      // Duration of screen flash effect
        FLASH_COLOR: '#FFD700',    // Gold color for flash
        TEXT_DURATION: 2000,       // Duration of zenkeshi text display
        TEXT_COLOR: '#FFD700',     // Gold color for text
        PARTICLE_COUNT: 50,        // Number of celebration particles
        PARTICLE_DURATION: 3000    // Duration of particle effect
    };

    /**
     * Create a new ZenkeshiManager instance
     */
    constructor() {
        this.reset();
    }

    /**
     * Reset zenkeshi manager to initial state
     */
    reset() {
        this.totalZenkeshiCount = 0;
        this.currentGameZenkeshiCount = 0;
        this.lastZenkeshiTime = 0;
        this.zenkeshiHistory = [];
        this.isZenkeshiActive = false;
        this.zenkeshiEffectStartTime = 0;
        this.consecutiveZenkeshi = 0;
        this.maxConsecutiveZenkeshi = 0;
    }

    /**
     * Detect if field state represents a zenkeshi (all-clear)
     * @param {FieldManager} fieldManager - Field manager to check
     * @returns {boolean} True if field is completely empty
     */
    detectZenkeshi(fieldManager) {
        if (!fieldManager) {
            return false;
        }

        // Check if field is completely empty
        const allPuyo = fieldManager.getAllPuyo();
        return allPuyo.length === 0;
    }

    /**
     * Detect zenkeshi from field array
     * @param {Array<Array>} field - 2D field array
     * @returns {boolean} True if field is completely empty
     */
    detectZenkeshiFromField(field) {
        if (!field || !Array.isArray(field)) {
            return false;
        }

        for (let y = 0; y < field.length; y++) {
            for (let x = 0; x < field[y].length; x++) {
                if (field[y][x] !== null) {
                    return false;
                }
            }
        }

        return true;
    }

    /**
     * Process a zenkeshi occurrence
     * @param {Object} clearData - Data about the clear that caused zenkeshi
     * @param {number} baseScore - Base score before zenkeshi bonus
     * @param {number} timestamp - Timestamp of zenkeshi occurrence
     * @returns {Object} Zenkeshi processing results
     */
    processZenkeshi(clearData, baseScore = 0, timestamp = Date.now()) {
        this.totalZenkeshiCount++;
        this.currentGameZenkeshiCount++;
        this.consecutiveZenkeshi++;
        this.lastZenkeshiTime = timestamp;
        this.isZenkeshiActive = true;
        this.zenkeshiEffectStartTime = timestamp;

        // Update max consecutive zenkeshi
        if (this.consecutiveZenkeshi > this.maxConsecutiveZenkeshi) {
            this.maxConsecutiveZenkeshi = this.consecutiveZenkeshi;
        }

        // Calculate bonus points
        const bonusPoints = this.calculateZenkeshiBonus(this.consecutiveZenkeshi);
        
        // Create zenkeshi record
        const zenkeshiRecord = {
            timestamp,
            gameZenkeshiNumber: this.currentGameZenkeshiCount,
            totalZenkeshiNumber: this.totalZenkeshiCount,
            consecutiveCount: this.consecutiveZenkeshi,
            baseScore,
            bonusPoints,
            totalScore: baseScore + bonusPoints,
            clearData: { ...clearData },
            effectDuration: ZenkeshiManager.VISUAL_EFFECTS.TEXT_DURATION
        };

        this.zenkeshiHistory.push(zenkeshiRecord);

        return {
            bonusPoints,
            totalScore: baseScore + bonusPoints,
            consecutiveCount: this.consecutiveZenkeshi,
            zenkeshiNumber: this.currentGameZenkeshiCount,
            record: zenkeshiRecord,
            visualEffects: this.getVisualEffectData()
        };
    }

    /**
     * Calculate zenkeshi bonus points based on consecutive count
     * @param {number} consecutiveCount - Number of consecutive zenkeshi
     * @returns {number} Bonus points
     */
    calculateZenkeshiBonus(consecutiveCount = 1) {
        let bonus = ZenkeshiManager.BONUS_POINTS;
        
        // Bonus scaling for consecutive zenkeshi
        if (consecutiveCount > 1) {
            const consecutiveMultiplier = 1 + (consecutiveCount - 1) * 0.5; // 50% bonus per consecutive
            bonus = Math.floor(bonus * consecutiveMultiplier);
        }

        return bonus;
    }

    /**
     * Reset consecutive zenkeshi count (called when chain ends without zenkeshi)
     */
    resetConsecutiveZenkeshi() {
        this.consecutiveZenkeshi = 0;
    }

    /**
     * Check if zenkeshi effect is currently active
     * @param {number} currentTime - Current timestamp
     * @returns {boolean} True if effect is active
     */
    isEffectActive(currentTime = Date.now()) {
        if (!this.isZenkeshiActive) {
            return false;
        }

        const elapsed = currentTime - this.zenkeshiEffectStartTime;
        return elapsed < ZenkeshiManager.VISUAL_EFFECTS.TEXT_DURATION;
    }

    /**
     * Update zenkeshi effect state
     * @param {number} currentTime - Current timestamp
     */
    updateEffect(currentTime = Date.now()) {
        if (this.isZenkeshiActive && !this.isEffectActive(currentTime)) {
            this.isZenkeshiActive = false;
        }
    }

    /**
     * Get visual effect data for rendering
     * @returns {Object} Visual effect configuration
     */
    getVisualEffectData() {
        return {
            flashDuration: ZenkeshiManager.VISUAL_EFFECTS.FLASH_DURATION,
            flashColor: ZenkeshiManager.VISUAL_EFFECTS.FLASH_COLOR,
            textDuration: ZenkeshiManager.VISUAL_EFFECTS.TEXT_DURATION,
            textColor: ZenkeshiManager.VISUAL_EFFECTS.TEXT_COLOR,
            particleCount: ZenkeshiManager.VISUAL_EFFECTS.PARTICLE_COUNT,
            particleDuration: ZenkeshiManager.VISUAL_EFFECTS.PARTICLE_DURATION,
            startTime: this.zenkeshiEffectStartTime,
            isActive: this.isZenkeshiActive
        };
    }

    /**
     * Get zenkeshi display information for UI
     * @returns {Object} Display information
     */
    getDisplayInfo() {
        const lastZenkeshi = this.getLastZenkeshi();
        
        return {
            totalCount: this.totalZenkeshiCount,
            currentGameCount: this.currentGameZenkeshiCount,
            consecutiveCount: this.consecutiveZenkeshi,
            maxConsecutiveCount: this.maxConsecutiveZenkeshi,
            lastZenkeshiTime: this.lastZenkeshiTime,
            isEffectActive: this.isZenkeshiActive,
            lastBonusPoints: lastZenkeshi ? lastZenkeshi.bonusPoints : 0,
            nextBonusPoints: this.calculateZenkeshiBonus(this.consecutiveZenkeshi + 1),
            effectData: this.getVisualEffectData()
        };
    }

    /**
     * Get the last zenkeshi record
     * @returns {Object|null} Last zenkeshi record or null
     */
    getLastZenkeshi() {
        return this.zenkeshiHistory.length > 0 ? 
            this.zenkeshiHistory[this.zenkeshiHistory.length - 1] : null;
    }

    /**
     * Get all zenkeshi records
     * @returns {Array<Object>} Array of zenkeshi records
     */
    getAllZenkeshi() {
        return [...this.zenkeshiHistory];
    }

    /**
     * Get zenkeshi records for current game
     * @returns {Array<Object>} Array of current game zenkeshi records
     */
    getCurrentGameZenkeshi() {
        // This would need game session tracking - for now return all
        return [...this.zenkeshiHistory];
    }

    /**
     * Get zenkeshi statistics
     * @returns {Object} Zenkeshi statistics
     */
    getStatistics() {
        const totalBonusPoints = this.zenkeshiHistory.reduce((sum, record) => sum + record.bonusPoints, 0);
        const averageBonusPoints = this.zenkeshiHistory.length > 0 ? 
            totalBonusPoints / this.zenkeshiHistory.length : 0;

        return {
            totalZenkeshi: this.totalZenkeshiCount,
            currentGameZenkeshi: this.currentGameZenkeshiCount,
            consecutiveZenkeshi: this.consecutiveZenkeshi,
            maxConsecutiveZenkeshi: this.maxConsecutiveZenkeshi,
            totalBonusPoints,
            averageBonusPoints,
            lastZenkeshiTime: this.lastZenkeshiTime,
            zenkeshiFrequency: this.zenkeshiHistory.length > 1 ? 
                (this.lastZenkeshiTime - this.zenkeshiHistory[0].timestamp) / this.zenkeshiHistory.length : 0
        };
    }

    /**
     * Get zenkeshi achievement data
     * @returns {Object} Achievement information
     */
    getAchievements() {
        return {
            firstZenkeshi: this.totalZenkeshiCount >= 1,
            zenkeshiMaster: this.totalZenkeshiCount >= 10,
            consecutiveZenkeshi: this.maxConsecutiveZenkeshi >= 2,
            zenkeshiChain: this.maxConsecutiveZenkeshi >= 3,
            perfectClear: this.consecutiveZenkeshi >= 5,
            achievements: [
                { name: 'First Clear', description: 'Achieve your first zenkeshi', unlocked: this.totalZenkeshiCount >= 1 },
                { name: 'Clear Master', description: 'Achieve 10 zenkeshi', unlocked: this.totalZenkeshiCount >= 10 },
                { name: 'Double Clear', description: 'Achieve 2 consecutive zenkeshi', unlocked: this.maxConsecutiveZenkeshi >= 2 },
                { name: 'Triple Clear', description: 'Achieve 3 consecutive zenkeshi', unlocked: this.maxConsecutiveZenkeshi >= 3 },
                { name: 'Perfect Clear', description: 'Achieve 5 consecutive zenkeshi', unlocked: this.maxConsecutiveZenkeshi >= 5 }
            ]
        };
    }

    /**
     * Start new game (reset current game counters)
     */
    startNewGame() {
        this.currentGameZenkeshiCount = 0;
        this.consecutiveZenkeshi = 0;
        this.isZenkeshiActive = false;
        this.zenkeshiEffectStartTime = 0;
    }

    /**
     * Create a copy of this zenkeshi manager
     * @returns {ZenkeshiManager} New ZenkeshiManager instance with same state
     */
    clone() {
        const cloned = new ZenkeshiManager();
        cloned.totalZenkeshiCount = this.totalZenkeshiCount;
        cloned.currentGameZenkeshiCount = this.currentGameZenkeshiCount;
        cloned.lastZenkeshiTime = this.lastZenkeshiTime;
        cloned.zenkeshiHistory = this.zenkeshiHistory.map(record => ({ ...record }));
        cloned.isZenkeshiActive = this.isZenkeshiActive;
        cloned.zenkeshiEffectStartTime = this.zenkeshiEffectStartTime;
        cloned.consecutiveZenkeshi = this.consecutiveZenkeshi;
        cloned.maxConsecutiveZenkeshi = this.maxConsecutiveZenkeshi;
        return cloned;
    }

    /**
     * Convert to JSON representation
     * @returns {Object} JSON object
     */
    toJSON() {
        return {
            totalZenkeshiCount: this.totalZenkeshiCount,
            currentGameZenkeshiCount: this.currentGameZenkeshiCount,
            lastZenkeshiTime: this.lastZenkeshiTime,
            zenkeshiHistory: this.zenkeshiHistory,
            isZenkeshiActive: this.isZenkeshiActive,
            zenkeshiEffectStartTime: this.zenkeshiEffectStartTime,
            consecutiveZenkeshi: this.consecutiveZenkeshi,
            maxConsecutiveZenkeshi: this.maxConsecutiveZenkeshi
        };
    }

    /**
     * Create ZenkeshiManager from JSON
     * @param {Object} json - JSON object
     * @returns {ZenkeshiManager} New ZenkeshiManager instance
     */
    static fromJSON(json) {
        const manager = new ZenkeshiManager();
        manager.totalZenkeshiCount = json.totalZenkeshiCount || 0;
        manager.currentGameZenkeshiCount = json.currentGameZenkeshiCount || 0;
        manager.lastZenkeshiTime = json.lastZenkeshiTime || 0;
        manager.zenkeshiHistory = json.zenkeshiHistory || [];
        manager.isZenkeshiActive = json.isZenkeshiActive || false;
        manager.zenkeshiEffectStartTime = json.zenkeshiEffectStartTime || 0;
        manager.consecutiveZenkeshi = json.consecutiveZenkeshi || 0;
        manager.maxConsecutiveZenkeshi = json.maxConsecutiveZenkeshi || 0;
        return manager;
    }

    /**
     * Get zenkeshi celebration message
     * @param {number} consecutiveCount - Number of consecutive zenkeshi
     * @returns {string} Celebration message
     */
    static getCelebrationMessage(consecutiveCount = 1) {
        const messages = [
            'ZENKESHI!',           // 1
            'DOUBLE ZENKESHI!',    // 2
            'TRIPLE ZENKESHI!',    // 3
            'QUAD ZENKESHI!',      // 4
            'PERFECT CLEAR!',      // 5+
        ];
        
        const index = Math.min(consecutiveCount - 1, messages.length - 1);
        return messages[Math.max(0, index)];
    }

    /**
     * Get zenkeshi sound effect name
     * @param {number} consecutiveCount - Number of consecutive zenkeshi
     * @returns {string} Sound effect name
     */
    static getSoundEffect(consecutiveCount = 1) {
        if (consecutiveCount === 1) return 'zenkeshi';
        if (consecutiveCount === 2) return 'double_zenkeshi';
        if (consecutiveCount === 3) return 'triple_zenkeshi';
        if (consecutiveCount >= 4) return 'perfect_clear';
        return 'zenkeshi';
    }
}