/**
 * Chain - Tracks multi-stage clearing sequences and chain reactions
 * Manages chain detection, scoring, and visual feedback for chain reactions
 */

export class Chain {
    // Chain state constants
    static STATE = {
        INACTIVE: 'inactive',
        ACTIVE: 'active',
        PROCESSING: 'processing',
        COMPLETED: 'completed'
    };

    // Chain timing constants (in milliseconds)
    static TIMING = {
        CLEAR_DELAY: 300,      // Delay before clearing puyos
        GRAVITY_DELAY: 200,    // Delay for gravity to settle
        CHAIN_DELAY: 400,      // Delay between chain links
        ANIMATION_DURATION: 500 // Duration of chain animations
    };

    /**
     * Create a new Chain instance
     */
    constructor() {
        this.reset();
    }

    /**
     * Reset chain to initial state
     */
    reset() {
        this.state = Chain.STATE.INACTIVE;
        this.links = [];
        this.currentLink = 0;
        this.totalScore = 0;
        this.totalPuyosCleared = 0;
        this.startTime = 0;
        this.endTime = 0;
        this.isProcessing = false;
        this.pendingClears = [];
        this.chainMultiplier = 1;
        this.maxChainLevel = 0;
    }

    /**
     * Start a new chain sequence
     * @param {number} timestamp - Start timestamp
     */
    start(timestamp = Date.now()) {
        this.reset();
        this.state = Chain.STATE.ACTIVE;
        this.startTime = timestamp;
        this.isProcessing = false;
    }

    /**
     * Add a chain link to the sequence
     * @param {Object} clearData - Data about the cleared groups
     * @param {number} score - Score for this link
     * @param {number} multiplier - Chain multiplier applied
     * @param {number} timestamp - Timestamp of this link
     * @returns {Object} Chain link information
     */
    addLink(clearData, score, multiplier, timestamp = Date.now()) {
        if (this.state !== Chain.STATE.ACTIVE) {
            throw new Error('Cannot add link to inactive chain');
        }

        const link = {
            linkNumber: this.currentLink,
            chainLevel: this.currentLink, // 0-based chain level
            clearedGroups: clearData.groups || [],
            groupCount: clearData.groupCount || 0,
            puyosCleared: clearData.totalCleared || 0,
            baseScore: clearData.baseScore || 0,
            multiplier: multiplier,
            score: score,
            timestamp: timestamp,
            duration: this.links.length > 0 ? timestamp - this.links[this.links.length - 1].timestamp : 0,
            positions: clearData.clearedPositions || []
        };

        this.links.push(link);
        this.totalScore += score;
        this.totalPuyosCleared += link.puyosCleared;
        this.currentLink++;
        this.maxChainLevel = Math.max(this.maxChainLevel, link.chainLevel);

        return link;
    }

    /**
     * Complete the chain sequence
     * @param {number} timestamp - End timestamp
     * @returns {Object} Chain summary
     */
    complete(timestamp = Date.now()) {
        this.state = Chain.STATE.COMPLETED;
        this.endTime = timestamp;
        this.isProcessing = false;

        const summary = {
            totalLinks: this.links.length,
            maxChainLevel: this.maxChainLevel,
            totalScore: this.totalScore,
            totalPuyosCleared: this.totalPuyosCleared,
            duration: this.endTime - this.startTime,
            averageMultiplier: this.links.length > 0 ? 
                this.links.reduce((sum, link) => sum + link.multiplier, 0) / this.links.length : 0,
            links: [...this.links],
            startTime: this.startTime,
            endTime: this.endTime
        };

        return summary;
    }

    /**
     * Check if chain is currently active
     * @returns {boolean} True if chain is active
     */
    isActive() {
        return this.state === Chain.STATE.ACTIVE;
    }

    /**
     * Check if chain is completed
     * @returns {boolean} True if chain is completed
     */
    isCompleted() {
        return this.state === Chain.STATE.COMPLETED;
    }

    /**
     * Check if chain is inactive
     * @returns {boolean} True if chain is inactive
     */
    isInactive() {
        return this.state === Chain.STATE.INACTIVE;
    }

    /**
     * Get current chain level (0-based)
     * @returns {number} Current chain level
     */
    getCurrentLevel() {
        return this.currentLink;
    }

    /**
     * Get maximum chain level achieved
     * @returns {number} Maximum chain level
     */
    getMaxLevel() {
        return this.maxChainLevel;
    }

    /**
     * Get total number of chain links
     * @returns {number} Number of chain links
     */
    getLinkCount() {
        return this.links.length;
    }

    /**
     * Get chain link by index
     * @param {number} index - Link index
     * @returns {Object|null} Chain link or null if not found
     */
    getLink(index) {
        return this.links[index] || null;
    }

    /**
     * Get the last chain link
     * @returns {Object|null} Last chain link or null if no links
     */
    getLastLink() {
        return this.links.length > 0 ? this.links[this.links.length - 1] : null;
    }

    /**
     * Get all chain links
     * @returns {Array<Object>} Array of chain links
     */
    getAllLinks() {
        return [...this.links];
    }

    /**
     * Get chain statistics
     * @returns {Object} Chain statistics
     */
    getStatistics() {
        const duration = this.isCompleted() ? this.endTime - this.startTime : Date.now() - this.startTime;
        
        return {
            state: this.state,
            totalLinks: this.links.length,
            currentLevel: this.currentLink,
            maxLevel: this.maxChainLevel,
            totalScore: this.totalScore,
            totalPuyosCleared: this.totalPuyosCleared,
            duration: duration,
            averageScore: this.links.length > 0 ? this.totalScore / this.links.length : 0,
            averageMultiplier: this.links.length > 0 ? 
                this.links.reduce((sum, link) => sum + link.multiplier, 0) / this.links.length : 0,
            puyosPerSecond: duration > 0 ? (this.totalPuyosCleared / duration) * 1000 : 0,
            isActive: this.isActive(),
            isCompleted: this.isCompleted()
        };
    }

    /**
     * Set processing state
     * @param {boolean} processing - Whether chain is currently processing
     */
    setProcessing(processing) {
        this.isProcessing = processing;
        if (processing && this.state === Chain.STATE.ACTIVE) {
            this.state = Chain.STATE.PROCESSING;
        } else if (!processing && this.state === Chain.STATE.PROCESSING) {
            this.state = Chain.STATE.ACTIVE;
        }
    }

    /**
     * Check if chain is currently processing
     * @returns {boolean} True if processing
     */
    isCurrentlyProcessing() {
        return this.isProcessing;
    }

    /**
     * Add pending clear data for processing
     * @param {Object} clearData - Clear data to process later
     */
    addPendingClear(clearData) {
        this.pendingClears.push({
            ...clearData,
            timestamp: Date.now()
        });
    }

    /**
     * Get and clear pending clears
     * @returns {Array<Object>} Array of pending clear data
     */
    getPendingClears() {
        const pending = [...this.pendingClears];
        this.pendingClears = [];
        return pending;
    }

    /**
     * Check if there are pending clears
     * @returns {boolean} True if there are pending clears
     */
    hasPendingClears() {
        return this.pendingClears.length > 0;
    }

    /**
     * Calculate chain display information for UI
     * @returns {Object} Display information
     */
    getDisplayInfo() {
        const lastLink = this.getLastLink();
        const stats = this.getStatistics();
        
        return {
            chainLevel: this.maxChainLevel,
            currentScore: this.totalScore,
            lastLinkScore: lastLink ? lastLink.score : 0,
            lastMultiplier: lastLink ? lastLink.multiplier : 1,
            totalLinks: this.links.length,
            isActive: this.isActive(),
            isProcessing: this.isCurrentlyProcessing(),
            duration: stats.duration,
            puyosCleared: this.totalPuyosCleared,
            averageMultiplier: stats.averageMultiplier
        };
    }

    /**
     * Get chain timing information for animations
     * @returns {Object} Timing information
     */
    getTimingInfo() {
        return {
            clearDelay: Chain.TIMING.CLEAR_DELAY,
            gravityDelay: Chain.TIMING.GRAVITY_DELAY,
            chainDelay: Chain.TIMING.CHAIN_DELAY,
            animationDuration: Chain.TIMING.ANIMATION_DURATION,
            totalDelay: Chain.TIMING.CLEAR_DELAY + Chain.TIMING.GRAVITY_DELAY + Chain.TIMING.CHAIN_DELAY
        };
    }

    /**
     * Create a copy of this chain
     * @returns {Chain} New Chain instance with same state
     */
    clone() {
        const cloned = new Chain();
        cloned.state = this.state;
        cloned.links = this.links.map(link => ({ ...link }));
        cloned.currentLink = this.currentLink;
        cloned.totalScore = this.totalScore;
        cloned.totalPuyosCleared = this.totalPuyosCleared;
        cloned.startTime = this.startTime;
        cloned.endTime = this.endTime;
        cloned.isProcessing = this.isProcessing;
        cloned.pendingClears = this.pendingClears.map(clear => ({ ...clear }));
        cloned.chainMultiplier = this.chainMultiplier;
        cloned.maxChainLevel = this.maxChainLevel;
        return cloned;
    }

    /**
     * Convert chain to JSON representation
     * @returns {Object} JSON object
     */
    toJSON() {
        return {
            state: this.state,
            links: this.links,
            currentLink: this.currentLink,
            totalScore: this.totalScore,
            totalPuyosCleared: this.totalPuyosCleared,
            startTime: this.startTime,
            endTime: this.endTime,
            isProcessing: this.isProcessing,
            pendingClears: this.pendingClears,
            chainMultiplier: this.chainMultiplier,
            maxChainLevel: this.maxChainLevel
        };
    }

    /**
     * Create Chain from JSON
     * @param {Object} json - JSON object
     * @returns {Chain} New Chain instance
     */
    static fromJSON(json) {
        const chain = new Chain();
        chain.state = json.state || Chain.STATE.INACTIVE;
        chain.links = json.links || [];
        chain.currentLink = json.currentLink || 0;
        chain.totalScore = json.totalScore || 0;
        chain.totalPuyosCleared = json.totalPuyosCleared || 0;
        chain.startTime = json.startTime || 0;
        chain.endTime = json.endTime || 0;
        chain.isProcessing = json.isProcessing || false;
        chain.pendingClears = json.pendingClears || [];
        chain.chainMultiplier = json.chainMultiplier || 1;
        chain.maxChainLevel = json.maxChainLevel || 0;
        return chain;
    }

    /**
     * Get chain level name for display
     * @param {number} level - Chain level
     * @returns {string} Display name
     */
    static getChainLevelName(level) {
        if (level === 0) return 'Clear';
        if (level === 1) return '1 Chain';
        if (level >= 2) return `${level} Chain`;
        return 'Chain';
    }

    /**
     * Get chain level color for display
     * @param {number} level - Chain level
     * @returns {string} Color code
     */
    static getChainLevelColor(level) {
        const colors = [
            '#FFFFFF', // 0 - White (no chain)
            '#FFFF00', // 1 - Yellow
            '#FF8000', // 2 - Orange
            '#FF0000', // 3 - Red
            '#FF00FF', // 4 - Magenta
            '#8000FF', // 5 - Purple
            '#0080FF', // 6 - Blue
            '#00FFFF', // 7 - Cyan
            '#00FF00', // 8 - Green
            '#80FF00', // 9 - Lime
            '#FFFF80', // 10+ - Light Yellow
        ];
        
        return colors[Math.min(level, colors.length - 1)];
    }
}