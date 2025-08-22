/**
 * Animation system for smooth visual effects
 * Handles puyo movement, clearing effects, and chain reaction highlights
 */

export class Animation {
    constructor(type, startTime, duration, properties = {}) {
        this.type = type;
        this.startTime = startTime;
        this.duration = duration;
        this.properties = properties;
        this.isComplete = false;
        this.progress = 0;
    }

    /**
     * Update animation progress
     */
    update(currentTime) {
        const elapsed = currentTime - this.startTime;
        this.progress = Math.min(elapsed / this.duration, 1);
        
        if (this.progress >= 1) {
            this.isComplete = true;
        }
        
        return this.progress;
    }

    /**
     * Get current animation value using easing function
     */
    getValue(startValue, endValue, easingFunction = 'linear') {
        const easedProgress = this.applyEasing(this.progress, easingFunction);
        return startValue + (endValue - startValue) * easedProgress;
    }

    /**
     * Apply easing function to progress
     */
    applyEasing(progress, easingFunction) {
        switch (easingFunction) {
            case 'easeIn':
                return progress * progress;
            case 'easeOut':
                return 1 - (1 - progress) * (1 - progress);
            case 'easeInOut':
                return progress < 0.5 
                    ? 2 * progress * progress 
                    : 1 - Math.pow(-2 * progress + 2, 2) / 2;
            case 'bounce':
                return this.bounceEasing(progress);
            case 'elastic':
                return this.elasticEasing(progress);
            default:
                return progress; // linear
        }
    }

    /**
     * Bounce easing function
     */
    bounceEasing(progress) {
        const n1 = 7.5625;
        const d1 = 2.75;

        if (progress < 1 / d1) {
            return n1 * progress * progress;
        } else if (progress < 2 / d1) {
            return n1 * (progress -= 1.5 / d1) * progress + 0.75;
        } else if (progress < 2.5 / d1) {
            return n1 * (progress -= 2.25 / d1) * progress + 0.9375;
        } else {
            return n1 * (progress -= 2.625 / d1) * progress + 0.984375;
        }
    }

    /**
     * Elastic easing function
     */
    elasticEasing(progress) {
        const c4 = (2 * Math.PI) / 3;
        
        return progress === 0
            ? 0
            : progress === 1
            ? 1
            : -Math.pow(2, 10 * progress - 10) * Math.sin((progress * 10 - 10.75) * c4);
    }
}

export class AnimationManager {
    constructor() {
        this.animations = new Map();
        this.nextId = 0;
    }

    /**
     * Create a new animation
     */
    createAnimation(type, duration, properties = {}) {
        const id = this.nextId++;
        const animation = new Animation(type, Date.now(), duration, properties);
        this.animations.set(id, animation);
        return id;
    }

    /**
     * Create smooth movement animation
     */
    createMoveAnimation(fromX, fromY, toX, toY, duration = 300) {
        return this.createAnimation('move', duration, {
            fromX, fromY, toX, toY,
            easing: 'easeOut'
        });
    }

    /**
     * Create fade-out clearing animation
     */
    createClearAnimation(x, y, duration = 500) {
        return this.createAnimation('clear', duration, {
            x, y,
            startAlpha: 1,
            endAlpha: 0,
            startScale: 1,
            endScale: 1.5,
            easing: 'easeIn'
        });
    }

    /**
     * Create chain reaction highlight animation
     */
    createChainHighlight(x, y, chainLevel, duration = 800) {
        const colors = [
            '#FFD700', // Gold
            '#FF6B6B', // Red
            '#4ECDC4', // Teal
            '#45B7D1', // Blue
            '#96CEB4', // Green
            '#FFEAA7', // Yellow
            '#DDA0DD', // Plum
            '#98D8C8'  // Mint
        ];
        
        const color = colors[Math.min(chainLevel - 1, colors.length - 1)];
        
        return this.createAnimation('chainHighlight', duration, {
            x, y, chainLevel, color,
            startRadius: 10,
            endRadius: 40,
            startAlpha: 0.8,
            endAlpha: 0,
            easing: 'easeOut'
        });
    }

    /**
     * Create puyo drop animation
     */
    createDropAnimation(x, fromY, toY, duration = 200) {
        return this.createAnimation('drop', duration, {
            x, fromY, toY,
            easing: 'bounce'
        });
    }

    /**
     * Create rotation animation
     */
    createRotationAnimation(centerX, centerY, radius, startAngle, endAngle, duration = 200) {
        return this.createAnimation('rotation', duration, {
            centerX, centerY, radius, startAngle, endAngle,
            easing: 'easeInOut'
        });
    }

    /**
     * Create screen shake animation for powerful chains
     */
    createScreenShake(intensity = 5, duration = 300) {
        return this.createAnimation('screenShake', duration, {
            intensity,
            easing: 'easeOut'
        });
    }

    /**
     * Update all animations
     */
    update(currentTime) {
        const completedAnimations = [];
        
        for (const [id, animation] of this.animations) {
            animation.update(currentTime);
            
            if (animation.isComplete) {
                completedAnimations.push(id);
            }
        }
        
        // Remove completed animations
        completedAnimations.forEach(id => {
            this.animations.delete(id);
        });
        
        return completedAnimations;
    }

    /**
     * Get animation by ID
     */
    getAnimation(id) {
        return this.animations.get(id);
    }

    /**
     * Get all animations of a specific type
     */
    getAnimationsByType(type) {
        return Array.from(this.animations.values()).filter(anim => anim.type === type);
    }

    /**
     * Remove animation by ID
     */
    removeAnimation(id) {
        return this.animations.delete(id);
    }

    /**
     * Clear all animations
     */
    clearAll() {
        this.animations.clear();
    }

    /**
     * Get current animation count
     */
    getAnimationCount() {
        return this.animations.size;
    }

    /**
     * Check if any animations are running
     */
    hasActiveAnimations() {
        return this.animations.size > 0;
    }

    /**
     * Get interpolated position for move animation
     */
    getMovePosition(animation) {
        if (animation.type !== 'move') return null;
        
        const { fromX, fromY, toX, toY, easing } = animation.properties;
        
        return {
            x: animation.getValue(fromX, toX, easing),
            y: animation.getValue(fromY, toY, easing)
        };
    }

    /**
     * Get interpolated values for clear animation
     */
    getClearValues(animation) {
        if (animation.type !== 'clear') return null;
        
        const { startAlpha, endAlpha, startScale, endScale, easing } = animation.properties;
        
        return {
            alpha: animation.getValue(startAlpha, endAlpha, easing),
            scale: animation.getValue(startScale, endScale, easing)
        };
    }

    /**
     * Get interpolated values for chain highlight animation
     */
    getChainHighlightValues(animation) {
        if (animation.type !== 'chainHighlight') return null;
        
        const { startRadius, endRadius, startAlpha, endAlpha, color, easing } = animation.properties;
        
        return {
            radius: animation.getValue(startRadius, endRadius, easing),
            alpha: animation.getValue(startAlpha, endAlpha, easing),
            color
        };
    }

    /**
     * Get interpolated position for drop animation
     */
    getDropPosition(animation) {
        if (animation.type !== 'drop') return null;
        
        const { x, fromY, toY, easing } = animation.properties;
        
        return {
            x,
            y: animation.getValue(fromY, toY, easing)
        };
    }

    /**
     * Get interpolated rotation values
     */
    getRotationValues(animation) {
        if (animation.type !== 'rotation') return null;
        
        const { centerX, centerY, radius, startAngle, endAngle, easing } = animation.properties;
        const currentAngle = animation.getValue(startAngle, endAngle, easing);
        
        return {
            x: centerX + Math.cos(currentAngle) * radius,
            y: centerY + Math.sin(currentAngle) * radius,
            angle: currentAngle
        };
    }

    /**
     * Get screen shake offset
     */
    getScreenShakeOffset(animation) {
        if (animation.type !== 'screenShake') return { x: 0, y: 0 };
        
        const { intensity, easing } = animation.properties;
        const currentIntensity = animation.getValue(intensity, 0, easing);
        
        return {
            x: (Math.random() - 0.5) * currentIntensity * 2,
            y: (Math.random() - 0.5) * currentIntensity * 2
        };
    }
}