/**
 * AudioManager - Handles sound effects and background music
 * Provides audio feedback for game events
 */

export class AudioManager {
    constructor() {
        this.audioContext = null;
        this.sounds = new Map();
        this.musicVolume = 0.5;
        this.sfxVolume = 0.7;
        this.isMuted = false;
        
        // Initialize audio context
        this.initializeAudio();
    }

    /**
     * Initialize Web Audio API context
     */
    async initializeAudio() {
        try {
            // Create audio context
            this.audioContext = new (window.AudioContext || window.webkitAudioContext)();
            
            // Resume audio context if it's suspended (required by some browsers)
            if (this.audioContext.state === 'suspended') {
                await this.audioContext.resume();
            }
            
            console.log('Audio system initialized');
        } catch (error) {
            console.warn('Failed to initialize audio:', error);
        }
    }

    /**
     * Create a simple tone for sound effects
     */
    createTone(frequency, duration, type = 'sine') {
        if (!this.audioContext || this.isMuted) return;
        
        try {
            const oscillator = this.audioContext.createOscillator();
            const gainNode = this.audioContext.createGain();
            
            oscillator.connect(gainNode);
            gainNode.connect(this.audioContext.destination);
            
            oscillator.frequency.setValueAtTime(frequency, this.audioContext.currentTime);
            oscillator.type = type;
            
            gainNode.gain.setValueAtTime(0, this.audioContext.currentTime);
            gainNode.gain.linearRampToValueAtTime(this.sfxVolume * 0.1, this.audioContext.currentTime + 0.01);
            gainNode.gain.exponentialRampToValueAtTime(0.001, this.audioContext.currentTime + duration);
            
            oscillator.start(this.audioContext.currentTime);
            oscillator.stop(this.audioContext.currentTime + duration);
        } catch (error) {
            console.warn('Failed to create tone:', error);
        }
    }

    /**
     * Play move sound effect
     */
    playMoveSound() {
        this.createTone(220, 0.1, 'square');
    }

    /**
     * Play rotate sound effect
     */
    playRotateSound() {
        this.createTone(330, 0.15, 'triangle');
    }

    /**
     * Play drop sound effect
     */
    playDropSound() {
        this.createTone(110, 0.2, 'sawtooth');
    }

    /**
     * Play puyo clear sound effect
     */
    playClearSound() {
        // Play a sequence of tones for clearing effect
        setTimeout(() => this.createTone(440, 0.1), 0);
        setTimeout(() => this.createTone(550, 0.1), 50);
        setTimeout(() => this.createTone(660, 0.1), 100);
    }

    /**
     * Play chain sound effect
     */
    playChainSound(chainLevel) {
        // Higher pitch for higher chain levels
        const baseFreq = 440;
        const frequency = baseFreq * Math.pow(1.2, Math.min(chainLevel, 10));
        
        // Longer duration for higher chains
        const duration = 0.2 + (chainLevel * 0.05);
        
        this.createTone(frequency, duration, 'sine');
        
        // Add harmony for higher chains
        if (chainLevel > 2) {
            setTimeout(() => {
                this.createTone(frequency * 1.5, duration * 0.8, 'triangle');
            }, 50);
        }
    }

    /**
     * Play all-clear (zenkeshi) sound effect
     */
    playAllClearSound() {
        // Play a triumphant sequence
        const notes = [440, 554, 659, 880];
        notes.forEach((freq, index) => {
            setTimeout(() => {
                this.createTone(freq, 0.3, 'sine');
            }, index * 100);
        });
    }

    /**
     * Play game over sound effect
     */
    playGameOverSound() {
        // Play a descending sequence
        const notes = [440, 370, 311, 262];
        notes.forEach((freq, index) => {
            setTimeout(() => {
                this.createTone(freq, 0.4, 'triangle');
            }, index * 200);
        });
    }

    /**
     * Play level up sound effect
     */
    playLevelUpSound() {
        // Play an ascending arpeggio
        const notes = [262, 330, 392, 523];
        notes.forEach((freq, index) => {
            setTimeout(() => {
                this.createTone(freq, 0.2, 'sine');
            }, index * 80);
        });
    }

    /**
     * Set music volume
     */
    setMusicVolume(volume) {
        this.musicVolume = Math.max(0, Math.min(1, volume));
    }

    /**
     * Set sound effects volume
     */
    setSfxVolume(volume) {
        this.sfxVolume = Math.max(0, Math.min(1, volume));
    }

    /**
     * Mute all audio
     */
    mute() {
        this.isMuted = true;
    }

    /**
     * Unmute all audio
     */
    unmute() {
        this.isMuted = false;
    }

    /**
     * Toggle mute state
     */
    toggleMute() {
        this.isMuted = !this.isMuted;
        return this.isMuted;
    }

    /**
     * Check if audio is muted
     */
    isMutedState() {
        return this.isMuted;
    }

    /**
     * Resume audio context (required for user interaction)
     */
    async resumeAudio() {
        if (this.audioContext && this.audioContext.state === 'suspended') {
            try {
                await this.audioContext.resume();
                console.log('Audio context resumed');
            } catch (error) {
                console.warn('Failed to resume audio context:', error);
            }
        }
    }

    /**
     * Clean up audio resources
     */
    dispose() {
        if (this.audioContext) {
            this.audioContext.close();
            this.audioContext = null;
        }
        this.sounds.clear();
    }
}