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
     * Play background music (placeholder for future implementation)
     */
    playBackgroundMusic(trackName = 'main') {
        if (this.isMuted) return;
        
        console.log(`Playing background music: ${trackName}`);
        // TODO: Implement actual background music playback
        // This would load and play audio files when implemented
    }

    /**
     * Stop background music
     */
    stopBackgroundMusic() {
        console.log('Stopping background music');
        // TODO: Implement actual background music stopping
    }

    /**
     * Play menu sound effect
     */
    playMenuSound() {
        this.createTone(523, 0.1, 'sine');
    }

    /**
     * Play button click sound effect
     */
    playButtonSound() {
        this.createTone(440, 0.08, 'square');
    }

    /**
     * Play warning sound effect (for game over warning)
     */
    playWarningSound() {
        this.createTone(200, 0.3, 'sawtooth');
        setTimeout(() => this.createTone(200, 0.3, 'sawtooth'), 400);
    }

    /**
     * Play combo sound effect for multiple simultaneous clears
     */
    playComboSound(comboCount) {
        const baseFreq = 330;
        for (let i = 0; i < Math.min(comboCount, 5); i++) {
            setTimeout(() => {
                this.createTone(baseFreq + (i * 110), 0.15, 'triangle');
            }, i * 60);
        }
    }

    /**
     * Play soft landing sound for puyo placement
     */
    playSoftLandSound() {
        this.createTone(150, 0.1, 'sine');
    }

    /**
     * Play hard landing sound for fast drop
     */
    playHardLandSound() {
        this.createTone(100, 0.2, 'square');
    }

    /**
     * Create audio integration points for external audio files
     */
    loadAudioFile(name, url) {
        // Placeholder for loading external audio files
        console.log(`Audio integration point: Load ${name} from ${url}`);
        // TODO: Implement actual audio file loading
        // This would use fetch() to load audio files and decode them
        return Promise.resolve();
    }

    /**
     * Play loaded audio file
     */
    playAudioFile(name, volume = 1.0, loop = false) {
        // Placeholder for playing loaded audio files
        console.log(`Audio integration point: Play ${name} (volume: ${volume}, loop: ${loop})`);
        // TODO: Implement actual audio file playback
    }

    /**
     * Get audio settings for persistence
     */
    getAudioSettings() {
        return {
            musicVolume: this.musicVolume,
            sfxVolume: this.sfxVolume,
            isMuted: this.isMuted
        };
    }

    /**
     * Apply audio settings from persistence
     */
    applyAudioSettings(settings) {
        if (settings.musicVolume !== undefined) {
            this.setMusicVolume(settings.musicVolume);
        }
        if (settings.sfxVolume !== undefined) {
            this.setSfxVolume(settings.sfxVolume);
        }
        if (settings.isMuted !== undefined) {
            this.isMuted = settings.isMuted;
        }
    }

    /**
     * Create dynamic sound effect based on game state
     */
    playDynamicSound(eventType, intensity = 1.0, pitch = 1.0) {
        if (this.isMuted) return;
        
        const soundMap = {
            'puyo_land': { freq: 150, duration: 0.1, type: 'sine' },
            'puyo_clear': { freq: 440, duration: 0.15, type: 'triangle' },
            'chain_start': { freq: 330, duration: 0.2, type: 'square' },
            'chain_continue': { freq: 550, duration: 0.18, type: 'sine' },
            'all_clear': { freq: 880, duration: 0.4, type: 'sine' },
            'game_over': { freq: 220, duration: 0.5, type: 'sawtooth' },
            'level_up': { freq: 523, duration: 0.3, type: 'triangle' },
            'warning': { freq: 200, duration: 0.25, type: 'square' }
        };
        
        const sound = soundMap[eventType];
        if (sound) {
            const frequency = sound.freq * pitch;
            const duration = sound.duration * intensity;
            this.createTone(frequency, duration, sound.type);
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