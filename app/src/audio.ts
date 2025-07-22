// サウンドシステム - Web Audio API実装

export enum SoundType {
  Move = 'move',
  Rotate = 'rotate',
  Land = 'land',
  Erase = 'erase',
  Chain = 'chain',
  GameOver = 'gameOver',
  BGM = 'bgm'
}

export interface SoundConfig {
  volume: number  // 0.0 - 1.0
  enabled: boolean
  bgmEnabled: boolean
}

export interface SoundEffect {
  id: string
  type: SoundType
  frequency: number
  duration: number
  fadeOut?: boolean
  chain?: number // チェイン数（チェイン音用）
}

export class AudioEngine {
  private audioContext: AudioContext | null = null
  private masterGain: GainNode | null = null
  private bgmGain: GainNode | null = null
  private sfxGain: GainNode | null = null
  private config: SoundConfig
  private isInitialized = false
  private bgmOscillator: OscillatorNode | null = null
  private activeSounds: Map<string, { oscillator: OscillatorNode; gain: GainNode }> = new Map()

  constructor() {
    this.config = {
      volume: 0.7,
      enabled: true,
      bgmEnabled: true
    }
  }

  async initialize(): Promise<void> {
    try {
      // Web Audio APIの初期化
      this.audioContext = new (window.AudioContext || (window as any).webkitAudioContext)()
      
      // マスターゲイン設定
      this.masterGain = this.audioContext.createGain()
      this.masterGain.connect(this.audioContext.destination)
      this.masterGain.gain.setValueAtTime(this.config.volume, this.audioContext.currentTime)

      // BGM用ゲイン
      this.bgmGain = this.audioContext.createGain()
      this.bgmGain.connect(this.masterGain)
      this.bgmGain.gain.setValueAtTime(0.3, this.audioContext.currentTime)

      // 効果音用ゲイン
      this.sfxGain = this.audioContext.createGain()
      this.sfxGain.connect(this.masterGain)
      this.sfxGain.gain.setValueAtTime(0.8, this.audioContext.currentTime)

      this.isInitialized = true
      console.log('🔊 Audio Engine initialized')
    } catch (error) {
      console.warn('オーディオの初期化に失敗しました:', error)
      this.isInitialized = false
    }
  }

  // 効果音再生
  playSound(type: SoundType, options?: { chain?: number; pitch?: number }): void {
    if (!this.isInitialized || !this.config.enabled || !this.audioContext || !this.sfxGain) {
      return
    }

    const effect = this.createSoundEffect(type, options)
    this.playSoundEffect(effect)
  }

  // BGM開始
  startBGM(): void {
    if (!this.isInitialized || !this.config.bgmEnabled || !this.audioContext || !this.bgmGain) {
      return
    }

    this.stopBGM() // 既存のBGMを停止

    try {
      // シンプルなBGMメロディ生成
      this.bgmOscillator = this.audioContext.createOscillator()
      this.bgmOscillator.type = 'sine'
      this.bgmOscillator.frequency.setValueAtTime(220, this.audioContext.currentTime) // A3

      // BGMパターン（簡単なアルペジオ）
      const melody = [220, 261.63, 329.63, 392, 329.63, 261.63] // A3, C4, E4, G4, E4, C4
      let currentNote = 0

      const playNextNote = () => {
        if (this.bgmOscillator && this.audioContext) {
          const frequency = melody[currentNote % melody.length]
          this.bgmOscillator.frequency.setValueAtTime(frequency, this.audioContext.currentTime)
          currentNote++
          setTimeout(playNextNote, 800) // 0.8秒ごとに音符変更
        }
      }

      this.bgmOscillator.connect(this.bgmGain)
      this.bgmOscillator.start()
      playNextNote()

    } catch (error) {
      console.warn('BGMの再生に失敗しました:', error)
    }
  }

  // BGM停止
  stopBGM(): void {
    if (this.bgmOscillator) {
      try {
        this.bgmOscillator.stop()
        this.bgmOscillator.disconnect()
      } catch (error) {
        // すでに停止している場合は無視
      }
      this.bgmOscillator = null
    }
  }

  // 音量設定
  setMasterVolume(volume: number): void {
    this.config.volume = Math.max(0, Math.min(1, volume))
    if (this.masterGain && this.audioContext) {
      this.masterGain.gain.setValueAtTime(this.config.volume, this.audioContext.currentTime)
    }
  }

  // 効果音有効/無効
  setSoundEnabled(enabled: boolean): void {
    this.config.enabled = enabled
  }

  // BGM有効/無効
  setBGMEnabled(enabled: boolean): void {
    this.config.bgmEnabled = enabled
    if (!enabled) {
      this.stopBGM()
    }
  }

  // 設定取得
  getConfig(): SoundConfig {
    return { ...this.config }
  }

  // 全音停止
  stopAllSounds(): void {
    // 効果音停止
    for (const [id, sound] of this.activeSounds.entries()) {
      try {
        sound.oscillator.stop()
        sound.oscillator.disconnect()
        sound.gain.disconnect()
      } catch (error) {
        // 無視
      }
    }
    this.activeSounds.clear()

    // BGM停止
    this.stopBGM()
  }

  // AudioContextの状態確認
  getAudioState(): string {
    if (!this.audioContext) return 'not-initialized'
    return this.audioContext.state
  }

  // ユーザージェスチャー後のAudioContext再開
  async resumeAudioContext(): Promise<void> {
    if (this.audioContext && this.audioContext.state === 'suspended') {
      try {
        await this.audioContext.resume()
        console.log('AudioContext resumed')
      } catch (error) {
        console.warn('AudioContext resume failed:', error)
      }
    }
  }

  private createSoundEffect(type: SoundType, options?: { chain?: number; pitch?: number }): SoundEffect {
    const baseFreq = options?.pitch || 1.0

    switch (type) {
      case SoundType.Move:
        return {
          id: `move_${Date.now()}`,
          type,
          frequency: 400 * baseFreq,
          duration: 0.1
        }

      case SoundType.Rotate:
        return {
          id: `rotate_${Date.now()}`,
          type,
          frequency: 600 * baseFreq,
          duration: 0.15
        }

      case SoundType.Land:
        return {
          id: `land_${Date.now()}`,
          type,
          frequency: 200 * baseFreq,
          duration: 0.2
        }

      case SoundType.Erase:
        return {
          id: `erase_${Date.now()}`,
          type,
          frequency: 800 * baseFreq,
          duration: 0.3,
          fadeOut: true
        }

      case SoundType.Chain:
        const chain = options?.chain || 1
        return {
          id: `chain_${Date.now()}`,
          type,
          frequency: (400 + (chain * 200)) * baseFreq, // チェイン数に応じて高音に
          duration: 0.4 + (chain * 0.1), // チェイン数に応じて長く
          fadeOut: true,
          chain
        }

      case SoundType.GameOver:
        return {
          id: `gameOver_${Date.now()}`,
          type,
          frequency: 150 * baseFreq,
          duration: 1.0,
          fadeOut: true
        }

      default:
        return {
          id: `sound_${Date.now()}`,
          type,
          frequency: 440 * baseFreq,
          duration: 0.2
        }
    }
  }

  private playSoundEffect(effect: SoundEffect): void {
    if (!this.audioContext || !this.sfxGain) return

    try {
      const oscillator = this.audioContext.createOscillator()
      const gain = this.audioContext.createGain()

      // 音の種類に応じた波形設定
      switch (effect.type) {
        case SoundType.Chain:
          oscillator.type = 'sawtooth'
          break
        case SoundType.Erase:
          oscillator.type = 'triangle'
          break
        case SoundType.GameOver:
          oscillator.type = 'square'
          break
        default:
          oscillator.type = 'sine'
      }

      oscillator.frequency.setValueAtTime(effect.frequency, this.audioContext.currentTime)
      
      // ゲイン設定
      const startTime = this.audioContext.currentTime
      const endTime = startTime + effect.duration

      gain.gain.setValueAtTime(0.5, startTime)
      
      if (effect.fadeOut) {
        gain.gain.exponentialRampToValueAtTime(0.01, endTime)
      } else {
        gain.gain.setValueAtTime(0.5, endTime - 0.01)
        gain.gain.exponentialRampToValueAtTime(0.01, endTime)
      }

      // 接続
      oscillator.connect(gain)
      gain.connect(this.sfxGain)

      // 再生
      oscillator.start(startTime)
      oscillator.stop(endTime)

      // アクティブサウンド登録
      this.activeSounds.set(effect.id, { oscillator, gain })

      // 終了後クリーンアップ
      oscillator.onended = () => {
        this.activeSounds.delete(effect.id)
        try {
          oscillator.disconnect()
          gain.disconnect()
        } catch (error) {
          // 無視
        }
      }

    } catch (error) {
      console.warn('効果音の再生に失敗しました:', error)
    }
  }

  // リソース解放
  destroy(): void {
    this.stopAllSounds()
    
    if (this.audioContext) {
      try {
        this.audioContext.close()
      } catch (error) {
        // 無視
      }
      this.audioContext = null
    }

    this.isInitialized = false
  }
}

// プリセット音響効果
export class SoundPresets {
  static getPuyoMoveSound(): SoundEffect {
    return {
      id: 'puyo_move',
      type: SoundType.Move,
      frequency: 400,
      duration: 0.08
    }
  }

  static getPuyoRotateSound(): SoundEffect {
    return {
      id: 'puyo_rotate', 
      type: SoundType.Rotate,
      frequency: 550,
      duration: 0.12
    }
  }

  static getPuyoLandSound(): SoundEffect {
    return {
      id: 'puyo_land',
      type: SoundType.Land,
      frequency: 180,
      duration: 0.25
    }
  }

  static getChainSound(chainCount: number): SoundEffect {
    return {
      id: `chain_${chainCount}`,
      type: SoundType.Chain,
      frequency: 600 + (chainCount * 150),
      duration: 0.3 + (chainCount * 0.08),
      fadeOut: true,
      chain: chainCount
    }
  }

  static getEraseSound(puyoCount: number): SoundEffect {
    const pitch = Math.min(1.5, 1.0 + (puyoCount - 4) * 0.1)
    return {
      id: 'puyo_erase',
      type: SoundType.Erase,
      frequency: 750 * pitch,
      duration: 0.35,
      fadeOut: true
    }
  }
}

// グローバルサウンドマネージャー
export const soundManager = new AudioEngine()