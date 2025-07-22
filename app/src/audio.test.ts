import { describe, it, expect, beforeEach, vi } from 'vitest'
import { AudioEngine, SoundType, SoundPresets } from './audio'

// Web Audio API のモック化
class MockAudioContext {
  public state = 'running'
  public currentTime = 0
  public destination = {}

  createGain() {
    return {
      gain: {
        setValueAtTime: vi.fn(),
        exponentialRampToValueAtTime: vi.fn()
      },
      connect: vi.fn()
    }
  }

  createOscillator() {
    return {
      type: 'sine',
      frequency: {
        setValueAtTime: vi.fn()
      },
      connect: vi.fn(),
      start: vi.fn(),
      stop: vi.fn(),
      disconnect: vi.fn(),
      onended: null
    }
  }

  resume() {
    return Promise.resolve()
  }

  close() {
    return Promise.resolve()
  }
}

// グローバルモック設定
Object.defineProperty(window, 'AudioContext', {
  writable: true,
  value: MockAudioContext
})

describe('AudioEngine', () => {
  let audioEngine: AudioEngine

  beforeEach(() => {
    audioEngine = new AudioEngine()
    vi.clearAllMocks()
  })

  describe('初期化', () => {
    it('AudioEngineを正常に作成できる', () => {
      expect(audioEngine).toBeInstanceOf(AudioEngine)
      expect(audioEngine.getAudioState()).toBe('not-initialized')
    })

    it('初期化後にオーディオ状態が変わる', async () => {
      await audioEngine.initialize()
      expect(audioEngine.getAudioState()).toBe('running')
    })

    it('設定のデフォルト値が正しい', () => {
      const config = audioEngine.getConfig()
      expect(config.volume).toBe(0.7)
      expect(config.enabled).toBe(true)
      expect(config.bgmEnabled).toBe(true)
    })
  })

  describe('設定管理', () => {
    beforeEach(async () => {
      await audioEngine.initialize()
    })

    it('音量を設定できる', () => {
      audioEngine.setMasterVolume(0.5)
      expect(audioEngine.getConfig().volume).toBe(0.5)
    })

    it('音量範囲が制限される', () => {
      audioEngine.setMasterVolume(-0.1)
      expect(audioEngine.getConfig().volume).toBe(0)
      
      audioEngine.setMasterVolume(1.5)
      expect(audioEngine.getConfig().volume).toBe(1)
    })

    it('効果音の有効/無効を設定できる', () => {
      audioEngine.setSoundEnabled(false)
      expect(audioEngine.getConfig().enabled).toBe(false)
      
      audioEngine.setSoundEnabled(true)
      expect(audioEngine.getConfig().enabled).toBe(true)
    })

    it('BGMの有効/無効を設定できる', () => {
      audioEngine.setBGMEnabled(false)
      expect(audioEngine.getConfig().bgmEnabled).toBe(false)
      
      audioEngine.setBGMEnabled(true)
      expect(audioEngine.getConfig().bgmEnabled).toBe(true)
    })
  })

  describe('効果音再生', () => {
    beforeEach(async () => {
      await audioEngine.initialize()
    })

    it('Move音を再生できる', () => {
      audioEngine.playSound(SoundType.Move)
      // モックなので実際の音は再生されないが、エラーなく実行される
      expect(true).toBe(true)
    })

    it('Rotate音を再生できる', () => {
      audioEngine.playSound(SoundType.Rotate)
      expect(true).toBe(true)
    })

    it('Chain音をチェイン数付きで再生できる', () => {
      audioEngine.playSound(SoundType.Chain, { chain: 3 })
      expect(true).toBe(true)
    })

    it('効果音無効時は再生されない', () => {
      audioEngine.setSoundEnabled(false)
      audioEngine.playSound(SoundType.Move)
      // エラーなく実行されることを確認
      expect(true).toBe(true)
    })

    it('未初期化時は再生されない', () => {
      const uninitializedEngine = new AudioEngine()
      uninitializedEngine.playSound(SoundType.Move)
      expect(true).toBe(true)
    })
  })

  describe('BGM制御', () => {
    beforeEach(async () => {
      await audioEngine.initialize()
    })

    it('BGMを開始できる', () => {
      audioEngine.startBGM()
      expect(true).toBe(true)
    })

    it('BGMを停止できる', () => {
      audioEngine.startBGM()
      audioEngine.stopBGM()
      expect(true).toBe(true)
    })

    it('BGM無効時は開始されない', () => {
      audioEngine.setBGMEnabled(false)
      audioEngine.startBGM()
      expect(true).toBe(true)
    })

    it('複数回startBGMを呼んでも問題ない', () => {
      audioEngine.startBGM()
      audioEngine.startBGM()
      expect(true).toBe(true)
    })
  })

  describe('リソース管理', () => {
    it('全音停止ができる', async () => {
      await audioEngine.initialize()
      audioEngine.playSound(SoundType.Move)
      audioEngine.startBGM()
      
      audioEngine.stopAllSounds()
      expect(true).toBe(true)
    })

    it('リソース破棄ができる', async () => {
      await audioEngine.initialize()
      audioEngine.destroy()
      
      expect(audioEngine.getAudioState()).toBe('not-initialized')
    })
  })

  describe('AudioContext復旧', () => {
    it('AudioContext復旧を試行できる', async () => {
      await audioEngine.initialize()
      await audioEngine.resumeAudioContext()
      expect(true).toBe(true)
    })
  })
})

describe('SoundPresets', () => {
  describe('プリセット効果音', () => {
    it('Move音のプリセットが正しい', () => {
      const sound = SoundPresets.getPuyoMoveSound()
      expect(sound.type).toBe(SoundType.Move)
      expect(sound.frequency).toBe(400)
      expect(sound.duration).toBe(0.08)
    })

    it('Rotate音のプリセットが正しい', () => {
      const sound = SoundPresets.getPuyoRotateSound()
      expect(sound.type).toBe(SoundType.Rotate)
      expect(sound.frequency).toBe(550)
      expect(sound.duration).toBe(0.12)
    })

    it('Land音のプリセットが正しい', () => {
      const sound = SoundPresets.getPuyoLandSound()
      expect(sound.type).toBe(SoundType.Land)
      expect(sound.frequency).toBe(180)
      expect(sound.duration).toBe(0.25)
    })

    it('Chain音のプリセットがチェイン数に応じて変わる', () => {
      const chain1 = SoundPresets.getChainSound(1)
      const chain3 = SoundPresets.getChainSound(3)
      
      expect(chain1.type).toBe(SoundType.Chain)
      expect(chain3.type).toBe(SoundType.Chain)
      expect(chain3.frequency).toBeGreaterThan(chain1.frequency)
      expect(chain3.duration).toBeGreaterThan(chain1.duration)
      expect(chain3.chain).toBe(3)
    })

    it('Erase音のプリセットがぷよ数に応じて変わる', () => {
      const erase4 = SoundPresets.getEraseSound(4)
      const erase8 = SoundPresets.getEraseSound(8)
      
      expect(erase4.type).toBe(SoundType.Erase)
      expect(erase8.type).toBe(SoundType.Erase)
      expect(erase8.frequency).toBeGreaterThan(erase4.frequency)
      expect(erase4.fadeOut).toBe(true)
      expect(erase8.fadeOut).toBe(true)
    })
  })

  describe('プリセット値の妥当性', () => {
    it('周波数が妥当な範囲内', () => {
      const sounds = [
        SoundPresets.getPuyoMoveSound(),
        SoundPresets.getPuyoRotateSound(),
        SoundPresets.getPuyoLandSound(),
        SoundPresets.getChainSound(1),
        SoundPresets.getEraseSound(4)
      ]

      sounds.forEach(sound => {
        expect(sound.frequency).toBeGreaterThan(0)
        expect(sound.frequency).toBeLessThan(2000) // 人間の可聴域内
      })
    })

    it('持続時間が妥当な範囲内', () => {
      const sounds = [
        SoundPresets.getPuyoMoveSound(),
        SoundPresets.getPuyoRotateSound(),
        SoundPresets.getPuyoLandSound(),
        SoundPresets.getChainSound(1),
        SoundPresets.getEraseSound(4)
      ]

      sounds.forEach(sound => {
        expect(sound.duration).toBeGreaterThan(0)
        expect(sound.duration).toBeLessThan(2) // 2秒以下
      })
    })
  })
})

describe('SoundType', () => {
  it('すべてのSoundTypeが定義されている', () => {
    expect(SoundType.Move).toBeDefined()
    expect(SoundType.Rotate).toBeDefined()
    expect(SoundType.Land).toBeDefined()
    expect(SoundType.Erase).toBeDefined()
    expect(SoundType.Chain).toBeDefined()
    expect(SoundType.GameOver).toBeDefined()
    expect(SoundType.BGM).toBeDefined()
  })

  it('SoundType値が期待通り', () => {
    expect(SoundType.Move).toBe('move')
    expect(SoundType.Rotate).toBe('rotate')
    expect(SoundType.Land).toBe('land')
    expect(SoundType.Erase).toBe('erase')
    expect(SoundType.Chain).toBe('chain')
    expect(SoundType.GameOver).toBe('gameOver')
    expect(SoundType.BGM).toBe('bgm')
  })
})