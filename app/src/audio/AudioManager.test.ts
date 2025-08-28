import { describe, it, expect, beforeEach, vi } from 'vitest'
import Phaser from 'phaser'
import { AudioManager } from './AudioManager'

describe('AudioManager', () => {
  let scene: Phaser.Scene
  let audioManager: AudioManager

  beforeEach(() => {
    // Mock scene
    scene = {
      sound: {
        add: vi.fn().mockReturnValue({
          play: vi.fn(),
          stop: vi.fn(),
          pause: vi.fn(),
          resume: vi.fn(),
          setVolume: vi.fn(),
          setLoop: vi.fn(),
          once: vi.fn(),
          isPlaying: false,
          volume: 1,
          destroy: vi.fn(),
        }),
        stopAll: vi.fn(),
      },
      tweens: {
        add: vi.fn(),
      },
    } as unknown as Phaser.Scene

    audioManager = new AudioManager(scene)
  })

  describe('コンストラクタ', () => {
    it('オーディオマネージャーが正しく初期化される', () => {
      expect(audioManager).toBeDefined()
      expect(audioManager.getMasterVolume()).toBe(1.0)
      expect(audioManager.getBgmVolume()).toBe(0.7)
      expect(audioManager.getSeVolume()).toBe(0.8)
    })
  })

  describe('音量管理', () => {
    it('マスター音量を設定できる', () => {
      audioManager.setMasterVolume(0.5)
      expect(audioManager.getMasterVolume()).toBe(0.5)
    })

    it('BGM音量を設定できる', () => {
      audioManager.setBgmVolume(0.6)
      expect(audioManager.getBgmVolume()).toBe(0.6)
    })

    it('SE音量を設定できる', () => {
      audioManager.setSeVolume(0.9)
      expect(audioManager.getSeVolume()).toBe(0.9)
    })

    it('音量は0-1の範囲に制限される', () => {
      audioManager.setMasterVolume(-0.5)
      expect(audioManager.getMasterVolume()).toBe(0)

      audioManager.setMasterVolume(1.5)
      expect(audioManager.getMasterVolume()).toBe(1)
    })

    it('音量変更時にコールバックが呼ばれる', () => {
      const onVolumeChange = vi.fn()
      audioManager.onVolumeChange = onVolumeChange

      audioManager.setMasterVolume(0.5)

      expect(onVolumeChange).toHaveBeenCalledWith({
        master: 0.5,
        bgm: 0.7,
        se: 0.8,
      })
    })
  })

  describe('BGM管理', () => {
    it('BGMを再生できる', () => {
      audioManager.playBgm('test-bgm')

      expect(scene.sound.add).toHaveBeenCalledWith('test-bgm')
      const mockSound = (scene.sound.add as ReturnType<typeof vi.fn>).mock.results[0].value
      expect(mockSound.setLoop).toHaveBeenCalledWith(true)
      expect(mockSound.setVolume).toHaveBeenCalled()
      expect(mockSound.play).toHaveBeenCalled()
    })

    it('BGMを停止できる', () => {
      audioManager.playBgm('test-bgm')
      audioManager.stopBgm()

      const mockSound = (scene.sound.add as ReturnType<typeof vi.fn>).mock.results[0].value
      expect(mockSound.stop).toHaveBeenCalled()
    })

    it('BGMを一時停止・再開できる', () => {
      audioManager.playBgm('test-bgm')

      audioManager.pauseBgm()
      const mockSound = (scene.sound.add as ReturnType<typeof vi.fn>).mock.results[0].value
      expect(mockSound.pause).toHaveBeenCalled()

      audioManager.resumeBgm()
      expect(mockSound.resume).toHaveBeenCalled()
    })

    it('同じBGMが再生中の場合は何もしない', () => {
      // 初回再生
      audioManager.playBgm('test-bgm')

      // 現在のBGMを再生中に設定
      const currentBgm = audioManager['currentBgm']
      if (currentBgm) {
        currentBgm.isPlaying = true
      }

      vi.clearAllMocks() // モックをリセット

      // 同じBGMを再度再生
      audioManager.playBgm('test-bgm')

      // add は呼ばれない
      expect(scene.sound.add).not.toHaveBeenCalled()
    })

    it('異なるBGMに切り替えができる', () => {
      audioManager.playBgm('bgm1')
      audioManager.playBgm('bgm2')

      expect(scene.sound.add).toHaveBeenCalledTimes(2)
      expect(scene.sound.add).toHaveBeenNthCalledWith(1, 'bgm1')
      expect(scene.sound.add).toHaveBeenNthCalledWith(2, 'bgm2')
    })
  })

  describe('SE管理', () => {
    it('SEを再生できる', () => {
      audioManager.playSe('test-se')

      expect(scene.sound.add).toHaveBeenCalledWith('test-se')
      const mockSound = (scene.sound.add as ReturnType<typeof vi.fn>).mock.results[0].value
      expect(mockSound.setVolume).toHaveBeenCalled()
      expect(mockSound.play).toHaveBeenCalled()
    })

    it('複数のSEを同時再生できる', () => {
      audioManager.playSe('se1')
      audioManager.playSe('se2')
      audioManager.playSe('se1') // 同じSEも再度再生可能

      expect(scene.sound.add).toHaveBeenCalledTimes(3)
    })

    it('全てのSEを停止できる', () => {
      audioManager.playSe('se1')
      audioManager.playSe('se2')

      audioManager.stopAllSe()

      // Phaser の stopAll が呼ばれることを確認するためのテスト
      // 実際の実装では個別のSE追跡が必要
      expect(scene.sound.stopAll).toHaveBeenCalled()
    })
  })

  describe('フェード機能', () => {
    it('BGMをフェードインできる', () => {
      audioManager.fadeInBgm('test-bgm', 1000)

      expect(scene.sound.add).toHaveBeenCalledWith('test-bgm')
      const mockSound = (scene.sound.add as ReturnType<typeof vi.fn>).mock.results[0].value
      expect(mockSound.play).toHaveBeenCalled()
    })

    it('BGMをフェードアウトできる', async () => {
      audioManager.playBgm('test-bgm')

      const fadePromise = audioManager.fadeOutBgm(500)

      expect(fadePromise).toBeDefined()
      // フェード完了の確認は統合テストで実施
    })
  })

  describe('ミュート機能', () => {
    it('全体をミュート・アンミュートできる', () => {
      audioManager.setMuted(true)
      expect(audioManager.isMuted()).toBe(true)

      audioManager.setMuted(false)
      expect(audioManager.isMuted()).toBe(false)
    })

    it('ミュート時は音量が0になる', () => {
      audioManager.setMasterVolume(0.8)
      audioManager.setMuted(true)

      audioManager.playBgm('test-bgm')

      const mockSound = (scene.sound.add as ReturnType<typeof vi.fn>).mock.results[0].value
      expect(mockSound.setVolume).toHaveBeenCalledWith(0)
    })
  })

  describe('破棄処理', () => {
    it('オーディオマネージャーが正しく破棄される', () => {
      audioManager.playBgm('test-bgm')
      audioManager.playSe('test-se')

      audioManager.destroy()

      expect(scene.sound.stopAll).toHaveBeenCalled()
    })
  })
})
