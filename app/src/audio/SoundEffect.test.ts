import { describe, it, expect, beforeEach, vi } from 'vitest'
import Phaser from 'phaser'
import { SoundEffect } from './SoundEffect'

describe('SoundEffect', () => {
  let scene: Phaser.Scene
  let soundEffect: SoundEffect

  beforeEach(() => {
    // Mock scene
    scene = {
      sound: {
        add: vi.fn().mockReturnValue({
          play: vi.fn(),
          stop: vi.fn(),
          setVolume: vi.fn(),
          isPlaying: false,
          destroy: vi.fn(),
        }),
      },
    } as unknown as Phaser.Scene
  })

  describe('事前定義SE', () => {
    it('クリック音を再生できる', () => {
      soundEffect = new SoundEffect(scene)
      soundEffect.playClick()

      expect(scene.sound.add).toHaveBeenCalledWith('click')
    })

    it('ホバー音を再生できる', () => {
      soundEffect = new SoundEffect(scene)
      soundEffect.playHover()

      expect(scene.sound.add).toHaveBeenCalledWith('hover')
    })

    it('確定音を再生できる', () => {
      soundEffect = new SoundEffect(scene)
      soundEffect.playConfirm()

      expect(scene.sound.add).toHaveBeenCalledWith('confirm')
    })

    it('キャンセル音を再生できる', () => {
      soundEffect = new SoundEffect(scene)
      soundEffect.playCancel()

      expect(scene.sound.add).toHaveBeenCalledWith('cancel')
    })

    it('エラー音を再生できる', () => {
      soundEffect = new SoundEffect(scene)
      soundEffect.playError()

      expect(scene.sound.add).toHaveBeenCalledWith('error')
    })
  })

  describe('カスタムSE', () => {
    it('カスタム音を再生できる', () => {
      soundEffect = new SoundEffect(scene)
      soundEffect.play('custom-sound')

      expect(scene.sound.add).toHaveBeenCalledWith('custom-sound')
    })

    it('音量を指定して再生できる', () => {
      soundEffect = new SoundEffect(scene)
      soundEffect.play('test-sound', 0.5)

      const mockSound = (scene.sound.add as ReturnType<typeof vi.fn>).mock.results[0].value
      expect(mockSound.setVolume).toHaveBeenCalledWith(0.5)
    })
  })

  describe('音響設定統合', () => {
    it('AudioManagerと統合して音量制御できる', () => {
      const mockAudioManager = {
        getSeVolume: vi.fn().mockReturnValue(0.8),
        getMasterVolume: vi.fn().mockReturnValue(0.7),
        isMuted: vi.fn().mockReturnValue(false),
      }

      soundEffect = new SoundEffect(scene, mockAudioManager)
      soundEffect.play('test-sound')

      const mockSound = (scene.sound.add as ReturnType<typeof vi.fn>).mock.results[0].value
      expect(mockSound.setVolume).toHaveBeenCalledWith(expect.closeTo(0.56, 5)) // 0.8 * 0.7
    })

    it('ミュート時は音量が0になる', () => {
      const mockAudioManager = {
        getSeVolume: vi.fn().mockReturnValue(0.8),
        getMasterVolume: vi.fn().mockReturnValue(0.7),
        isMuted: vi.fn().mockReturnValue(true),
      }

      soundEffect = new SoundEffect(scene, mockAudioManager)
      soundEffect.play('test-sound')

      const mockSound = (scene.sound.add as ReturnType<typeof vi.fn>).mock.results[0].value
      expect(mockSound.setVolume).toHaveBeenCalledWith(0)
    })
  })
})
