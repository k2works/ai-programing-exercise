import { describe, it, expect, beforeEach, vi } from 'vitest'
import Phaser from 'phaser'
import { SettingsPanel } from './SettingsPanel'

describe('SettingsPanel', () => {
  let scene: Phaser.Scene
  let panel: SettingsPanel

  beforeEach(() => {
    // Mock scene
    scene = {
      add: {
        rectangle: vi.fn().mockReturnValue({
          setOrigin: vi.fn().mockReturnThis(),
          setAlpha: vi.fn().mockReturnThis(),
          setVisible: vi.fn().mockReturnThis(),
          setStrokeStyle: vi.fn().mockReturnThis(),
          destroy: vi.fn(),
        }),
        text: vi.fn().mockReturnValue({
          setOrigin: vi.fn().mockReturnThis(),
          setStyle: vi.fn().mockReturnThis(),
          setVisible: vi.fn().mockReturnThis(),
          destroy: vi.fn(),
        }),
      },
    } as unknown as Phaser.Scene
  })

  describe('コンストラクタ', () => {
    it('設定パネルが正しく初期化される', () => {
      panel = new SettingsPanel(scene, 400, 300)

      expect(scene.add.rectangle).toHaveBeenCalled() // 背景
      expect(scene.add.text).toHaveBeenCalled() // タイトルとラベル
    })

    it('初期設定値が正しく設定される', () => {
      panel = new SettingsPanel(scene, 400, 300)

      expect(panel.getSettings().masterVolume).toBe(0.8)
      expect(panel.getSettings().bgmVolume).toBe(0.7)
      expect(panel.getSettings().seVolume).toBe(0.8)
      expect(panel.getSettings().textSpeed).toBe(1.0)
    })
  })

  describe('設定値管理', () => {
    beforeEach(() => {
      panel = new SettingsPanel(scene, 400, 300)
    })

    it('マスターボリュームを設定できる', () => {
      panel.setMasterVolume(0.5)
      expect(panel.getSettings().masterVolume).toBe(0.5)
    })

    it('BGMボリュームを設定できる', () => {
      panel.setBgmVolume(0.6)
      expect(panel.getSettings().bgmVolume).toBe(0.6)
    })

    it('SEボリュームを設定できる', () => {
      panel.setSeVolume(0.9)
      expect(panel.getSettings().seVolume).toBe(0.9)
    })

    it('テキスト速度を設定できる', () => {
      panel.setTextSpeed(2.0)
      expect(panel.getSettings().textSpeed).toBe(2.0)
    })

    it('フルスクリーンモードを切り替えできる', () => {
      panel.setFullscreen(true)
      expect(panel.getSettings().fullscreen).toBe(true)

      panel.setFullscreen(false)
      expect(panel.getSettings().fullscreen).toBe(false)
    })
  })

  describe('設定の保存と読み込み', () => {
    beforeEach(() => {
      panel = new SettingsPanel(scene, 400, 300)
      // localStorage をモック
      Object.defineProperty(window, 'localStorage', {
        value: {
          getItem: vi.fn(),
          setItem: vi.fn(),
          removeItem: vi.fn(),
        },
        writable: true,
      })
    })

    it('設定を保存できる', () => {
      panel.setMasterVolume(0.5)
      panel.saveSettings()

      expect(localStorage.setItem).toHaveBeenCalledWith(
        'gameSettings',
        expect.stringContaining('"masterVolume":0.5')
      )
    })

    it('設定を読み込みできる', () => {
      const mockSettings = {
        masterVolume: 0.3,
        bgmVolume: 0.4,
        seVolume: 0.5,
        textSpeed: 1.5,
        fullscreen: true,
      }

      ;(localStorage.getItem as ReturnType<typeof vi.fn>).mockReturnValue(
        JSON.stringify(mockSettings)
      )

      panel.loadSettings()

      expect(panel.getSettings()).toEqual(mockSettings)
    })

    it('無効な設定データの場合はデフォルト値を使用', () => {
      ;(localStorage.getItem as ReturnType<typeof vi.fn>).mockReturnValue('invalid-json')

      const newPanel = new SettingsPanel(scene, 400, 300)

      // デフォルト値が維持されることを確認
      expect(newPanel.getSettings().masterVolume).toBe(0.8)
    })
  })

  describe('表示制御', () => {
    beforeEach(() => {
      panel = new SettingsPanel(scene, 400, 300)
    })

    it('パネルを表示/非表示にできる', () => {
      panel.setVisible(false)
      panel.setVisible(true)

      // 各要素の表示状態が変更されることを確認
      const mockRectangle = scene.add.rectangle as ReturnType<typeof vi.fn>

      // 複数回呼ばれているため、最後の呼び出しをチェック
      expect(mockRectangle.mock.results[0].value.setVisible).toHaveBeenCalledWith(false)
      expect(mockRectangle.mock.results[0].value.setVisible).toHaveBeenCalledWith(true)
    })
  })

  describe('設定変更イベント', () => {
    beforeEach(() => {
      panel = new SettingsPanel(scene, 400, 300)
    })

    it('設定変更時にコールバックが呼ばれる', () => {
      const onSettingsChange = vi.fn()
      panel.onSettingsChange = onSettingsChange

      panel.setMasterVolume(0.5)

      expect(onSettingsChange).toHaveBeenCalledWith(expect.objectContaining({ masterVolume: 0.5 }))
    })
  })

  describe('破棄処理', () => {
    beforeEach(() => {
      panel = new SettingsPanel(scene, 400, 300)
    })

    it('パネルが正しく破棄される', () => {
      panel.destroy()

      // 全ての要素が破棄されることを確認
      const mockRectangle = scene.add.rectangle as ReturnType<typeof vi.fn>
      const mockText = scene.add.text as ReturnType<typeof vi.fn>

      mockRectangle.mock.results.forEach((result) => {
        expect(result.value.destroy).toHaveBeenCalled()
      })

      mockText.mock.results.forEach((result) => {
        expect(result.value.destroy).toHaveBeenCalled()
      })
    })
  })
})
