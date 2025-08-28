import { describe, it, expect, beforeEach, vi } from 'vitest'
import { BackgroundManager } from './BackgroundManager'

describe('BackgroundManager', () => {
  let backgroundManager: BackgroundManager
  let mockScene: any
  let mockCurrentBackground: any

  beforeEach(() => {
    mockCurrentBackground = {
      setTexture: vi.fn().mockReturnThis(),
      setAlpha: vi.fn().mockReturnThis(),
      setVisible: vi.fn().mockReturnThis(),
      setDepth: vi.fn().mockReturnThis(),
      setPosition: vi.fn().mockReturnThis(),
      setScrollFactor: vi.fn().mockReturnThis(),
      destroy: vi.fn(),
      alpha: 1,
    }

    mockScene = {
      add: {
        image: vi.fn().mockReturnValue(mockCurrentBackground),
        rectangle: vi.fn().mockReturnValue({
          setFillStyle: vi.fn().mockReturnThis(),
          setAlpha: vi.fn().mockReturnThis(),
          setDepth: vi.fn().mockReturnThis(),
          destroy: vi.fn(),
        }),
      },
      tweens: {
        add: vi.fn().mockReturnValue({
          setCallback: vi.fn().mockReturnThis(),
          stop: vi.fn(),
          remove: vi.fn(),
        }),
      },
    }

    backgroundManager = new BackgroundManager(mockScene)
  })

  describe('インスタンス作成', () => {
    it('BackgroundManagerが正常に作成される', () => {
      expect(backgroundManager).toBeInstanceOf(BackgroundManager)
    })

    it('シーンが正しく設定される', () => {
      expect(backgroundManager.scene).toBe(mockScene)
    })
  })

  describe('背景画像設定', () => {
    it('初回背景設定が実行される', () => {
      backgroundManager.setBackground('bg_forest')

      expect(mockScene.add.image).toHaveBeenCalledWith(400, 300, 'bg_forest')
      expect(mockCurrentBackground.setAlpha).toHaveBeenCalledWith(1)
    })

    it('背景画像が即座に変更される', () => {
      // 最初の背景を設定
      backgroundManager.setBackground('bg_forest')

      // 2番目の背景に変更
      backgroundManager.setBackground('bg_mountain', 0)

      expect(mockScene.add.image).toHaveBeenCalledWith(400, 300, 'bg_mountain')
    })

    it('フェード付きで背景画像が変更される', () => {
      // 最初の背景を設定
      backgroundManager.setBackground('bg_forest')

      // フェード付きで変更
      backgroundManager.setBackground('bg_mountain', 1000)

      expect(mockScene.add.image).toHaveBeenCalledWith(400, 300, 'bg_mountain')
      expect(mockScene.tweens.add).toHaveBeenCalled()
    })

    it('コールバック付きで背景画像が変更される', () => {
      const callback = vi.fn()

      backgroundManager.setBackground('bg_forest', 500, callback)

      expect(mockScene.add.image).toHaveBeenCalledWith(400, 300, 'bg_forest')
    })
  })

  describe('背景クリア', () => {
    it('背景画像がクリアされる', () => {
      // 背景を設定してからクリア
      backgroundManager.setBackground('bg_forest')
      backgroundManager.clearBackground()

      expect(mockCurrentBackground.destroy).toHaveBeenCalled()
    })

    it('フェード付きで背景画像がクリアされる', () => {
      backgroundManager.setBackground('bg_forest')
      backgroundManager.clearBackground(800)

      expect(mockScene.tweens.add).toHaveBeenCalled()
    })

    it('背景が存在しない場合でもクリア処理が安全に実行される', () => {
      // 背景未設定でクリア
      expect(() => backgroundManager.clearBackground()).not.toThrow()
    })
  })

  describe('背景フェード', () => {
    it('フェードアウトが実行される', () => {
      backgroundManager.setBackground('bg_forest')
      backgroundManager.fadeOut(600)

      expect(mockScene.tweens.add).toHaveBeenCalledWith(
        expect.objectContaining({
          targets: mockCurrentBackground,
          alpha: 0,
          duration: 600,
        })
      )
    })

    it('フェードインが実行される', () => {
      backgroundManager.setBackground('bg_forest')
      mockCurrentBackground.alpha = 0 // 初期状態を透明に

      backgroundManager.fadeIn(500)

      expect(mockScene.tweens.add).toHaveBeenCalledWith(
        expect.objectContaining({
          targets: mockCurrentBackground,
          alpha: 1,
          duration: 500,
        })
      )
    })

    it('指定透明度でフェードが実行される', () => {
      backgroundManager.setBackground('bg_forest')
      backgroundManager.fadeTo(0.5, 400)

      expect(mockScene.tweens.add).toHaveBeenCalledWith(
        expect.objectContaining({
          targets: mockCurrentBackground,
          alpha: 0.5,
          duration: 400,
        })
      )
    })

    it('背景が存在しない場合はフェード処理がスキップされる', () => {
      expect(() => backgroundManager.fadeOut()).not.toThrow()
      expect(mockScene.tweens.add).not.toHaveBeenCalled()
    })
  })

  describe('背景色設定', () => {
    it('単色背景が設定される', () => {
      backgroundManager.setBackgroundColor('#ff0000')

      expect(mockScene.add.rectangle).toHaveBeenCalledWith(400, 300, 800, 600, 0xff0000)
    })

    it('透明度付きで単色背景が設定される', () => {
      backgroundManager.setBackgroundColor('#00ff00', 0.7)

      expect(mockScene.add.rectangle).toHaveBeenCalledWith(400, 300, 800, 600, 0x00ff00)
    })

    it('フェード付きで単色背景が設定される', () => {
      backgroundManager.setBackgroundColor('#0000ff', 1.0, 800)

      expect(mockScene.add.rectangle).toHaveBeenCalledWith(400, 300, 800, 600, 0x0000ff)
      expect(mockScene.tweens.add).toHaveBeenCalled()
    })
  })

  describe('背景パララックス', () => {
    it('パララックス効果が設定される', () => {
      backgroundManager.setBackground('bg_forest')
      backgroundManager.setParallax(0.5, 0.3)

      // パララックス設定の検証（実装依存）
      expect(backgroundManager.parallaxX).toBe(0.5)
      expect(backgroundManager.parallaxY).toBe(0.3)
    })

    it('パララックス効果が更新される', () => {
      backgroundManager.setBackground('bg_forest')
      backgroundManager.setParallax(0.5, 0.3)

      backgroundManager.updateParallax(100, 50)

      // パララックス更新の検証（実装依存）
      expect(mockCurrentBackground.setPosition).toHaveBeenCalled()
    })
  })

  describe('アニメーション制御', () => {
    it('すべてのアニメーションが停止される', () => {
      const mockTween = { stop: vi.fn(), remove: vi.fn() }
      ;(backgroundManager as any).activeTweens = [mockTween]

      backgroundManager.stopAllAnimations()

      expect(mockTween.stop).toHaveBeenCalled()
      expect(mockTween.remove).toHaveBeenCalled()
    })
  })

  describe('破棄処理', () => {
    it('すべてのリソースが正しく破棄される', () => {
      backgroundManager.setBackground('bg_forest')
      const mockTween = { stop: vi.fn(), remove: vi.fn() }
      ;(backgroundManager as any).activeTweens = [mockTween]

      backgroundManager.destroy()

      expect(mockCurrentBackground.destroy).toHaveBeenCalled()
      expect(mockTween.stop).toHaveBeenCalled()
    })
  })
})
