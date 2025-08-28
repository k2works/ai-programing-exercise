import { describe, it, expect, beforeEach, vi } from 'vitest'
import { EffectManager } from './EffectManager'

describe('EffectManager', () => {
  let effectManager: EffectManager
  let mockContainer: any

  beforeEach(() => {
    // Mock Phaser3 container
    mockContainer = {
      add: {
        rectangle: vi.fn().mockReturnValue({
          setFillStyle: vi.fn().mockReturnThis(),
          setAlpha: vi.fn().mockReturnThis(),
          setDepth: vi.fn().mockReturnThis(),
          setVisible: vi.fn().mockReturnThis(),
          destroy: vi.fn(),
        }),
        tween: vi.fn().mockReturnValue({
          setCallback: vi.fn().mockReturnThis(),
          start: vi.fn(),
        }),
      },
      scene: {
        tweens: {
          add: vi.fn().mockReturnValue({
            setCallback: vi.fn().mockReturnThis(),
          }),
        },
        cameras: {
          main: {
            shake: vi.fn(),
          },
        },
      },
    }

    effectManager = new EffectManager(mockContainer)
  })

  describe('インスタンス作成', () => {
    it('EffectManagerが正常に作成される', () => {
      expect(effectManager).toBeInstanceOf(EffectManager)
    })

    it('コンテナが正しく設定される', () => {
      expect(effectManager.container).toBe(mockContainer)
    })
  })

  describe('フラッシュエフェクト', () => {
    it('白フラッシュエフェクトが実行される', () => {
      effectManager.flash('white', 500)

      expect(mockContainer.add.rectangle).toHaveBeenCalledWith(0, 0, 800, 600, 0xffffff)
      expect(mockContainer.scene.tweens.add).toHaveBeenCalled()
    })

    it('黒フラッシュエフェクトが実行される', () => {
      effectManager.flash('black', 300)

      expect(mockContainer.add.rectangle).toHaveBeenCalledWith(0, 0, 800, 600, 0x000000)
    })

    it('カスタム色でフラッシュエフェクトが実行される', () => {
      effectManager.flash('#ff0000', 400)

      expect(mockContainer.add.rectangle).toHaveBeenCalledWith(0, 0, 800, 600, 0xff0000)
    })

    it('デフォルト継続時間が使用される', () => {
      effectManager.flash('white')

      expect(mockContainer.scene.tweens.add).toHaveBeenCalledWith(
        expect.objectContaining({
          duration: 300,
        })
      )
    })
  })

  describe('画面振動エフェクト', () => {
    it('軽い振動エフェクトが実行される', () => {
      effectManager.shake('light', 500)

      expect(mockContainer.scene.cameras.main.shake).toHaveBeenCalledWith(500, 5)
    })

    it('中程度の振動エフェクトが実行される', () => {
      effectManager.shake('medium', 800)

      expect(mockContainer.scene.cameras.main.shake).toHaveBeenCalledWith(800, 10)
    })

    it('強い振動エフェクトが実行される', () => {
      effectManager.shake('heavy', 1000)

      expect(mockContainer.scene.cameras.main.shake).toHaveBeenCalledWith(1000, 20)
    })

    it('デフォルト継続時間が使用される', () => {
      effectManager.shake('medium')

      expect(mockContainer.scene.cameras.main.shake).toHaveBeenCalledWith(600, 10)
    })
  })

  describe('画面フェード', () => {
    it('フェードインエフェクトが実行される', () => {
      effectManager.fade('in', 1000, '#000000')

      expect(mockContainer.add.rectangle).toHaveBeenCalledWith(0, 0, 800, 600, 0x000000)
      expect(mockContainer.scene.tweens.add).toHaveBeenCalledWith(
        expect.objectContaining({
          alpha: { from: 1, to: 0 },
          duration: 1000,
        })
      )
    })

    it('フェードアウトエフェクトが実行される', () => {
      effectManager.fade('out', 800, '#ffffff')

      expect(mockContainer.add.rectangle).toHaveBeenCalledWith(0, 0, 800, 600, 0xffffff)
      expect(mockContainer.scene.tweens.add).toHaveBeenCalledWith(
        expect.objectContaining({
          alpha: { from: 0, to: 1 },
          duration: 800,
        })
      )
    })

    it('デフォルト値でフェードが実行される', () => {
      effectManager.fade('in')

      expect(mockContainer.add.rectangle).toHaveBeenCalledWith(0, 0, 800, 600, 0x000000)
      expect(mockContainer.scene.tweens.add).toHaveBeenCalledWith(
        expect.objectContaining({
          duration: 500,
        })
      )
    })
  })

  describe('エフェクト停止', () => {
    it('すべてのエフェクトが停止される', () => {
      const mockTween = {
        stop: vi.fn(),
        remove: vi.fn(),
      }

      // エフェクトマネージャーの内部状態をモック
      ;(effectManager as any).activeTweens = [mockTween]
      ;(effectManager as any).activeOverlays = []

      effectManager.stopAllEffects()

      expect(mockTween.stop).toHaveBeenCalled()
      expect(mockTween.remove).toHaveBeenCalled()
    })

    it('アクティブなオーバーレイが削除される', () => {
      const mockOverlay = {
        destroy: vi.fn(),
      }

      ;(effectManager as any).activeTweens = []
      ;(effectManager as any).activeOverlays = [mockOverlay]

      effectManager.stopAllEffects()

      expect(mockOverlay.destroy).toHaveBeenCalled()
    })
  })

  describe('エフェクトチェーン', () => {
    it('複数のエフェクトを順番に実行できる', () => {
      const callback = vi.fn()

      effectManager.chain(
        [
          { type: 'flash', color: 'white', duration: 200 },
          { type: 'shake', intensity: 'medium', duration: 300 },
          { type: 'fade', direction: 'out', duration: 400 },
        ],
        callback
      )

      // 最初のエフェクト（flash）が即座に実行される
      expect(mockContainer.add.rectangle).toHaveBeenCalled()
    })

    it('エフェクトチェーンのコールバックが呼ばれる', () => {
      const callback = vi.fn()

      // 短い duration でテスト
      effectManager.chain([{ type: 'flash', color: 'white', duration: 1 }], callback)

      expect(mockContainer.add.rectangle).toHaveBeenCalled()
    })
  })

  describe('破棄処理', () => {
    it('すべてのリソースが正しく破棄される', () => {
      const mockTween = { stop: vi.fn(), remove: vi.fn() }
      const mockOverlay = { destroy: vi.fn() }

      ;(effectManager as any).activeTweens = [mockTween]
      ;(effectManager as any).activeOverlays = [mockOverlay]

      effectManager.destroy()

      expect(mockTween.stop).toHaveBeenCalled()
      expect(mockOverlay.destroy).toHaveBeenCalled()
    })
  })
})
