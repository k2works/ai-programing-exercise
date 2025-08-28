import { describe, it, expect, beforeEach, vi } from 'vitest'
import { IntegratedEffectManager } from './index'

describe('IntegratedEffectManager', () => {
  let integratedManager: IntegratedEffectManager
  let mockContainer: any
  let mockScene: any

  beforeEach(() => {
    mockContainer = {
      add: {
        rectangle: vi.fn().mockReturnValue({
          setFillStyle: vi.fn().mockReturnThis(),
          setAlpha: vi.fn().mockReturnThis(),
          setDepth: vi.fn().mockReturnThis(),
          setVisible: vi.fn().mockReturnThis(),
          destroy: vi.fn(),
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

    mockScene = {
      add: {
        image: vi.fn().mockReturnValue({
          setDepth: vi.fn().mockReturnThis(),
          setAlpha: vi.fn().mockReturnThis(),
          setPosition: vi.fn().mockReturnThis(),
          setScrollFactor: vi.fn().mockReturnThis(),
          destroy: vi.fn(),
          alpha: 1,
        }),
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
      cameras: {
        main: {
          shake: vi.fn(),
        },
      },
    }

    integratedManager = new IntegratedEffectManager(mockContainer, mockScene)
  })

  describe('インスタンス作成', () => {
    it('IntegratedEffectManagerが正常に作成される', () => {
      expect(integratedManager).toBeInstanceOf(IntegratedEffectManager)
    })

    it('各マネージャーのインスタンスにアクセスできる', () => {
      expect(integratedManager.effect).toBeDefined()
      expect(integratedManager.character).toBeDefined()
      expect(integratedManager.background).toBeDefined()
    })
  })

  describe('統合操作', () => {
    it('画面エフェクトが実行される', () => {
      integratedManager.effect.flash('white', 500)

      expect(mockContainer.add.rectangle).toHaveBeenCalled()
    })

    it('キャラクターアニメーションが実行される', () => {
      const mockCharacter = {
        x: 100,
        y: 200,
        scaleX: 1,
        scaleY: 1,
        alpha: 1,
        angle: 0,
        setPosition: vi.fn().mockReturnThis(),
        setScale: vi.fn().mockReturnThis(),
        setAlpha: vi.fn().mockReturnThis(),
        setAngle: vi.fn().mockReturnThis(),
      }

      integratedManager.character.moveTo(mockCharacter, 300, 400)

      expect(mockScene.tweens.add).toHaveBeenCalled()
    })

    it('背景変更が実行される', () => {
      integratedManager.background.setBackground('bg_forest')

      expect(mockScene.add.image).toHaveBeenCalledWith(400, 300, 'bg_forest')
    })
  })

  describe('一括制御', () => {
    it('すべてのエフェクトが停止される', () => {
      // 各マネージャーのstopメソッドをモック
      const stopAllEffectsSpy = vi.spyOn(integratedManager.effect, 'stopAllEffects')
      const stopAllAnimationsSpy = vi.spyOn(integratedManager.character, 'stopAllAnimations')
      const stopAllBgAnimationsSpy = vi.spyOn(integratedManager.background, 'stopAllAnimations')

      integratedManager.stopAll()

      expect(stopAllEffectsSpy).toHaveBeenCalled()
      expect(stopAllAnimationsSpy).toHaveBeenCalled()
      expect(stopAllBgAnimationsSpy).toHaveBeenCalled()
    })

    it('すべてのリソースが破棄される', () => {
      // 各マネージャーのdestroyメソッドをモック
      const destroyEffectSpy = vi.spyOn(integratedManager.effect, 'destroy')
      const destroyCharacterSpy = vi.spyOn(integratedManager.character, 'destroy')
      const destroyBackgroundSpy = vi.spyOn(integratedManager.background, 'destroy')

      integratedManager.destroy()

      expect(destroyEffectSpy).toHaveBeenCalled()
      expect(destroyCharacterSpy).toHaveBeenCalled()
      expect(destroyBackgroundSpy).toHaveBeenCalled()
    })
  })

  describe('複合エフェクト', () => {
    it('複数のエフェクトを組み合わせて実行できる', () => {
      const mockCharacter = {
        x: 100,
        y: 200,
        scaleX: 1,
        scaleY: 1,
        alpha: 1,
        angle: 0,
        setPosition: vi.fn().mockReturnThis(),
        setScale: vi.fn().mockReturnThis(),
        setAlpha: vi.fn().mockReturnThis(),
        setAngle: vi.fn().mockReturnThis(),
      }

      // 背景変更 + 画面フラッシュ + キャラクター移動
      integratedManager.background.setBackground('bg_mountain')
      integratedManager.effect.flash('white', 300)
      integratedManager.character.moveTo(mockCharacter, 400, 300)

      expect(mockScene.add.image).toHaveBeenCalled()
      expect(mockContainer.add.rectangle).toHaveBeenCalled()
      expect(mockScene.tweens.add).toHaveBeenCalled()
    })
  })
})
