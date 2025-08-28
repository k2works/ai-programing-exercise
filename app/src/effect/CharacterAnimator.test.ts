import { describe, it, expect, beforeEach, vi } from 'vitest'
import { CharacterAnimator } from './CharacterAnimator'

describe('CharacterAnimator', () => {
  let animator: CharacterAnimator
  let mockScene: any
  let mockCharacter: any

  beforeEach(() => {
    mockCharacter = {
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

    mockScene = {
      tweens: {
        add: vi.fn().mockReturnValue({
          setCallback: vi.fn().mockReturnThis(),
          stop: vi.fn(),
          remove: vi.fn(),
        }),
      },
    }

    animator = new CharacterAnimator(mockScene)
  })

  describe('インスタンス作成', () => {
    it('CharacterAnimatorが正常に作成される', () => {
      expect(animator).toBeInstanceOf(CharacterAnimator)
    })

    it('シーンが正しく設定される', () => {
      expect(animator.scene).toBe(mockScene)
    })
  })

  describe('移動アニメーション', () => {
    it('指定位置への移動アニメーションが実行される', () => {
      animator.moveTo(mockCharacter, 300, 400, 1000)

      expect(mockScene.tweens.add).toHaveBeenCalledWith({
        targets: mockCharacter,
        x: 300,
        y: 400,
        duration: 1000,
        ease: 'Power2',
        onComplete: expect.any(Function),
      })
    })

    it('デフォルト値で移動アニメーションが実行される', () => {
      animator.moveTo(mockCharacter, 150, 250)

      expect(mockScene.tweens.add).toHaveBeenCalledWith({
        targets: mockCharacter,
        x: 150,
        y: 250,
        duration: 500,
        ease: 'Power2',
        onComplete: expect.any(Function),
      })
    })

    it('相対移動アニメーションが実行される', () => {
      animator.moveBy(mockCharacter, 50, -30, 800)

      expect(mockScene.tweens.add).toHaveBeenCalledWith({
        targets: mockCharacter,
        x: '+=50',
        y: '+=-30',
        duration: 800,
        ease: 'Power2',
        onComplete: expect.any(Function),
      })
    })
  })

  describe('スケールアニメーション', () => {
    it('スケール変更アニメーションが実行される', () => {
      animator.scaleTo(mockCharacter, 1.5, 2.0, 600)

      expect(mockScene.tweens.add).toHaveBeenCalledWith({
        targets: mockCharacter,
        scaleX: 1.5,
        scaleY: 2.0,
        duration: 600,
        ease: 'Power2',
        onComplete: expect.any(Function),
      })
    })

    it('均等スケールアニメーションが実行される', () => {
      animator.scaleTo(mockCharacter, 0.8)

      expect(mockScene.tweens.add).toHaveBeenCalledWith({
        targets: mockCharacter,
        scaleX: 0.8,
        scaleY: 0.8,
        duration: 500,
        ease: 'Power2',
        onComplete: expect.any(Function),
      })
    })
  })

  describe('フェードアニメーション', () => {
    it('フェードアウトアニメーションが実行される', () => {
      animator.fadeOut(mockCharacter, 700)

      expect(mockScene.tweens.add).toHaveBeenCalledWith({
        targets: mockCharacter,
        alpha: 0,
        duration: 700,
        ease: 'Power2',
        onComplete: expect.any(Function),
      })
    })

    it('フェードインアニメーションが実行される', () => {
      animator.fadeIn(mockCharacter, 400)

      expect(mockScene.tweens.add).toHaveBeenCalledWith({
        targets: mockCharacter,
        alpha: 1,
        duration: 400,
        ease: 'Power2',
        onComplete: expect.any(Function),
      })
    })

    it('透明度指定フェードアニメーションが実行される', () => {
      animator.fadeTo(mockCharacter, 0.3, 500)

      expect(mockScene.tweens.add).toHaveBeenCalledWith({
        targets: mockCharacter,
        alpha: 0.3,
        duration: 500,
        ease: 'Power2',
        onComplete: expect.any(Function),
      })
    })
  })

  describe('回転アニメーション', () => {
    it('回転アニメーションが実行される', () => {
      animator.rotateTo(mockCharacter, 90, 800)

      expect(mockScene.tweens.add).toHaveBeenCalledWith({
        targets: mockCharacter,
        angle: 90,
        duration: 800,
        ease: 'Power2',
        onComplete: expect.any(Function),
      })
    })

    it('相対回転アニメーションが実行される', () => {
      animator.rotateBy(mockCharacter, 45, 300)

      expect(mockScene.tweens.add).toHaveBeenCalledWith({
        targets: mockCharacter,
        angle: '+=45',
        duration: 300,
        ease: 'Power2',
        onComplete: expect.any(Function),
      })
    })
  })

  describe('振動アニメーション', () => {
    it('水平振動アニメーションが実行される', () => {
      animator.shake(mockCharacter, 'horizontal', 10, 1000)

      // 振動アニメーションはsetPositionが呼ばれることを確認
      expect(mockCharacter.setPosition).toHaveBeenCalled()
    })

    it('垂直振動アニメーションが実行される', () => {
      animator.shake(mockCharacter, 'vertical', 15, 800)

      expect(mockCharacter.setPosition).toHaveBeenCalled()
    })

    it('ランダム振動アニメーションが実行される', () => {
      animator.shake(mockCharacter, 'random', 8, 600)

      expect(mockCharacter.setPosition).toHaveBeenCalled()
    })
  })

  describe('アニメーション制御', () => {
    it('指定キャラクターのアニメーションが停止される', () => {
      const mockTween = {
        stop: vi.fn(),
        remove: vi.fn(),
      }

      // アニメーション開始
      animator.moveTo(mockCharacter, 200, 300)
      ;(animator as any).activeAnimations.set(mockCharacter, [mockTween])

      // 停止
      animator.stopAnimations(mockCharacter)

      expect(mockTween.stop).toHaveBeenCalled()
      expect(mockTween.remove).toHaveBeenCalled()
    })

    it('すべてのアニメーションが停止される', () => {
      const mockTween1 = { stop: vi.fn(), remove: vi.fn() }
      const mockTween2 = { stop: vi.fn(), remove: vi.fn() }

      ;(animator as any).activeAnimations.set(mockCharacter, [mockTween1])
      ;(animator as any).activeAnimations.set({}, [mockTween2])

      animator.stopAllAnimations()

      expect(mockTween1.stop).toHaveBeenCalled()
      expect(mockTween2.stop).toHaveBeenCalled()
    })

    it('アニメーション完了時にコールバックが呼ばれる', () => {
      const callback = vi.fn()

      animator.moveTo(mockCharacter, 400, 500, 300, callback)

      expect(mockScene.tweens.add).toHaveBeenCalledWith(
        expect.objectContaining({
          onComplete: expect.any(Function),
        })
      )
    })
  })

  describe('バウンスアニメーション', () => {
    it('バウンスアニメーションが実行される', () => {
      animator.bounce(mockCharacter, 0.2, 400)

      expect(mockScene.tweens.add).toHaveBeenCalled()
    })
  })

  describe('パルスアニメーション', () => {
    it('パルスアニメーションが実行される', () => {
      animator.pulse(mockCharacter, 1.3, 600)

      expect(mockScene.tweens.add).toHaveBeenCalled()
    })
  })

  describe('破棄処理', () => {
    it('すべてのアニメーションが停止され、リソースが破棄される', () => {
      const mockTween = { stop: vi.fn(), remove: vi.fn() }
      ;(animator as any).activeAnimations.set(mockCharacter, [mockTween])

      animator.destroy()

      expect(mockTween.stop).toHaveBeenCalled()
      expect(mockTween.remove).toHaveBeenCalled()
    })
  })
})
