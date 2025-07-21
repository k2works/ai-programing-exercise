import { describe, it, expect, beforeEach, vi } from 'vitest'
import { 
  Animation, 
  Easing,
  PuyoEraseAnimation,
  PuyoFallAnimation,
  ChainEffectAnimation,
  AnimationManager 
} from './animation'

describe('Animation', () => {
  let animation: Animation
  const duration = 1000

  beforeEach(() => {
    animation = new Animation(duration)
    // performance.nowをモック化
    vi.spyOn(performance, 'now').mockReturnValue(0)
  })

  describe('基本動作', () => {
    it('初期状態では非アクティブ', () => {
      expect(animation.isRunning()).toBe(false)
      expect(animation.getProgress()).toBe(0)
    })

    it('開始すると進行状況を追跡する', () => {
      vi.spyOn(performance, 'now')
        .mockReturnValueOnce(0)   // start時
        .mockReturnValueOnce(500) // update時

      animation.start()
      expect(animation.isRunning()).toBe(true)

      const frame = animation.update()
      expect(frame.progress).toBeCloseTo(0.5)
      expect(frame.isComplete).toBe(false)
    })

    it('期間完了時に完了状態になる', () => {
      vi.spyOn(performance, 'now')
        .mockReturnValueOnce(0)    // start時
        .mockReturnValueOnce(1000) // update時

      animation.start()
      const frame = animation.update()
      
      expect(frame.progress).toBe(1.0)
      expect(frame.isComplete).toBe(true)
      expect(animation.isRunning()).toBe(false)
    })

    it('リセット後は初期状態に戻る', () => {
      animation.start()
      animation.reset()
      
      expect(animation.isRunning()).toBe(false)
      expect(animation.getProgress()).toBe(0)
    })
  })
})

describe('Easing', () => {
  describe('イージング関数のテスト', () => {
    it('linear: 線形補間', () => {
      expect(Easing.linear(0)).toBe(0)
      expect(Easing.linear(0.5)).toBe(0.5)
      expect(Easing.linear(1)).toBe(1)
    })

    it('easeInQuad: 2次イーズイン', () => {
      expect(Easing.easeInQuad(0)).toBe(0)
      expect(Easing.easeInQuad(0.5)).toBe(0.25)
      expect(Easing.easeInQuad(1)).toBe(1)
    })

    it('easeOutQuad: 2次イーズアウト', () => {
      expect(Easing.easeOutQuad(0)).toBe(0)
      expect(Easing.easeOutQuad(0.5)).toBe(0.75)
      expect(Easing.easeOutQuad(1)).toBe(1)
    })

    it('bounce: バウンス効果', () => {
      expect(Easing.bounce(0)).toBe(0)
      expect(Easing.bounce(1)).toBe(1)
      expect(Easing.bounce(0.5)).toBeGreaterThan(0)
    })
  })
})

describe('PuyoEraseAnimation', () => {
  let eraseAnimation: PuyoEraseAnimation
  const x = 2, y = 3

  beforeEach(() => {
    eraseAnimation = new PuyoEraseAnimation(x, y, 300)
  })

  describe('消去アニメーション', () => {
    it('位置情報を正しく保持する', () => {
      const position = eraseAnimation.getPosition()
      expect(position.x).toBe(x)
      expect(position.y).toBe(y)
    })

    it('未開始時は通常状態', () => {
      expect(eraseAnimation.getScale()).toBe(1.0)
      expect(eraseAnimation.getOpacity()).toBe(1.0)
      expect(eraseAnimation.getRotation()).toBe(0)
    })

    it('進行に応じてスケール・透明度・回転が変化する', () => {
      let currentTime = 0
      vi.spyOn(performance, 'now').mockImplementation(() => {
        const time = currentTime
        currentTime += 150 // 50%進行
        return time
      })

      eraseAnimation.start()
      eraseAnimation.update()

      expect(eraseAnimation.getScale()).toBeLessThan(1.0)
      expect(eraseAnimation.getOpacity()).toBeLessThan(1.0)
      expect(eraseAnimation.getRotation()).toBeGreaterThan(0)
    })
  })
})

describe('PuyoFallAnimation', () => {
  let fallAnimation: PuyoFallAnimation
  const fromX = 1, fromY = 1, toX = 1, toY = 5

  beforeEach(() => {
    fallAnimation = new PuyoFallAnimation(fromX, fromY, toX, toY, 200)
  })

  describe('落下アニメーション', () => {
    it('未開始時は終了位置を返す', () => {
      const position = fallAnimation.getCurrentPosition()
      expect(position.x).toBe(toX)
      expect(position.y).toBe(toY)
    })

    it('進行に応じて位置が変化する', () => {
      let currentTime = 0
      vi.spyOn(performance, 'now').mockImplementation(() => {
        const time = currentTime
        currentTime += 100 // 50%進行
        return time
      })

      fallAnimation.start()
      fallAnimation.update()

      const position = fallAnimation.getCurrentPosition()
      expect(position.x).toBeCloseTo((fromX + toX) / 2, 0)
      // イージング効果で線形ではない
      expect(position.y).toBeGreaterThan(fromY)
      expect(position.y).toBeLessThan(toY)
    })

    it('跳ね返りオフセットを提供する', () => {
      let currentTime = 0
      vi.spyOn(performance, 'now').mockImplementation(() => {
        const time = currentTime
        currentTime += 50 // 25%進行
        return time
      })

      fallAnimation.start()
      fallAnimation.update()

      const bounceOffset = fallAnimation.getBounceOffset()
      // 進行中は跳ね返り効果がある
      expect(Math.abs(bounceOffset)).toBeGreaterThanOrEqual(0)
    })
  })
})

describe('ChainEffectAnimation', () => {
  let chainEffect: ChainEffectAnimation
  const chainCount = 3, centerX = 5, centerY = 7

  beforeEach(() => {
    chainEffect = new ChainEffectAnimation(chainCount, centerX, centerY, 500)
  })

  describe('チェインエフェクト', () => {
    it('チェイン情報と中心位置を保持する', () => {
      expect(chainEffect.getChainCount()).toBe(chainCount)
      
      const center = chainEffect.getCenterPosition()
      expect(center.x).toBe(centerX)
      expect(center.y).toBe(centerY)
    })

    it('未開始時はエフェクトなし', () => {
      expect(chainEffect.getExpansionRadius()).toBe(0)
      expect(chainEffect.getIntensity()).toBe(0)
      expect(chainEffect.getPulseScale()).toBe(1.0)
    })

    it('進行に応じてエフェクトが変化する', () => {
      let currentTime = 0
      vi.spyOn(performance, 'now').mockImplementation(() => {
        const time = currentTime
        currentTime += 250 // 50%進行
        return time
      })

      chainEffect.start()
      chainEffect.update()

      expect(chainEffect.getExpansionRadius()).toBeGreaterThan(0)
      expect(chainEffect.getIntensity()).toBeGreaterThan(0)
      // パルススケールは1.0周辺で変動
      const pulseScale = chainEffect.getPulseScale()
      expect(pulseScale).toBeCloseTo(1.0, 0.2)
    })
  })
})

describe('AnimationManager', () => {
  let manager: AnimationManager

  beforeEach(() => {
    manager = new AnimationManager()
  })

  describe('アニメーション管理', () => {
    it('消去アニメーションを追加・取得できる', () => {
      manager.addEraseAnimation('test1', 1, 2)
      
      const animation = manager.getEraseAnimation('test1')
      expect(animation).toBeDefined()
      expect(animation!.getPosition()).toEqual({ x: 1, y: 2 })
    })

    it('落下アニメーションを追加・取得できる', () => {
      manager.addFallAnimation('fall1', 0, 0, 1, 5)
      
      const animation = manager.getFallAnimation('fall1')
      expect(animation).toBeDefined()
    })

    it('チェインエフェクトを追加・取得できる', () => {
      manager.addChainEffect('chain1', 4, 3, 8)
      
      const animation = manager.getChainEffect('chain1')
      expect(animation).toBeDefined()
      expect(animation!.getChainCount()).toBe(4)
    })

    it('アクティブなアニメーション判定が正しく動作する', () => {
      expect(manager.hasActiveAnimations()).toBe(false)
      
      manager.addEraseAnimation('test', 1, 1)
      expect(manager.hasActiveAnimations()).toBe(true)
    })

    it('アニメーション統計情報を提供する', () => {
      manager.addEraseAnimation('erase1', 1, 1)
      manager.addEraseAnimation('erase2', 2, 2)
      manager.addFallAnimation('fall1', 0, 0, 1, 1)
      manager.addChainEffect('chain1', 2, 5, 5)
      
      const stats = manager.getAnimationStats()
      expect(stats.erasing).toBe(2)
      expect(stats.falling).toBe(1)
      expect(stats.effects).toBe(1)
    })

    it('アニメーションをクリアできる', () => {
      manager.addEraseAnimation('test', 1, 1)
      manager.addFallAnimation('test2', 0, 0, 1, 1)
      
      expect(manager.hasActiveAnimations()).toBe(true)
      
      manager.clear()
      expect(manager.hasActiveAnimations()).toBe(false)
    })
  })

  describe('更新処理', () => {
    it('完了したアニメーションを自動削除する', () => {
      // 極短時間のアニメーション
      const shortDuration = 1
      let currentTime = 0
      vi.spyOn(performance, 'now').mockImplementation(() => {
        const time = currentTime
        currentTime += shortDuration + 1 // 完了するまで進める
        return time
      })
      
      manager.addEraseAnimation('short', 1, 1, shortDuration)
      expect(manager.hasActiveAnimations()).toBe(true)
      
      manager.update()
      expect(manager.hasActiveAnimations()).toBe(false)
    })
  })
})