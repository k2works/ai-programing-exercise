import { describe, it, expect, beforeEach, vi, afterEach } from 'vitest'
import { EnhancedAnimationManager, Easing, ParticleSystem } from './enhanced-animation'
import { settingsManager } from './settings'

// settingsManagerをモック化
vi.mock('./settings', () => ({
  settingsManager: {
    get: vi.fn()
  }
}))

const mockSettingsManager = settingsManager as any

describe('Easing', () => {
  describe('基本イージング関数', () => {
    it('linear関数が正しく動作する', () => {
      expect(Easing.linear(0)).toBe(0)
      expect(Easing.linear(0.5)).toBe(0.5)
      expect(Easing.linear(1)).toBe(1)
    })

    it('easeInQuad関数が正しく動作する', () => {
      expect(Easing.easeInQuad(0)).toBe(0)
      expect(Easing.easeInQuad(0.5)).toBe(0.25)
      expect(Easing.easeInQuad(1)).toBe(1)
    })

    it('easeOutQuad関数が正しく動作する', () => {
      expect(Easing.easeOutQuad(0)).toBe(0)
      expect(Easing.easeOutQuad(0.5)).toBe(0.75)
      expect(Easing.easeOutQuad(1)).toBe(1)
    })

    it('easeInOutQuad関数が正しく動作する', () => {
      expect(Easing.easeInOutQuad(0)).toBe(0)
      expect(Easing.easeInOutQuad(1)).toBe(1)
      const midValue = Easing.easeInOutQuad(0.5)
      expect(midValue).toBeCloseTo(0.5, 5)
    })
  })

  describe('高度なイージング関数', () => {
    it('easeOutBounce関数が正しく動作する', () => {
      expect(Easing.easeOutBounce(0)).toBe(0)
      expect(Easing.easeOutBounce(1)).toBe(1)
      const bounceValue = Easing.easeOutBounce(0.5)
      expect(bounceValue).toBeGreaterThan(0)
      expect(bounceValue).toBeLessThan(1)
    })

    it('easeOutElastic関数が境界値で正しく動作する', () => {
      expect(Easing.easeOutElastic(0)).toBe(0)
      expect(Easing.easeOutElastic(1)).toBe(1)
    })
  })
})

describe('ParticleSystem', () => {
  let particleSystem: ParticleSystem

  beforeEach(() => {
    particleSystem = new ParticleSystem()
  })

  it('パーティクルシステムを作成できる', () => {
    expect(particleSystem).toBeInstanceOf(ParticleSystem)
  })

  it('バーストエフェクトを作成できる', () => {
    particleSystem.createBurstEffect(100, 100, '#ff0000', 5)
    
    // プライベートメンバにアクセスするための型キャスト
    const particles = (particleSystem as any).particles
    expect(particles.length).toBe(5)
    
    particles.forEach((particle: any) => {
      expect(particle.x).toBe(100)
      expect(particle.y).toBe(100)
      expect(particle.color).toBe('#ff0000')
      expect(particle.life).toBe(1.0)
    })
  })

  it('チェーンエフェクトを作成できる', () => {
    particleSystem.createChainEffect(150, 150, 3)
    
    const particles = (particleSystem as any).particles
    expect(particles.length).toBeGreaterThan(15) // 基本数 + チェーンレベルボーナス
  })

  it('パーティクルが時間経過で更新される', () => {
    particleSystem.createBurstEffect(100, 100, '#ff0000', 3)
    
    const particlesBefore = (particleSystem as any).particles.length
    expect(particlesBefore).toBe(3)
    
    // 複数回更新してパーティクルを消費
    for (let i = 0; i < 100; i++) {
      particleSystem.update()
    }
    
    const particlesAfter = (particleSystem as any).particles.length
    expect(particlesAfter).toBeLessThan(particlesBefore)
  })

  it('パーティクルをクリアできる', () => {
    particleSystem.createBurstEffect(100, 100, '#ff0000', 10)
    expect((particleSystem as any).particles.length).toBe(10)
    
    particleSystem.clear()
    expect((particleSystem as any).particles.length).toBe(0)
  })
})

describe('EnhancedAnimationManager', () => {
  let animationManager: EnhancedAnimationManager

  beforeEach(() => {
    mockSettingsManager.get.mockReturnValue(true) // アニメーション有効
    animationManager = new EnhancedAnimationManager()
    vi.useFakeTimers()
  })

  afterEach(() => {
    vi.useRealTimers()
    vi.clearAllMocks()
  })

  it('EnhancedAnimationManagerを作成できる', () => {
    expect(animationManager).toBeInstanceOf(EnhancedAnimationManager)
  })

  it('基本アニメーションを作成できる', () => {
    const animId = animationManager.createAnimation(
      'move',
      { x: 0, y: 0 },
      { x: 100, y: 100 },
      { duration: 1000, easing: Easing.linear }
    )
    
    expect(animId).toBeTruthy()
    expect(animationManager.getActiveAnimationCount()).toBe(1)
  })

  it('ぷよ移動アニメーションを作成できる', () => {
    const animId = animationManager.animatePuyoMove('puyo1', 0, 0, 2, 3)
    
    expect(animId).toBeTruthy()
    expect(animationManager.getActiveAnimationCount()).toBe(1)
    
    const animation = animationManager.getAnimation(animId)
    expect(animation).toBeDefined()
    expect(animation!.type).toBe('move')
    expect(animation!.from).toEqual({ x: 0, y: 0 })
    expect(animation!.to).toEqual({ x: 2, y: 3 })
  })

  it('ぷよ回転アニメーションを作成できる', () => {
    const animId = animationManager.animatePuyoRotation('puyo1', 0, Math.PI / 2)
    
    expect(animId).toBeTruthy()
    const animation = animationManager.getAnimation(animId)
    expect(animation!.type).toBe('rotate')
    expect(animation!.from.angle).toBe(0)
    expect(animation!.to.angle).toBe(Math.PI / 2)
  })

  it('ぷよ落下アニメーションを作成できる', () => {
    const animId = animationManager.animatePuyoDrop('puyo1', 0, 10, true)
    
    expect(animId).toBeTruthy()
    const animation = animationManager.getAnimation(animId)
    expect(animation!.type).toBe('drop')
    expect(animation!.from.y).toBe(0)
    expect(animation!.to.y).toBe(10)
  })

  it('ぷよ消去アニメーションを作成できる', () => {
    const animId = animationManager.animatePuyoErase('puyo1', 50, 50, '#ff0000', 2)
    
    expect(animId).toBeTruthy()
    const animation = animationManager.getAnimation(animId)
    expect(animation!.type).toBe('erase')
    expect(animation!.from.scale).toBe(1.0)
    expect(animation!.to.scale).toBe(1.8)
  })

  it('チェーンアニメーションを作成できる', () => {
    const animId = animationManager.animateChain(100, 100, 3)
    
    expect(animId).toBeTruthy()
    const animation = animationManager.getAnimation(animId)
    expect(animation!.type).toBe('chain')
  })

  it('UIアニメーションを作成できる', () => {
    const animId = animationManager.animateUI(
      'element1',
      { opacity: 0, scale: 0.5 },
      { opacity: 1, scale: 1.0 },
      500
    )
    
    expect(animId).toBeTruthy()
    const animation = animationManager.getAnimation(animId)
    expect(animation!.type).toBe('ui')
    expect(animation!.config.duration).toBe(500)
  })

  it('スコアポップアップアニメーションを作成できる', () => {
    const animId = animationManager.animateScorePopup(200, 200, 1000, '#ffff00')
    
    expect(animId).toBeTruthy()
    const animation = animationManager.getAnimation(animId)
    expect(animation!.type).toBe('ui')
    expect(animation!.from.x).toBe(200)
    expect(animation!.from.y).toBe(200)
  })

  it('アニメーションが時間経過で更新される', () => {
    const animId = animationManager.createAnimation(
      'move',
      { x: 0 },
      { x: 100 },
      { duration: 1000, easing: Easing.linear }
    )
    
    const animation = animationManager.getAnimation(animId)!
    expect(animation.current.x).toBe(0)
    
    // 500ms進める
    vi.advanceTimersByTime(500)
    animationManager.update()
    
    expect(animation.current.x).toBe(50) // 線形補間で中間値
  })

  it('アニメーション完了時にコールバックが呼ばれる', () => {
    const onComplete = vi.fn()
    
    const animId = animationManager.createAnimation(
      'move',
      { x: 0 },
      { x: 100 },
      { duration: 1000, easing: Easing.linear }
    )
    
    const animation = animationManager.getAnimation(animId)!
    animation.onComplete = onComplete
    
    // アニメーション完了まで進める
    vi.advanceTimersByTime(1000)
    animationManager.update()
    
    expect(onComplete).toHaveBeenCalled()
    expect(animationManager.getActiveAnimationCount()).toBe(0)
  })

  it('特定タイプのアニメーションを停止できる', () => {
    animationManager.animatePuyoMove('puyo1', 0, 0, 10, 10)
    animationManager.animatePuyoRotation('puyo2', 0, Math.PI)
    animationManager.animateChain(100, 100, 1)
    
    expect(animationManager.getActiveAnimationCount()).toBe(3)
    
    animationManager.stopAnimationsByType('move')
    expect(animationManager.getActiveAnimationCount()).toBe(2)
  })

  it('全てのアニメーションを停止できる', () => {
    animationManager.animatePuyoMove('puyo1', 0, 0, 10, 10)
    animationManager.animateChain(100, 100, 1)
    
    expect(animationManager.getActiveAnimationCount()).toBe(2)
    
    animationManager.stopAllAnimations()
    expect(animationManager.getActiveAnimationCount()).toBe(0)
  })

  it('アニメーション無効時は処理をスキップする', () => {
    mockSettingsManager.get.mockReturnValue(false) // アニメーション無効
    
    const animId = animationManager.createAnimation(
      'move',
      { x: 0 },
      { x: 100 },
      { duration: 1000, easing: Easing.linear }
    )
    
    vi.advanceTimersByTime(500)
    animationManager.update() // 無効のため更新されない
    
    const animation = animationManager.getAnimation(animId)!
    expect(animation.current.x).toBe(0) // 初期値のまま
  })

  it('パフォーマンス統計を取得できる', () => {
    animationManager.animatePuyoMove('puyo1', 0, 0, 10, 10)
    animationManager.animateChain(100, 100, 2)
    
    const stats = animationManager.getPerformanceStats()
    
    expect(stats.activeAnimations).toBe(2)
    expect(stats.activeParticles).toBeGreaterThan(0) // チェーンエフェクトでパーティクル生成
    expect(stats.memoryUsage).toBeGreaterThan(0)
  })
})

// キャンバス描画のモック
const mockCanvas = {
  getContext: () => ({
    save: vi.fn(),
    restore: vi.fn(),
    globalAlpha: 1,
    fillStyle: '',
    strokeStyle: '',
    beginPath: vi.fn(),
    arc: vi.fn(),
    fill: vi.fn(),
    stroke: vi.fn(),
    textAlign: '',
    font: '',
    fillText: vi.fn()
  })
}

describe('描画機能', () => {
  let animationManager: EnhancedAnimationManager
  let mockCtx: any

  beforeEach(() => {
    mockSettingsManager.get.mockReturnValue(true)
    animationManager = new EnhancedAnimationManager()
    mockCtx = mockCanvas.getContext()
  })

  it('パーティクルシステムが描画される', () => {
    const particleSystem = (animationManager as any).particles
    particleSystem.createBurstEffect(100, 100, '#ff0000', 3)
    
    animationManager.render(mockCtx)
    
    expect(mockCtx.save).toHaveBeenCalled()
    expect(mockCtx.restore).toHaveBeenCalled()
  })

  it('アニメーション無効時は描画をスキップする', () => {
    mockSettingsManager.get.mockReturnValue(false)
    
    animationManager.render(mockCtx)
    
    expect(mockCtx.save).not.toHaveBeenCalled()
  })
})