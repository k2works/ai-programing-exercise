// 拡張アニメーションシステム
import { settingsManager } from './settings'

export interface AnimationConfig {
  duration: number
  easing: EasingFunction
  delay?: number
  repeat?: number
  yoyo?: boolean
  onComplete?: () => void
}

export type EasingFunction = (t: number) => number

export class Easing {
  // 基本イージング関数
  static linear(t: number): number {
    return t
  }

  static easeInQuad(t: number): number {
    return t * t
  }

  static easeOutQuad(t: number): number {
    return t * (2 - t)
  }

  static easeInOutQuad(t: number): number {
    return t < 0.5 ? 2 * t * t : -1 + (4 - 2 * t) * t
  }

  static easeInCubic(t: number): number {
    return t * t * t
  }

  static easeOutCubic(t: number): number {
    return --t * t * t + 1
  }

  static easeInOutCubic(t: number): number {
    return t < 0.5 ? 4 * t * t * t : (t - 1) * (2 * t - 2) * (2 * t - 2) + 1
  }

  static easeInBounce(t: number): number {
    return 1 - Easing.easeOutBounce(1 - t)
  }

  static easeOutBounce(t: number): number {
    if (t < 1 / 2.75) {
      return 7.5625 * t * t
    } else if (t < 2 / 2.75) {
      return 7.5625 * (t -= 1.5 / 2.75) * t + 0.75
    } else if (t < 2.5 / 2.75) {
      return 7.5625 * (t -= 2.25 / 2.75) * t + 0.9375
    } else {
      return 7.5625 * (t -= 2.625 / 2.75) * t + 0.984375
    }
  }

  static easeInElastic(t: number): number {
    if (t === 0) return 0
    if (t === 1) return 1
    return -(Math.pow(2, 10 * (t - 1)) * Math.sin((t - 1.1) * 5 * Math.PI))
  }

  static easeOutElastic(t: number): number {
    if (t === 0) return 0
    if (t === 1) return 1
    return Math.pow(2, -10 * t) * Math.sin((t - 0.1) * 5 * Math.PI) + 1
  }
}

export interface EnhancedAnimationState {
  id: string
  type: 'move' | 'rotate' | 'drop' | 'erase' | 'chain' | 'particle' | 'ui'
  startTime: number
  config: AnimationConfig
  from: Record<string, number>
  to: Record<string, number>
  current: Record<string, number>
  isActive: boolean
  onComplete?: () => void
  onUpdate?: (state: EnhancedAnimationState) => void
}

export class ParticleSystem {
  private particles: Array<{
    id: string
    x: number
    y: number
    vx: number
    vy: number
    life: number
    maxLife: number
    color: string
    size: number
    decay: number
  }> = []
  
  private nextParticleId = 0

  createBurstEffect(x: number, y: number, color: string, count: number = 10): void {
    for (let i = 0; i < count; i++) {
      const angle = (Math.PI * 2 * i) / count + Math.random() * 0.5
      const speed = 2 + Math.random() * 3
      
      this.particles.push({
        id: `particle_${this.nextParticleId++}`,
        x,
        y,
        vx: Math.cos(angle) * speed,
        vy: Math.sin(angle) * speed,
        life: 1.0,
        maxLife: 1.0,
        color,
        size: 3 + Math.random() * 4,
        decay: 0.02 + Math.random() * 0.02
      })
    }
  }

  createChainEffect(x: number, y: number, chainLevel: number): void {
    const colors = ['#ffff44', '#ff8844', '#ff4488', '#8844ff', '#44ff88']
    const color = colors[Math.min(chainLevel - 1, colors.length - 1)]
    const count = Math.min(15 + chainLevel * 3, 30)
    
    this.createBurstEffect(x, y, color, count)
    
    // 追加のスパークル効果
    for (let i = 0; i < chainLevel * 2; i++) {
      setTimeout(() => {
        const offsetX = (Math.random() - 0.5) * 40
        const offsetY = (Math.random() - 0.5) * 40
        this.createBurstEffect(x + offsetX, y + offsetY, color, 5)
      }, i * 100)
    }
  }

  update(): void {
    this.particles = this.particles.filter(particle => {
      particle.x += particle.vx
      particle.y += particle.vy
      particle.vy += 0.1 // 重力効果
      particle.life -= particle.decay
      particle.vx *= 0.98 // 空気抵抗
      particle.vy *= 0.98
      
      return particle.life > 0
    })
  }

  render(ctx: CanvasRenderingContext2D): void {
    this.particles.forEach(particle => {
      ctx.save()
      ctx.globalAlpha = particle.life
      ctx.fillStyle = particle.color
      ctx.beginPath()
      ctx.arc(particle.x, particle.y, particle.size * particle.life, 0, Math.PI * 2)
      ctx.fill()
      ctx.restore()
    })
  }

  clear(): void {
    this.particles.length = 0
  }
}

export class EnhancedAnimationManager {
  private animations: Map<string, EnhancedAnimationState> = new Map()
  private particles: ParticleSystem = new ParticleSystem()
  private nextAnimationId = 0

  // アニメーション作成
  createAnimation(
    type: EnhancedAnimationState['type'],
    from: Record<string, number>,
    to: Record<string, number>,
    config: AnimationConfig
  ): string {
    const id = `anim_${this.nextAnimationId++}_${type}`
    
    const animation: EnhancedAnimationState = {
      id,
      type,
      startTime: Date.now() + (config.delay || 0),
      config,
      from: { ...from },
      to: { ...to },
      current: { ...from },
      isActive: true
    }

    this.animations.set(id, animation)
    return id
  }

  // ぷよ移動アニメーション
  animatePuyoMove(
    _puyoId: string,
    fromX: number,
    fromY: number,
    toX: number,
    toY: number
  ): string {
    return this.createAnimation(
      'move',
      { x: fromX, y: fromY },
      { x: toX, y: toY },
      {
        duration: 150,
        easing: Easing.easeOutQuad
      }
    )
  }

  // ぷよ回転アニメーション
  animatePuyoRotation(
    _puyoId: string,
    fromAngle: number,
    toAngle: number
  ): string {
    return this.createAnimation(
      'rotate',
      { angle: fromAngle },
      { angle: toAngle },
      {
        duration: 200,
        easing: Easing.easeInOutCubic
      }
    )
  }

  // ぷよ落下アニメーション
  animatePuyoDrop(
    _puyoId: string,
    fromY: number,
    toY: number,
    bounceEffect: boolean = true
  ): string {
    const easing = bounceEffect ? Easing.easeOutBounce : Easing.easeInQuad
    
    return this.createAnimation(
      'drop',
      { y: fromY, scale: 1.0 },
      { y: toY, scale: 1.0 },
      {
        duration: 300 + Math.abs(toY - fromY) * 10,
        easing
      }
    )
  }

  // ぷよ消去アニメーション
  animatePuyoErase(
    _puyoId: string,
    x: number,
    y: number,
    _color: string,
    chainLevel: number = 1
  ): string {
    // パーティクル効果作成
    this.particles.createChainEffect(x, y, chainLevel)
    
    return this.createAnimation(
      'erase',
      { scale: 1.0, alpha: 1.0, rotation: 0 },
      { scale: 1.8, alpha: 0.0, rotation: Math.PI * 2 },
      {
        duration: 400,
        easing: Easing.easeOutCubic,
        onComplete: () => {
          // 消去完了時の処理
        }
      }
    )
  }

  // チェーンアニメーション
  animateChain(
    centerX: number,
    centerY: number,
    chainLevel: number
  ): string {
    // 連鎖レベルに応じたエフェクト
    this.particles.createChainEffect(centerX, centerY, chainLevel)
    
    return this.createAnimation(
      'chain',
      { scale: 0.5, alpha: 1.0, shockwave: 0 },
      { scale: 2.0, alpha: 0.0, shockwave: 50 },
      {
        duration: 800 + chainLevel * 200,
        easing: Easing.easeOutElastic
      }
    )
  }

  // UI要素のアニメーション
  animateUI(
    _elementId: string,
    properties: Record<string, number>,
    targetProperties: Record<string, number>,
    duration: number = 300
  ): string {
    return this.createAnimation(
      'ui',
      properties,
      targetProperties,
      {
        duration,
        easing: Easing.easeOutQuad
      }
    )
  }

  // スコア表示アニメーション
  animateScorePopup(
    x: number,
    y: number,
    _score: number,
    _color: string = '#ffff44'
  ): string {
    return this.createAnimation(
      'ui',
      { x, y: y, scale: 0.5, alpha: 1.0 },
      { x, y: y - 50, scale: 1.2, alpha: 0.0 },
      {
        duration: 1000,
        easing: Easing.easeOutCubic
      }
    )
  }

  // アニメーション更新
  update(): void {
    if (!settingsManager.get('enableAnimations')) {
      return // アニメーション無効時はスキップ
    }

    const now = Date.now()
    
    this.animations.forEach((animation, id) => {
      if (!animation.isActive || now < animation.startTime) {
        return
      }

      const elapsed = now - animation.startTime
      const progress = Math.min(elapsed / animation.config.duration, 1)
      const easedProgress = animation.config.easing(progress)

      // プロパティ値の補間
      for (const [key, startValue] of Object.entries(animation.from)) {
        const endValue = animation.to[key]
        if (endValue !== undefined) {
          animation.current[key] = startValue + (endValue - startValue) * easedProgress
        }
      }

      // 更新コールバック実行
      if (animation.onUpdate) {
        animation.onUpdate(animation)
      }

      // アニメーション完了チェック
      if (progress >= 1) {
        if (animation.config.yoyo && animation.config.repeat !== 0) {
          // ヨーヨー効果
          const temp = animation.from
          animation.from = animation.to
          animation.to = temp
          animation.startTime = now
          
          if (animation.config.repeat !== undefined && animation.config.repeat > 0) {
            animation.config.repeat--
          }
        } else if (animation.config.repeat && animation.config.repeat > 0) {
          // リピート
          animation.config.repeat--
          animation.startTime = now
          animation.current = { ...animation.from }
        } else {
          // アニメーション完了
          animation.isActive = false
          if (animation.onComplete) {
            animation.onComplete()
          }
          this.animations.delete(id)
        }
      }
    })

    // パーティクルシステム更新
    this.particles.update()
  }

  // アニメーション描画
  render(ctx: CanvasRenderingContext2D): void {
    if (!settingsManager.get('enableAnimations')) {
      return
    }

    // パーティクル描画
    this.particles.render(ctx)

    // その他のアニメーション効果描画
    this.animations.forEach(animation => {
      this.renderAnimation(ctx, animation)
    })
  }

  private renderAnimation(ctx: CanvasRenderingContext2D, animation: EnhancedAnimationState): void {
    if (!animation.isActive) return

    ctx.save()

    switch (animation.type) {
      case 'chain':
        this.renderChainEffect(ctx, animation)
        break
      case 'ui':
        this.renderUIEffect(ctx, animation)
        break
      // 他のアニメーションタイプも必要に応じて実装
    }

    ctx.restore()
  }

  private renderChainEffect(ctx: CanvasRenderingContext2D, animation: EnhancedAnimationState): void {
    const { scale, alpha, shockwave } = animation.current
    
    if (shockwave > 0) {
      ctx.globalAlpha = alpha * 0.3
      ctx.strokeStyle = '#ffffff'
      ctx.lineWidth = 3
      ctx.beginPath()
      ctx.arc(200, 300, shockwave, 0, Math.PI * 2) // 仮の中心座標
      ctx.stroke()
    }
  }

  private renderUIEffect(ctx: CanvasRenderingContext2D, animation: EnhancedAnimationState): void {
    const { x, y, scale, alpha } = animation.current
    
    if (x !== undefined && y !== undefined) {
      ctx.globalAlpha = alpha || 1.0
      ctx.font = `${Math.round(16 * (scale || 1.0))}px Arial`
      ctx.fillStyle = '#ffff44'
      ctx.textAlign = 'center'
      ctx.fillText('+1000', x, y) // 仮のスコア表示
    }
  }

  // アニメーション取得
  getAnimation(id: string): EnhancedAnimationState | undefined {
    return this.animations.get(id)
  }

  // 特定タイプのアニメーション停止
  stopAnimationsByType(type: EnhancedAnimationState['type']): void {
    this.animations.forEach((animation, id) => {
      if (animation.type === type) {
        animation.isActive = false
        this.animations.delete(id)
      }
    })
  }

  // 全アニメーション停止
  stopAllAnimations(): void {
    this.animations.clear()
    this.particles.clear()
  }

  // アニメーション数取得
  getActiveAnimationCount(): number {
    return this.animations.size
  }

  // パフォーマンス統計
  getPerformanceStats(): {
    activeAnimations: number
    activeParticles: number
    memoryUsage: number
  } {
    return {
      activeAnimations: this.animations.size,
      activeParticles: this.particles['particles'].length,
      memoryUsage: this.animations.size * 200 + this.particles['particles'].length * 50 // 概算
    }
  }
}