// アニメーションシステム

export interface AnimationFrame {
  duration: number; // ミリ秒
  progress: number; // 0.0 - 1.0
  isComplete: boolean;
}

export class Animation {
  private startTime: number
  private currentTime: number
  private duration: number
  private isActive: boolean = false

  constructor(duration: number) {
    this.duration = duration
    this.startTime = 0
    this.currentTime = 0
  }

  start(): void {
    this.isActive = true
    this.startTime = performance.now()
    this.currentTime = this.startTime
  }

  update(): AnimationFrame {
    if (!this.isActive) {
      return { duration: this.duration, progress: 0, isComplete: true }
    }

    this.currentTime = performance.now()
    const elapsed = this.currentTime - this.startTime
    const progress = Math.min(elapsed / this.duration, 1.0)

    if (progress >= 1.0) {
      this.isActive = false
      return { duration: this.duration, progress: 1.0, isComplete: true }
    }

    return { duration: this.duration, progress, isComplete: false }
  }

  isRunning(): boolean {
    return this.isActive
  }

  reset(): void {
    this.isActive = false
    this.startTime = 0
    this.currentTime = 0
  }

  getProgress(): number {
    if (!this.isActive) return 0
    const elapsed = this.currentTime - this.startTime
    return Math.min(elapsed / this.duration, 1.0)
  }
}

// イージング関数
export const Easing = {
  linear: (t: number): number => t,
  easeInQuad: (t: number): number => t * t,
  easeOutQuad: (t: number): number => t * (2 - t),
  easeInOutQuad: (t: number): number => t < 0.5 ? 2 * t * t : -1 + (4 - 2 * t) * t,
  easeInCubic: (t: number): number => t * t * t,
  easeOutCubic: (t: number): number => (--t) * t * t + 1,
  easeInOutCubic: (t: number): number => t < 0.5 ? 4 * t * t * t : (t - 1) * (2 * t - 2) * (2 * t - 2) + 1,
  bounce: (t: number): number => {
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
}

// ぷよ消去アニメーション
export class PuyoEraseAnimation extends Animation {
  private x: number
  private y: number
  private originalScale: number = 1.0

  constructor(x: number, y: number, duration: number = 300) {
    super(duration)
    this.x = x
    this.y = y
  }

  getPosition(): { x: number; y: number } {
    return { x: this.x, y: this.y }
  }

  getScale(): number {
    if (!this.isRunning()) return this.originalScale
    
    const progress = this.getProgress()
    // 縮小しながら消去
    return this.originalScale * (1.0 - Easing.easeInQuad(progress))
  }

  getOpacity(): number {
    if (!this.isRunning()) return 1.0
    
    const progress = this.getProgress()
    // フェードアウト
    return 1.0 - Easing.easeOutQuad(progress)
  }

  getRotation(): number {
    if (!this.isRunning()) return 0
    
    const progress = this.getProgress()
    // 回転しながら消去
    return progress * Math.PI * 2
  }
}

// ぷよ落下アニメーション
export class PuyoFallAnimation extends Animation {
  private fromX: number
  private fromY: number
  private toX: number
  private toY: number

  constructor(fromX: number, fromY: number, toX: number, toY: number, duration: number = 200) {
    super(duration)
    this.fromX = fromX
    this.fromY = fromY
    this.toX = toX
    this.toY = toY
  }

  getCurrentPosition(): { x: number; y: number } {
    if (!this.isRunning()) {
      return { x: this.toX, y: this.toY }
    }

    const progress = this.getProgress()
    const easedProgress = Easing.easeOutCubic(progress)

    return {
      x: this.fromX + (this.toX - this.fromX) * easedProgress,
      y: this.fromY + (this.toY - this.fromY) * easedProgress
    }
  }

  getBounceOffset(): number {
    if (!this.isRunning()) return 0
    
    const progress = this.getProgress()
    // 落下時の跳ね返り効果
    return Math.sin(progress * Math.PI * 3) * (1 - progress) * 2
  }
}

// チェインエフェクトアニメーション
export class ChainEffectAnimation extends Animation {
  private chainCount: number
  private centerX: number
  private centerY: number

  constructor(chainCount: number, centerX: number, centerY: number, duration: number = 500) {
    super(duration)
    this.chainCount = chainCount
    this.centerX = centerX
    this.centerY = centerY
  }

  getChainCount(): number {
    return this.chainCount
  }

  getCenterPosition(): { x: number; y: number } {
    return { x: this.centerX, y: this.centerY }
  }

  getExpansionRadius(): number {
    if (!this.isRunning()) return 0
    
    const progress = this.getProgress()
    const maxRadius = 50 + (this.chainCount * 10) // チェイン数に応じて拡大
    return maxRadius * Easing.easeOutQuad(progress)
  }

  getIntensity(): number {
    if (!this.isRunning()) return 0
    
    const progress = this.getProgress()
    // 開始時に強く、徐々に弱く
    return (1.0 - progress) * Math.min(this.chainCount / 5, 1.0)
  }

  getPulseScale(): number {
    if (!this.isRunning()) return 1.0
    
    const progress = this.getProgress()
    // パルス効果
    const pulseFreq = 2 + this.chainCount * 0.5
    const pulse = Math.sin(progress * Math.PI * pulseFreq) * 0.1
    return 1.0 + pulse * (1 - progress)
  }
}

// アニメーションマネージャー
export class AnimationManager {
  private animations: Map<string, Animation> = new Map()
  private eraseAnimations: Map<string, PuyoEraseAnimation> = new Map()
  private fallAnimations: Map<string, PuyoFallAnimation> = new Map()
  private chainAnimations: Map<string, ChainEffectAnimation> = new Map()

  // 消去アニメーション追加
  addEraseAnimation(id: string, x: number, y: number, duration?: number): void {
    const animation = new PuyoEraseAnimation(x, y, duration)
    animation.start()
    this.eraseAnimations.set(id, animation)
  }

  // 落下アニメーション追加
  addFallAnimation(id: string, fromX: number, fromY: number, toX: number, toY: number, duration?: number): void {
    const animation = new PuyoFallAnimation(fromX, fromY, toX, toY, duration)
    animation.start()
    this.fallAnimations.set(id, animation)
  }

  // チェインエフェクト追加
  addChainEffect(id: string, chainCount: number, centerX: number, centerY: number, duration?: number): void {
    const animation = new ChainEffectAnimation(chainCount, centerX, centerY, duration)
    animation.start()
    this.chainAnimations.set(id, animation)
  }

  // すべてのアニメーションを更新
  update(): void {
    // 消去アニメーション更新
    for (const [id, animation] of this.eraseAnimations.entries()) {
      const frame = animation.update()
      if (frame.isComplete) {
        this.eraseAnimations.delete(id)
      }
    }

    // 落下アニメーション更新
    for (const [id, animation] of this.fallAnimations.entries()) {
      const frame = animation.update()
      if (frame.isComplete) {
        this.fallAnimations.delete(id)
      }
    }

    // チェインエフェクト更新
    for (const [id, animation] of this.chainAnimations.entries()) {
      const frame = animation.update()
      if (frame.isComplete) {
        this.chainAnimations.delete(id)
      }
    }
  }

  // アニメーション取得
  getEraseAnimation(id: string): PuyoEraseAnimation | undefined {
    return this.eraseAnimations.get(id)
  }

  getFallAnimation(id: string): PuyoFallAnimation | undefined {
    return this.fallAnimations.get(id)
  }

  getChainEffect(id: string): ChainEffectAnimation | undefined {
    return this.chainAnimations.get(id)
  }

  // アクティブなアニメーション確認
  hasActiveAnimations(): boolean {
    return this.eraseAnimations.size > 0 || 
           this.fallAnimations.size > 0 || 
           this.chainAnimations.size > 0
  }

  // すべてのアニメーションをクリア
  clear(): void {
    this.eraseAnimations.clear()
    this.fallAnimations.clear()
    this.chainAnimations.clear()
  }

  // デバッグ情報
  getAnimationStats(): { erasing: number; falling: number; effects: number } {
    return {
      erasing: this.eraseAnimations.size,
      falling: this.fallAnimations.size,
      effects: this.chainAnimations.size
    }
  }
}