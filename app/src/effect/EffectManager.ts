/**
 * 演出効果管理クラス
 * 画面フラッシュ、振動、フェードなどの視覚効果を管理
 */
export class EffectManager {
  public container: any
  private activeTweens: any[] = []
  private activeOverlays: any[] = []

  constructor(container: any) {
    this.container = container
  }

  /**
   * フラッシュエフェクトを実行
   * @param color フラッシュ色 ('white', 'black', '#rrggbb')
   * @param duration 継続時間（ミリ秒）
   */
  flash(color: string, duration = 300): void {
    const colorValue = this.parseColor(color)

    const overlay = this.container.add.rectangle(0, 0, 800, 600, colorValue)
    overlay.setFillStyle(colorValue)
    overlay.setAlpha(1)
    overlay.setDepth(10000)
    overlay.setVisible(true)

    this.activeOverlays.push(overlay)

    const tween = this.container.scene.tweens.add({
      targets: overlay,
      alpha: { from: 1, to: 0 },
      duration: duration,
      onComplete: () => {
        overlay.destroy()
        this.removeFromActive(tween, overlay)
      },
    })

    this.activeTweens.push(tween)
  }

  /**
   * 画面振動エフェクトを実行
   * @param intensity 振動強度 ('light', 'medium', 'heavy')
   * @param duration 継続時間（ミリ秒）
   */
  shake(intensity: 'light' | 'medium' | 'heavy', duration = 600): void {
    const intensityValue = this.getShakeIntensity(intensity)
    this.container.scene.cameras.main.shake(duration, intensityValue)
  }

  /**
   * フェードエフェクトを実行
   * @param direction フェード方向 ('in', 'out')
   * @param duration 継続時間（ミリ秒）
   * @param color フェード色
   */
  fade(direction: 'in' | 'out', duration = 500, color = '#000000'): void {
    const colorValue = this.parseColor(color)

    const overlay = this.container.add.rectangle(0, 0, 800, 600, colorValue)
    overlay.setFillStyle(colorValue)
    overlay.setDepth(10000)
    overlay.setVisible(true)

    this.activeOverlays.push(overlay)

    const fromAlpha = direction === 'in' ? 1 : 0
    const toAlpha = direction === 'in' ? 0 : 1

    overlay.setAlpha(fromAlpha)

    const tween = this.container.scene.tweens.add({
      targets: overlay,
      alpha: { from: fromAlpha, to: toAlpha },
      duration: duration,
      onComplete: () => {
        if (direction === 'in') {
          overlay.destroy()
        }
        this.removeFromActive(tween, overlay)
      },
    })

    this.activeTweens.push(tween)
  }

  /**
   * エフェクトチェーンを実行
   * @param effects エフェクト配列
   * @param callback 完了時のコールバック
   */
  chain(
    effects: Array<{
      type: 'flash' | 'shake' | 'fade'
      color?: string
      intensity?: 'light' | 'medium' | 'heavy'
      direction?: 'in' | 'out'
      duration?: number
    }>,
    callback?: () => void
  ): void {
    this.executeChain(effects, 0, callback)
  }

  /**
   * すべてのエフェクトを停止
   */
  stopAllEffects(): void {
    this.activeTweens.forEach((tween) => {
      tween.stop()
      tween.remove()
    })

    this.activeOverlays.forEach((overlay) => {
      overlay.destroy()
    })

    this.activeTweens = []
    this.activeOverlays = []
  }

  /**
   * リソースの破棄
   */
  destroy(): void {
    this.stopAllEffects()
  }

  /**
   * 色文字列を数値に変換
   */
  private parseColor(color: string): number {
    switch (color) {
      case 'white':
        return 0xffffff
      case 'black':
        return 0x000000
      default:
        // #rrggbb形式の場合
        if (color.startsWith('#')) {
          return parseInt(color.substring(1), 16)
        }
        return 0x000000
    }
  }

  /**
   * 振動強度を数値に変換
   */
  private getShakeIntensity(intensity: 'light' | 'medium' | 'heavy'): number {
    switch (intensity) {
      case 'light':
        return 5
      case 'medium':
        return 10
      case 'heavy':
        return 20
      default:
        return 10
    }
  }

  /**
   * アクティブな要素から削除
   */
  private removeFromActive(tween: any, overlay: any): void {
    const tweenIndex = this.activeTweens.indexOf(tween)
    if (tweenIndex >= 0) {
      this.activeTweens.splice(tweenIndex, 1)
    }

    const overlayIndex = this.activeOverlays.indexOf(overlay)
    if (overlayIndex >= 0) {
      this.activeOverlays.splice(overlayIndex, 1)
    }
  }

  /**
   * エフェクトチェーンの再帰実行
   */
  private executeChain(effects: any[], index: number, callback?: () => void): void {
    if (index >= effects.length) {
      callback?.()
      return
    }

    const effect = effects[index]
    this.executeEffect(effect)

    // 次のエフェクトを実行（簡単な遅延実装）
    globalThis.setTimeout(() => {
      this.executeChain(effects, index + 1, callback)
    }, effect.duration || 300)
  }

  /**
   * 個別エフェクトの実行
   */
  private executeEffect(effect: any): void {
    switch (effect.type) {
      case 'flash':
        this.flash(effect.color || 'white', effect.duration)
        break
      case 'shake':
        this.shake(effect.intensity || 'medium', effect.duration)
        break
      case 'fade':
        this.fade(effect.direction || 'out', effect.duration, effect.color)
        break
    }
  }
}
