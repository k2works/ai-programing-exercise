/**
 * 背景管理クラス
 * 背景画像の変更、フェード、パララックス効果を管理
 */
export class BackgroundManager {
  public scene: any
  public parallaxX = 0
  public parallaxY = 0

  private currentBackground: any = null
  private activeTweens: any[] = []

  constructor(scene: any) {
    this.scene = scene
  }

  /**
   * 背景画像を設定
   * @param textureKey 画像テクスチャキー
   * @param fadeTime フェード時間（ミリ秒、0で即座に変更）
   * @param callback 完了時のコールバック
   */
  setBackground(textureKey: string, fadeTime = 0, callback?: () => void): void {
    const newBackground = this.scene.add.image(400, 300, textureKey)
    newBackground.setDepth(-1000) // 最背面に配置

    if (fadeTime <= 0) {
      // 即座に変更
      this.destroyCurrentBackground()
      this.currentBackground = newBackground
      newBackground.setAlpha(1)
      callback?.()
    } else {
      // フェード付きで変更
      newBackground.setAlpha(0)

      const tween = this.scene.tweens.add({
        targets: newBackground,
        alpha: 1,
        duration: fadeTime,
        onComplete: () => {
          this.destroyCurrentBackground()
          this.currentBackground = newBackground
          this.removeActiveTween(tween)
          callback?.()
        },
      })

      this.activeTweens.push(tween)
    }
  }

  /**
   * 背景をクリア
   * @param fadeTime フェード時間（ミリ秒、0で即座にクリア）
   * @param callback 完了時のコールバック
   */
  clearBackground(fadeTime = 0, callback?: () => void): void {
    if (!this.currentBackground) {
      callback?.()
      return
    }

    if (fadeTime <= 0) {
      // 即座にクリア
      this.destroyCurrentBackground()
      callback?.()
    } else {
      // フェード付きでクリア
      const tween = this.scene.tweens.add({
        targets: this.currentBackground,
        alpha: 0,
        duration: fadeTime,
        onComplete: () => {
          this.destroyCurrentBackground()
          this.removeActiveTween(tween)
          callback?.()
        },
      })

      this.activeTweens.push(tween)
    }
  }

  /**
   * 背景をフェードアウト
   * @param duration 継続時間（ミリ秒）
   * @param callback 完了時のコールバック
   */
  fadeOut(duration = 500, callback?: () => void): void {
    if (!this.currentBackground) {
      callback?.()
      return
    }

    const tween = this.scene.tweens.add({
      targets: this.currentBackground,
      alpha: 0,
      duration: duration,
      onComplete: () => {
        this.removeActiveTween(tween)
        callback?.()
      },
    })

    this.activeTweens.push(tween)
  }

  /**
   * 背景をフェードイン
   * @param duration 継続時間（ミリ秒）
   * @param callback 完了時のコールバック
   */
  fadeIn(duration = 500, callback?: () => void): void {
    if (!this.currentBackground) {
      callback?.()
      return
    }

    const tween = this.scene.tweens.add({
      targets: this.currentBackground,
      alpha: 1,
      duration: duration,
      onComplete: () => {
        this.removeActiveTween(tween)
        callback?.()
      },
    })

    this.activeTweens.push(tween)
  }

  /**
   * 指定透明度にフェード
   * @param alpha 目標透明度（0-1）
   * @param duration 継続時間（ミリ秒）
   * @param callback 完了時のコールバック
   */
  fadeTo(alpha: number, duration = 500, callback?: () => void): void {
    if (!this.currentBackground) {
      callback?.()
      return
    }

    const tween = this.scene.tweens.add({
      targets: this.currentBackground,
      alpha: alpha,
      duration: duration,
      onComplete: () => {
        this.removeActiveTween(tween)
        callback?.()
      },
    })

    this.activeTweens.push(tween)
  }

  /**
   * 単色背景を設定
   * @param color 背景色（#rrggbb形式）
   * @param alpha 透明度（0-1）
   * @param fadeTime フェード時間（ミリ秒）
   * @param callback 完了時のコールバック
   */
  setBackgroundColor(color: string, alpha = 1.0, fadeTime = 0, callback?: () => void): void {
    const colorValue = this.parseColor(color)
    const colorBackground = this.scene.add.rectangle(400, 300, 800, 600, colorValue)
    colorBackground.setDepth(-1000)
    colorBackground.setFillStyle(colorValue, alpha)

    if (fadeTime <= 0) {
      // 即座に変更
      this.destroyCurrentBackground()
      this.currentBackground = colorBackground
      callback?.()
    } else {
      // フェード付きで変更
      colorBackground.setAlpha(0)

      const tween = this.scene.tweens.add({
        targets: colorBackground,
        alpha: alpha,
        duration: fadeTime,
        onComplete: () => {
          this.destroyCurrentBackground()
          this.currentBackground = colorBackground
          this.removeActiveTween(tween)
          callback?.()
        },
      })

      this.activeTweens.push(tween)
    }
  }

  /**
   * パララックス効果を設定
   * @param scrollFactorX X方向のスクロール係数（0-1）
   * @param scrollFactorY Y方向のスクロール係数（0-1）
   */
  setParallax(scrollFactorX: number, scrollFactorY: number): void {
    this.parallaxX = scrollFactorX
    this.parallaxY = scrollFactorY

    if (this.currentBackground && this.currentBackground.setScrollFactor) {
      this.currentBackground.setScrollFactor(scrollFactorX, scrollFactorY)
    }
  }

  /**
   * パララックス効果を更新
   * @param cameraX カメラのX座標
   * @param cameraY カメラのY座標
   */
  updateParallax(cameraX: number, cameraY: number): void {
    if (!this.currentBackground) return

    const parallaxOffsetX = cameraX * this.parallaxX
    const parallaxOffsetY = cameraY * this.parallaxY

    if (this.currentBackground.setPosition) {
      this.currentBackground.setPosition(400 - parallaxOffsetX, 300 - parallaxOffsetY)
    }
  }

  /**
   * すべてのアニメーションを停止
   */
  stopAllAnimations(): void {
    this.activeTweens.forEach((tween) => {
      tween.stop()
      tween.remove()
    })
    this.activeTweens = []
  }

  /**
   * リソースの破棄
   */
  destroy(): void {
    this.stopAllAnimations()
    this.destroyCurrentBackground()
  }

  /**
   * 色文字列を数値に変換
   */
  private parseColor(color: string): number {
    if (color.startsWith('#')) {
      return parseInt(color.substring(1), 16)
    }
    return 0x000000
  }

  /**
   * 現在の背景を破棄
   */
  private destroyCurrentBackground(): void {
    if (this.currentBackground) {
      this.currentBackground.destroy()
      this.currentBackground = null
    }
  }

  /**
   * アクティブなトゥイーンから削除
   */
  private removeActiveTween(tween: any): void {
    const index = this.activeTweens.indexOf(tween)
    if (index >= 0) {
      this.activeTweens.splice(index, 1)
    }
  }
}
