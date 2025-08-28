/**
 * キャラクターアニメーション管理クラス
 * キャラクターの移動、スケール、フェード、回転などのアニメーションを管理
 */
export class CharacterAnimator {
  public scene: any
  private activeAnimations = new Map<any, any[]>()

  constructor(scene: any) {
    this.scene = scene
  }

  /**
   * 指定位置への移動アニメーション
   * @param character 対象キャラクター
   * @param x 目標X座標
   * @param y 目標Y座標
   * @param duration 継続時間（ミリ秒）
   * @param callback 完了時のコールバック
   */
  moveTo(character: any, x: number, y: number, duration = 500, callback?: () => void): void {
    const tween = this.scene.tweens.add({
      targets: character,
      x: x,
      y: y,
      duration: duration,
      ease: 'Power2',
      onComplete: () => {
        this.removeAnimation(character, tween)
        callback?.()
      },
    })

    this.addAnimation(character, tween)
  }

  /**
   * 相対移動アニメーション
   * @param character 対象キャラクター
   * @param deltaX X方向の移動量
   * @param deltaY Y方向の移動量
   * @param duration 継続時間（ミリ秒）
   * @param callback 完了時のコールバック
   */
  moveBy(
    character: any,
    deltaX: number,
    deltaY: number,
    duration = 500,
    callback?: () => void
  ): void {
    const tween = this.scene.tweens.add({
      targets: character,
      x: `+=${deltaX}`,
      y: `+=${deltaY}`,
      duration: duration,
      ease: 'Power2',
      onComplete: () => {
        this.removeAnimation(character, tween)
        callback?.()
      },
    })

    this.addAnimation(character, tween)
  }

  /**
   * スケール変更アニメーション
   * @param character 対象キャラクター
   * @param scaleX X方向のスケール
   * @param scaleY Y方向のスケール（省略時はscaleXと同値）
   * @param duration 継続時間（ミリ秒）
   * @param callback 完了時のコールバック
   */
  scaleTo(
    character: any,
    scaleX: number,
    scaleY?: number,
    duration = 500,
    callback?: () => void
  ): void {
    const finalScaleY = scaleY !== undefined ? scaleY : scaleX

    const tween = this.scene.tweens.add({
      targets: character,
      scaleX: scaleX,
      scaleY: finalScaleY,
      duration: duration,
      ease: 'Power2',
      onComplete: () => {
        this.removeAnimation(character, tween)
        callback?.()
      },
    })

    this.addAnimation(character, tween)
  }

  /**
   * フェードアウトアニメーション
   * @param character 対象キャラクター
   * @param duration 継続時間（ミリ秒）
   * @param callback 完了時のコールバック
   */
  fadeOut(character: any, duration = 500, callback?: () => void): void {
    const tween = this.scene.tweens.add({
      targets: character,
      alpha: 0,
      duration: duration,
      ease: 'Power2',
      onComplete: () => {
        this.removeAnimation(character, tween)
        callback?.()
      },
    })

    this.addAnimation(character, tween)
  }

  /**
   * フェードインアニメーション
   * @param character 対象キャラクター
   * @param duration 継続時間（ミリ秒）
   * @param callback 完了時のコールバック
   */
  fadeIn(character: any, duration = 500, callback?: () => void): void {
    const tween = this.scene.tweens.add({
      targets: character,
      alpha: 1,
      duration: duration,
      ease: 'Power2',
      onComplete: () => {
        this.removeAnimation(character, tween)
        callback?.()
      },
    })

    this.addAnimation(character, tween)
  }

  /**
   * 透明度指定フェードアニメーション
   * @param character 対象キャラクター
   * @param alpha 目標透明度（0-1）
   * @param duration 継続時間（ミリ秒）
   * @param callback 完了時のコールバック
   */
  fadeTo(character: any, alpha: number, duration = 500, callback?: () => void): void {
    const tween = this.scene.tweens.add({
      targets: character,
      alpha: alpha,
      duration: duration,
      ease: 'Power2',
      onComplete: () => {
        this.removeAnimation(character, tween)
        callback?.()
      },
    })

    this.addAnimation(character, tween)
  }

  /**
   * 回転アニメーション
   * @param character 対象キャラクター
   * @param angle 目標角度（度）
   * @param duration 継続時間（ミリ秒）
   * @param callback 完了時のコールバック
   */
  rotateTo(character: any, angle: number, duration = 500, callback?: () => void): void {
    const tween = this.scene.tweens.add({
      targets: character,
      angle: angle,
      duration: duration,
      ease: 'Power2',
      onComplete: () => {
        this.removeAnimation(character, tween)
        callback?.()
      },
    })

    this.addAnimation(character, tween)
  }

  /**
   * 相対回転アニメーション
   * @param character 対象キャラクター
   * @param deltaAngle 回転量（度）
   * @param duration 継続時間（ミリ秒）
   * @param callback 完了時のコールバック
   */
  rotateBy(character: any, deltaAngle: number, duration = 500, callback?: () => void): void {
    const tween = this.scene.tweens.add({
      targets: character,
      angle: `+=${deltaAngle}`,
      duration: duration,
      ease: 'Power2',
      onComplete: () => {
        this.removeAnimation(character, tween)
        callback?.()
      },
    })

    this.addAnimation(character, tween)
  }

  /**
   * 振動アニメーション
   * @param character 対象キャラクター
   * @param direction 振動方向 ('horizontal', 'vertical', 'random')
   * @param intensity 振動強度（ピクセル）
   * @param duration 継続時間（ミリ秒）
   * @param callback 完了時のコールバック
   */
  shake(
    character: any,
    direction: 'horizontal' | 'vertical' | 'random',
    intensity = 10,
    duration = 500,
    callback?: () => void
  ): void {
    const originalX = character.x
    const originalY = character.y

    const shakeCount = Math.floor(duration / 50) // 50ms間隔で振動
    let currentShake = 0

    const doShake = () => {
      if (currentShake >= shakeCount) {
        // 元の位置に戻す
        character.setPosition(originalX, originalY)
        callback?.()
        return
      }

      let offsetX = 0
      let offsetY = 0

      switch (direction) {
        case 'horizontal':
          offsetX = (Math.random() - 0.5) * 2 * intensity
          break
        case 'vertical':
          offsetY = (Math.random() - 0.5) * 2 * intensity
          break
        case 'random':
          offsetX = (Math.random() - 0.5) * 2 * intensity
          offsetY = (Math.random() - 0.5) * 2 * intensity
          break
      }

      character.setPosition(originalX + offsetX, originalY + offsetY)
      currentShake++

      // 次の振動をスケジュール
      globalThis.setTimeout(doShake, 50)
    }

    doShake()
  }

  /**
   * バウンスアニメーション
   * @param character 対象キャラクター
   * @param bounceHeight バウンス高さ（相対値）
   * @param duration 継続時間（ミリ秒）
   * @param callback 完了時のコールバック
   */
  bounce(character: any, bounceHeight = 0.2, duration = 500, callback?: () => void): void {
    const tween = this.scene.tweens.add({
      targets: character,
      y: `-=${bounceHeight * 100}`,
      duration: duration / 2,
      ease: 'Power2',
      yoyo: true,
      onComplete: () => {
        this.removeAnimation(character, tween)
        callback?.()
      },
    })

    this.addAnimation(character, tween)
  }

  /**
   * パルスアニメーション（拡大縮小の繰り返し）
   * @param character 対象キャラクター
   * @param pulseScale パルス倍率
   * @param duration 継続時間（ミリ秒）
   * @param callback 完了時のコールバック
   */
  pulse(character: any, pulseScale = 1.2, duration = 500, callback?: () => void): void {
    const tween = this.scene.tweens.add({
      targets: character,
      scaleX: pulseScale,
      scaleY: pulseScale,
      duration: duration / 2,
      ease: 'Power2',
      yoyo: true,
      onComplete: () => {
        this.removeAnimation(character, tween)
        callback?.()
      },
    })

    this.addAnimation(character, tween)
  }

  /**
   * 指定キャラクターのアニメーションを停止
   * @param character 対象キャラクター
   */
  stopAnimations(character: any): void {
    const animations = this.activeAnimations.get(character)
    if (animations) {
      animations.forEach((tween) => {
        tween.stop()
        tween.remove()
      })
      this.activeAnimations.delete(character)
    }
  }

  /**
   * すべてのアニメーションを停止
   */
  stopAllAnimations(): void {
    this.activeAnimations.forEach((animations) => {
      animations.forEach((tween) => {
        tween.stop()
        tween.remove()
      })
    })
    this.activeAnimations.clear()
  }

  /**
   * リソースの破棄
   */
  destroy(): void {
    this.stopAllAnimations()
  }

  /**
   * アクティブなアニメーションに追加
   */
  private addAnimation(character: any, tween: any): void {
    if (!this.activeAnimations.has(character)) {
      this.activeAnimations.set(character, [])
    }
    this.activeAnimations.get(character)!.push(tween)
  }

  /**
   * アクティブなアニメーションから削除
   */
  private removeAnimation(character: any, tween: any): void {
    const animations = this.activeAnimations.get(character)
    if (animations) {
      const index = animations.indexOf(tween)
      if (index >= 0) {
        animations.splice(index, 1)
      }

      if (animations.length === 0) {
        this.activeAnimations.delete(character)
      }
    }
  }
}
