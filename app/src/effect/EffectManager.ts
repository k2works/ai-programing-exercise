/**
 * 演出効果管理クラス
 * 画面フラッシュ、振動、フェードなどの視覚効果を管理
 */
export class EffectManager {
	private scene: any
  private activeTweens: any[] = []
  private activeOverlays: any[] = []

	constructor(scene: any) {
		this.scene = scene
  }

  /**
   * フラッシュエフェクトを実行
   * @param color フラッシュ色 ('white', 'black', '#rrggbb')
   * @param duration 継続時間（ミリ秒）
   */
	flash(color: string, duration: number = 300): void {
		// デバッグ情報
		console.log('EffectManager.flash called with:', { color, duration })
		console.log('Scene check:', {
			scene: !!this.scene,
			add: !!(this.scene && this.scene.add),
			tweens: !!(this.scene && this.scene.tweens)
		})

		if (!this.scene) {
			console.error('EffectManager: scene is undefined')
			return
		}
		if (!this.scene.add) {
			console.error('EffectManager: scene.add is undefined', this.scene)
			return
		}

    const colorValue = this.parseColor(color)

		try {
			// カメラの寸法を取得
			const camera = this.scene.cameras.main
			const { width, height } = camera

			// 画面中央にオーバーレイを配置
			const overlay = this.scene.add.rectangle(width / 2, height / 2, width, height, colorValue)
			overlay.setFillStyle(colorValue)
			overlay.setAlpha(1)
			overlay.setDepth(10000)
			overlay.setVisible(true)

			this.activeOverlays.push(overlay)

			const tween = this.scene.tweens.add({
				targets: overlay,
				alpha: { from: 1, to: 0 },
				duration: duration,
				onComplete: () => {
					overlay.destroy()
					this.removeFromActive(tween, overlay)
				},
			})

			this.activeTweens.push(tween)
		} catch (error) {
			console.error('Error in flash effect:', error)
			console.error('Scene state:', this.scene)
			// エラー時にもオーバーレイを破棄
			this.activeOverlays.forEach(overlay => {
				if (overlay && overlay.destroy) {
					overlay.destroy()
				}
			})
			this.activeOverlays = []
		}
  }

  /**
   * 画面振動エフェクトを実行
   * @param intensity 振動強度 ('light', 'medium', 'heavy')
   * @param duration 継続時間（ミリ秒）
   */
	shake(intensity: 'light' | 'medium' | 'heavy', duration: number = 600): void {
    const intensityValue = this.getShakeIntensity(intensity)
		this.scene.cameras.main.shake(duration, intensityValue)
  }

  /**
   * フェードエフェクトを実行
   * @param direction フェード方向 ('in', 'out')
   * @param duration 継続時間（ミリ秒）
   * @param color フェード色
   */
	fade(direction: 'in' | 'out', duration: number = 500, color: string = '#000000'): void {
    const colorValue = this.parseColor(color)

		// カメラの寸法を取得
		const camera = this.scene.cameras.main
		const { width, height } = camera

		// 画面中央にオーバーレイを配置
		const overlay = this.scene.add.rectangle(width / 2, height / 2, width, height, colorValue)
    overlay.setFillStyle(colorValue)
    overlay.setDepth(10000)
    overlay.setVisible(true)

    this.activeOverlays.push(overlay)

    const fromAlpha = direction === 'in' ? 1 : 0
    const toAlpha = direction === 'in' ? 0 : 1

    overlay.setAlpha(fromAlpha)

		const tween = this.scene.tweens.add({
      targets: overlay,
      alpha: { from: fromAlpha, to: toAlpha },
      duration: duration,
      onComplete: () => {
				// すべてのフェードエフェクト完了時にオーバーレイを破棄
				overlay.destroy()
				this.activeOverlays = this.activeOverlays.filter(o => o !== overlay)
				this.activeTweens = this.activeTweens.filter(t => t !== tween)
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

	/**
	 * すべてのアクティブなエフェクトをクリーンアップ
	 */
	public cleanup(): void {
		// アクティブなTweenを停止
		this.activeTweens.forEach(tween => {
			if (tween && tween.stop) {
				tween.stop()
			}
		})
		this.activeTweens = []

		// アクティブなオーバーレイを破棄
		this.activeOverlays.forEach(overlay => {
			if (overlay && overlay.destroy) {
				overlay.destroy()
			}
		})
		this.activeOverlays = []
	}
}
