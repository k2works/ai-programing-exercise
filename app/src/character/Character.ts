/**
 * キャラクター表示システム
 * ノベルゲームでのキャラクター立ち絵表示・管理を担当
 */

import * as Phaser from 'phaser'

export interface Position {
  x: number
  y: number
}

export interface CharacterConfig {
  name: string
  expressions: string[]
  defaultExpression?: string
}

export class Character {
  public readonly name: string
  public isVisible: boolean = false
  public currentExpression: string = 'normal'
  public position: Position = { x: 0, y: 0 }

  private scene: Phaser.Scene
  private currentImage: Phaser.GameObjects.Image | null = null

  constructor(scene: Phaser.Scene, name: string) {
    this.scene = scene
    this.name = name
  }

  /**
   * キャラクターを表示する
   * @param expression 表情名
   * @param position 表示位置
   */
  show(expression: string, position: Position): void {
    this.hideCurrentImage()

    const imageKey = `${this.name}-${expression}`
    this.currentImage = this.scene.add.image(position.x, position.y, imageKey)
    this.currentImage.setOrigin(0.5, 1) // 下中央を基準点に

    this.isVisible = true
    this.currentExpression = expression
    this.position = { ...position }
  }

  /**
   * キャラクターを非表示にする
   */
  hide(): void {
    this.hideCurrentImage()
    this.isVisible = false
  }

  /**
   * 表情を変更する
   * @param expression 新しい表情名
   */
  changeExpression(expression: string): void {
    if (!this.isVisible || this.currentExpression === expression) {
      return
    }

    this.show(expression, this.position)
  }

  /**
   * 位置を変更する
   * @param position 新しい位置
   */
  setPosition(position: Position): void {
    this.position = { ...position }

    if (this.currentImage) {
      this.currentImage.setPosition(position.x, position.y)
    }
  }

  /**
   * フェードイン効果でキャラクターを表示する
   * @param expression 表情名
   * @param position 表示位置
   * @param duration フェード時間（ミリ秒）
   */
  showWithFadeIn(expression: string, position: Position, duration: number = 500): void {
    this.show(expression, position)

    if (this.currentImage) {
      this.currentImage.setAlpha(0)

      this.scene.tweens.add({
        targets: this.currentImage,
        alpha: 1,
        duration: duration,
        ease: 'Power2',
      })
    }
  }

  /**
   * フェードアウト効果でキャラクターを非表示にする
   * @param duration フェード時間（ミリ秒）
   */
  hideWithFadeOut(duration: number = 500): void {
    if (!this.currentImage) {
      return
    }

    this.scene.tweens.add({
      targets: this.currentImage,
      alpha: 0,
      duration: duration,
      ease: 'Power2',
      onComplete: () => {
        this.hide()
      },
    })
  }

  /**
   * 現在の画像を破棄する
   */
  private hideCurrentImage(): void {
    if (this.currentImage) {
      this.currentImage.destroy()
      this.currentImage = null
    }
  }

  /**
   * キャラクターのクリーンアップ
   */
  destroy(): void {
    this.hideCurrentImage()
    this.isVisible = false
  }
}
