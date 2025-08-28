import Phaser from 'phaser'

/**
 * タイトルシーン
 * ゲームのタイトル画面を表示し、プレイヤーの入力を待つ
 */
export class TitleScene extends Phaser.Scene {
  private startText!: Phaser.GameObjects.Text
  private logo!: Phaser.GameObjects.Image

  constructor() {
    super({ key: 'title' })
  }

  public create(): void {
    this.createTitleUI()
    this.setupInput()
  }

  /**
   * タイトル画面のUI要素を作成
   */
  private createTitleUI(): void {
    const { width, height } = this.cameras.main

    // 背景色設定
    this.cameras.main.setBackgroundColor('#2c3e50')

    // Phaser3ロゴ（読み込み済みのアセットを使用）
    if (this.textures.exists('phaser-logo')) {
      this.logo = this.add.image(width / 2, height / 2 - 100, 'phaser-logo')
      this.logo.setScale(0.3)
    }

    // ゲームタイトル
    this.add
      .text(width / 2, height / 2, 'TypeScript Novel Game', {
        fontFamily: 'Arial',
        fontSize: '48px',
        color: '#ffffff',
        stroke: '#000000',
        strokeThickness: 4,
      })
      .setOrigin(0.5)

    // スタート案内テキスト
    this.startText = this.add
      .text(width / 2, height / 2 + 100, 'Click to Start', {
        fontFamily: 'Arial',
        fontSize: '24px',
        color: '#ecf0f1',
      })
      .setOrigin(0.5)

    // 点滅効果を追加
    this.tweens.add({
      targets: this.startText,
      alpha: 0.3,
      duration: 1000,
      yoyo: true,
      repeat: -1,
    })
  }

  /**
   * 入力設定
   */
  private setupInput(): void {
    // クリック・タップでメインシーンに遷移
    this.input.once('pointerdown', () => {
      this.startGame()
    })

    // キーボード入力（スペースキーまたはEnter）
    this.input.keyboard?.once('keydown-SPACE', () => {
      this.startGame()
    })

    this.input.keyboard?.once('keydown-ENTER', () => {
      this.startGame()
    })
  }

  /**
   * ゲーム開始処理
   */
  private startGame(): void {
    // フェードアウト効果
    this.cameras.main.fadeOut(500, 0, 0, 0)

    // フェードアウト完了後にメインシーンに遷移
    this.cameras.main.once('camerafadeoutcomplete', () => {
      this.scene.start('GameScene') // キャラクターシステムテストシーンに遷移
    })
  }
}
