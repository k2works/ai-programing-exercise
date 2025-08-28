import Phaser from 'phaser'

/**
 * タイトルシーン
 * ゲームのタイトル画面を表示し、プレイヤーの入力を待つ
 */
export class TitleScene extends Phaser.Scene {
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
      this.logo = this.add.image(width / 2, height / 2 - 150, 'phaser-logo')
      this.logo.setScale(0.3)
    }

    // ゲームタイトル
    this.add
      .text(width / 2, height / 2 - 50, 'TypeScript Novel Game', {
        fontFamily: 'Arial',
        fontSize: '48px',
        color: '#ffffff',
        stroke: '#000000',
        strokeThickness: 4,
      })
      .setOrigin(0.5)

    // メニューオプション
    this.createMenuOptions()
  }

  /**
   * メニューオプションを作成
   */
  private createMenuOptions(): void {
    const { width, height } = this.cameras.main

    // 新しい統合ゲームオプション
    const integratedGameOption = this.add
      .text(width / 2, height / 2 + 30, '🎮 統合ゲーム体験 (1キー)', {
        fontFamily: 'Arial',
        fontSize: '18px',
        color: '#3498db',
        stroke: '#000000',
        strokeThickness: 2,
      })
      .setOrigin(0.5)
      .setInteractive()

    integratedGameOption.on('pointerover', () => {
      integratedGameOption.setScale(1.1)
    })

    integratedGameOption.on('pointerout', () => {
      integratedGameOption.setScale(1.0)
    })

    integratedGameOption.on('pointerdown', () => {
      this.startIntegratedGame()
    })

    // シナリオデモオプション
    const scenarioOption = this.add
      .text(width / 2, height / 2 + 70, '📖 シナリオデモ (2キー)', {
        fontFamily: 'Arial',
        fontSize: '18px',
        color: '#9b59b6',
        stroke: '#000000',
        strokeThickness: 2,
      })
      .setOrigin(0.5)
      .setInteractive()

    scenarioOption.on('pointerover', () => {
      scenarioOption.setScale(1.1)
    })

    scenarioOption.on('pointerout', () => {
      scenarioOption.setScale(1.0)
    })

    scenarioOption.on('pointerdown', () => {
      this.startScenarioDemo()
    })

    // 従来のテストオプション
    const originalTestOption = this.add
      .text(width / 2, height / 2 + 110, '🧪 従来のテスト (3キー)', {
        fontFamily: 'Arial',
        fontSize: '18px',
        color: '#e74c3c',
        stroke: '#000000',
        strokeThickness: 2,
      })
      .setOrigin(0.5)
      .setInteractive()

    originalTestOption.on('pointerover', () => {
      originalTestOption.setScale(1.1)
    })

    originalTestOption.on('pointerout', () => {
      originalTestOption.setScale(1.0)
    })

    originalTestOption.on('pointerdown', () => {
      this.startOriginalTest()
    })

    // 説明テキスト
    this.add
      .text(
        width / 2,
        height / 2 + 170,
        '統合ゲーム: 全機能デモ\nシナリオデモ: 本格ストーリー体験\n従来テスト: キャラクター表示のみ',
        {
          fontFamily: 'Arial',
          fontSize: '14px',
          color: '#bdc3c7',
          align: 'center',
        }
      )
      .setOrigin(0.5)
  }

  /**
   * 入力設定
   */
  private setupInput(): void {
    // 数字キー 1: 統合ゲーム
    this.input.keyboard?.addKey('ONE').on('down', () => {
      this.startIntegratedGame()
    })

    // 数字キー 2: シナリオデモ
    this.input.keyboard?.addKey('TWO').on('down', () => {
      this.startScenarioDemo()
    })

    // 数字キー 3: 従来テスト
    this.input.keyboard?.addKey('THREE').on('down', () => {
      this.startOriginalTest()
    })

    // スペースキー: デフォルトで統合ゲーム
    this.input.keyboard?.addKey('SPACE').on('down', () => {
      this.startIntegratedGame()
    })
  }

  /**
   * 統合ゲームを開始
   */
  private startIntegratedGame(): void {
    this.cameras.main.fadeOut(500, 0, 0, 0)
    this.cameras.main.once('camerafadeoutcomplete', () => {
      this.scene.start('GameScene') // まずは既存のGameSceneで確認
    })
  }

  /**
   * シナリオデモを開始
   */
  private startScenarioDemo(): void {
    this.cameras.main.fadeOut(500, 0, 0, 0)
    this.cameras.main.once('camerafadeoutcomplete', () => {
      this.scene.start('ScenarioScene')
    })
  }

  /**
   * 従来のテストを開始
   */
  private startOriginalTest(): void {
    this.cameras.main.fadeOut(500, 0, 0, 0)
    this.cameras.main.once('camerafadeoutcomplete', () => {
      this.scene.start('TestScene') // TestSceneに遷移
    })
  }
}
