import Phaser from 'phaser'

/**
 * ローディングシーン
 * ゲーム開始時にアセットを読み込み、進捗を表示する
 */
export class LoadingScene extends Phaser.Scene {
  private loadingText!: Phaser.GameObjects.Text
  private progressBar!: Phaser.GameObjects.Graphics
  private progressBox!: Phaser.GameObjects.Graphics

  constructor() {
    super({ key: 'loading' })
  }

  public preload(): void {
    this.createLoadingUI()
    this.setupLoadingEvents()
    this.loadAssets()
  }

  public create(): void {
    // ローディング完了後、タイトルシーンに遷移
    this.time.delayedCall(1000, () => {
      this.scene.start('title')
    })
  }

  /**
   * ローディング画面のUI要素を作成
   */
  private createLoadingUI(): void {
    const { width, height } = this.cameras.main

    // 背景色設定
    this.cameras.main.setBackgroundColor('#1a1a1a')

    // ローディングテキスト
    this.loadingText = this.add
      .text(width / 2, height / 2 - 50, 'Loading...', {
        fontFamily: 'Arial',
        fontSize: '24px',
        color: '#ffffff',
      })
      .setOrigin(0.5)

    // プログレスバーの背景
    this.progressBox = this.add.graphics()
    this.progressBox.fillStyle(0x222222)
    this.progressBox.fillRect(width / 2 - 160, height / 2, 320, 20)

    // プログレスバー
    this.progressBar = this.add.graphics()
  }

  /**
   * ローディングイベントを設定
   */
  private setupLoadingEvents(): void {
    // ローディング進捗の更新
    this.load.on('progress', (value: number) => {
      this.updateProgressBar(value)
      this.loadingText.setText(`Loading... ${Math.round(value * 100)}%`)
    })

    // ファイル読み込み完了
    this.load.on('fileprogress', (file: Phaser.Loader.File) => {
      console.log(`Loaded: ${file.key}`)
    })

    // 全ファイル読み込み完了
    this.load.on('complete', () => {
      this.loadingText.setText('Complete!')
      console.log('All assets loaded')
    })
  }

  /**
   * プログレスバーの更新
   */
  private updateProgressBar(value: number): void {
    const { width, height } = this.cameras.main

    this.progressBar.clear()
    this.progressBar.fillStyle(0x00ff00)
    this.progressBar.fillRect(width / 2 - 158, height / 2 + 2, 316 * value, 16)
  }

  /**
   * アセットの読み込み
   * 現在は最小限のアセットのみ
   */
  private loadAssets(): void {
    // Phaser3のロゴ（テスト用）
    this.load.image('phaser-logo', 'https://labs.phaser.io/assets/sprites/phaser3-logo.png')

    // 今後ここに実際のゲームアセットを追加
    // this.load.image('background', 'assets/images/background.png');
    // this.load.image('character', 'assets/images/character.png');
  }
}
