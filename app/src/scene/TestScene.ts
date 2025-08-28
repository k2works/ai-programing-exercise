import Phaser from 'phaser'

/**
 * テストシーン
 * 基本的な動作確認のためのシーン
 */
export class TestScene extends Phaser.Scene {
  constructor() {
    super({ key: 'test' })
  }

  public create(): void {
    const { width, height } = this.cameras.main

    // 背景色設定
    this.cameras.main.setBackgroundColor('#34495e')

    // テストメッセージ
    this.add
      .text(width / 2, height / 2, 'Test Scene\n\nGame is working!', {
        fontFamily: 'Arial',
        fontSize: '32px',
        color: '#ffffff',
        align: 'center',
      })
      .setOrigin(0.5)

    // クリックでタイトルに戻る
    this.input.once('pointerdown', () => {
      this.scene.start('title')
    })

    // ESCキーでタイトルに戻る
    this.input.keyboard?.once('keydown-ESC', () => {
      this.scene.start('title')
    })

    console.log('Test scene created successfully')
  }
}
