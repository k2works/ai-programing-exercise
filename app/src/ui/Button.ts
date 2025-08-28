import Phaser from 'phaser'

/**
 * ボタンのスタイル設定
 */
export interface ButtonStyle {
  backgroundColor?: number
  textColor?: string
  fontSize?: string
  fontFamily?: string
  borderColor?: number
  borderWidth?: number
}

/**
 * UIボタンコンポーネント
 * ゲーム内で使用する汎用ボタン
 */
export class Button {
  private rectangle: Phaser.GameObjects.Rectangle
  private text: Phaser.GameObjects.Text
  private onClick: () => void
  public enabled: boolean = true

  private defaultStyle: ButtonStyle = {
    backgroundColor: 0x4caf50,
    textColor: '#FFFFFF',
    fontSize: '18px',
    fontFamily: 'Arial',
    borderColor: 0x388e3c,
    borderWidth: 2,
  }

  constructor(
    scene: Phaser.Scene,
    x: number,
    y: number,
    width: number,
    height: number,
    text: string,
    onClick: () => void,
    style?: ButtonStyle
  ) {
    this.onClick = onClick
    const mergedStyle = { ...this.defaultStyle, ...style }

    // 背景矩形作成
    this.rectangle = scene.add.rectangle(x, y, width, height, mergedStyle.backgroundColor)
    this.rectangle.setOrigin(0.5, 0.5)
    this.rectangle.setStrokeStyle(mergedStyle.borderWidth!, mergedStyle.borderColor)

    // テキスト作成
    this.text = scene.add.text(x, y, text, {
      fontSize: mergedStyle.fontSize,
      color: mergedStyle.textColor,
      fontFamily: mergedStyle.fontFamily,
    })
    this.text.setOrigin(0.5, 0.5)

    // インタラクション設定
    this.setupInteraction()
  }

  /**
   * インタラクション（クリック、ホバー）設定
   */
  private setupInteraction(): void {
    this.rectangle.setInteractive()

    // クリックイベント
    this.rectangle.on('pointerdown', () => {
      if (this.enabled) {
        this.onClick()
      }
    })

    // ホバーイベント
    this.rectangle.on('pointerover', () => {
      if (this.enabled) {
        this.rectangle.setFillStyle(0x66bb6a) // 明るい緑
        this.text.setScale(1.05)
      }
    })

    this.rectangle.on('pointerout', () => {
      if (this.enabled) {
        this.rectangle.setFillStyle(this.defaultStyle.backgroundColor!)
        this.text.setScale(1.0)
      }
    })
  }

  /**
   * ボタンの有効/無効を設定
   */
  setEnabled(enabled: boolean): void {
    this.enabled = enabled

    if (enabled) {
      this.rectangle.setFillStyle(this.defaultStyle.backgroundColor!)
      this.rectangle.setAlpha(1.0)
      this.text.setAlpha(1.0)
    } else {
      this.rectangle.setFillStyle(0x9e9e9e) // グレー
      this.rectangle.setAlpha(0.6)
      this.text.setAlpha(0.6)
    }
  }

  /**
   * ボタンのテキストを変更
   */
  setText(newText: string): void {
    this.text.setText(newText)
  }

  /**
   * ボタンの位置を設定
   */
  setPosition(x: number, y: number): void {
    this.rectangle.setPosition(x, y)
    this.text.setPosition(x, y)
  }

  /**
   * ボタンを破棄
   */
  destroy(): void {
    this.rectangle.destroy()
    this.text.destroy()
  }

  /**
   * ボタンの表示/非表示を設定
   */
  setVisible(visible: boolean): void {
    this.rectangle.setVisible(visible)
    this.text.setVisible(visible)
  }
}
