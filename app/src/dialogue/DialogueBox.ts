/**
 * ダイアログボックスシステム
 * ノベルゲームでのテキスト表示・会話進行を担当
 */

import * as Phaser from 'phaser'

export interface DialogueMessage {
  speaker?: string
  text: string
}

export class DialogueBox {
  public isVisible: boolean = false
  public currentText: string = ''
  public currentSpeaker: string = ''
  public currentMessageIndex: number = 0
  public isFinished: boolean = false
  public readonly width: number
  public readonly height: number

  private scene: Phaser.Scene
  private messages: string[] = []
  private backgroundBox: Phaser.GameObjects.Rectangle | null = null
  private textObject: Phaser.GameObjects.Text | null = null
  private speakerObject: Phaser.GameObjects.Text | null = null

  constructor(scene: Phaser.Scene, width: number, height: number = 100) {
    this.scene = scene
    this.width = width
    this.height = height
    this.createDialogueBox()
    this.setupClickHandler()
  }

  /**
   * ダイアログボックスのUI要素を作成
   */
  private createDialogueBox(): void {
    // 背景ボックス
    this.backgroundBox = this.scene.add.rectangle(
      this.width / 2,
      this.scene.scale.height - this.height / 2,
      this.width,
      this.height,
      0x000000,
      0.8
    )
    this.backgroundBox.setStrokeStyle(2, 0xffffff)
    this.backgroundBox.setVisible(false)

    // 話者名テキスト
    this.speakerObject = this.scene.add.text(20, this.scene.scale.height - this.height - 30, '', {
      fontSize: '16px',
      color: '#ffffff',
      backgroundColor: '#333333',
      padding: { x: 10, y: 5 },
    })
    this.speakerObject.setVisible(false)

    // メインテキスト
    this.textObject = this.scene.add.text(20, this.scene.scale.height - this.height + 10, '', {
      fontSize: '18px',
      color: '#ffffff',
      wordWrap: { width: this.width - 40 },
    })
    this.textObject.setVisible(false)
  }

  /**
   * クリックハンドラーを設定
   */
  private setupClickHandler(): void {
    this.scene.input.on('pointerdown', () => {
      if (this.isVisible) {
        this.handleClick()
      }
    })
  }

  /**
   * ダイアログボックスを表示
   */
  public show(): void {
    this.isVisible = true
    this.backgroundBox?.setVisible(true)
    this.textObject?.setVisible(true)

    if (this.currentSpeaker) {
      this.speakerObject?.setVisible(true)
    }
  }

  /**
   * ダイアログボックスを非表示
   */
  public hide(): void {
    this.isVisible = false
    this.backgroundBox?.setVisible(false)
    this.textObject?.setVisible(false)
    this.speakerObject?.setVisible(false)
  }

  /**
   * テキストを設定
   */
  public setText(text: string): void {
    this.currentText = text
    this.textObject?.setText(text)
  }

  /**
   * テキストを設定して表示
   */
  public setTextAndShow(text: string): void {
    this.setText(text)
    this.show()
  }

  /**
   * 話者名を設定
   */
  public setSpeaker(speaker: string): void {
    this.currentSpeaker = speaker
    this.speakerObject?.setText(speaker)

    if (this.isVisible) {
      this.speakerObject?.setVisible(speaker !== '')
    }
  }

  /**
   * メッセージ配列を設定
   */
  public setMessages(messages: string[]): void {
    this.messages = messages
    this.currentMessageIndex = 0
    this.isFinished = false

    if (messages.length > 0) {
      this.setText(messages[0])
    }
  }

  /**
   * 次のメッセージに進む
   */
  public nextMessage(): void {
    if (this.hasNextMessage()) {
      this.currentMessageIndex++
      this.setText(this.messages[this.currentMessageIndex])
    } else {
      this.isFinished = true
    }
  }

  /**
   * 次のメッセージが存在するかチェック
   */
  public hasNextMessage(): boolean {
    return this.currentMessageIndex < this.messages.length - 1
  }

  /**
   * クリックイベントハンドラー
   */
  public handleClick(): void {
    this.nextMessage()
  }
}
