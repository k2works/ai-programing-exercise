import Phaser from 'phaser'
import { DialogueBox } from '../dialogue/DialogueBox'
import { ScenarioManager } from '../story/ScenarioManager'
import { sampleScenario } from '../story/data/sampleScenario'
import type { ChoiceData } from '../story/types'

/**
 * シナリオデモシーン
 * ScenarioManagerを使用した本格的なストーリー体験
 */
export class ScenarioScene extends Phaser.Scene {
  private dialogueBox!: DialogueBox
  private scenarioManager!: ScenarioManager
  private choiceButtons: Phaser.GameObjects.Text[] = []
  private choiceContainer!: Phaser.GameObjects.Container
  private isProcessing: boolean = false

  constructor() {
    super({ key: 'ScenarioScene' })
  }

  preload(): void {
    this.setupPlaceholderAssets()
  }

  create(): void {
    // カメラフェードイン
    this.cameras.main.fadeIn(500, 0, 0, 0)

    // 背景
    this.add.rectangle(400, 300, 800, 600, 0x2c3e50) // ダークブルー背景

    // システム初期化
    this.initializeSystems()
    this.setupUI()
    this.setupInput()

    // シナリオ開始
    this.startScenario()
  }

  /**
   * プレースホルダーアセット設定
   */
  private setupPlaceholderAssets(): void {
    // 基本的な色付き矩形テクスチャを作成
    this.add.graphics()
      .fillStyle(0x4a90e2)
      .fillRect(0, 0, 64, 64)
      .generateTexture('blue-placeholder', 64, 64)
      .destroy()
  }

  /**
   * システム初期化
   */
  private initializeSystems(): void {
    console.log('Initializing scenario systems...')

    // シナリオマネージャー初期化
    this.scenarioManager = new ScenarioManager()
    this.scenarioManager.loadScenario(sampleScenario)

    // ダイアログボックス初期化
    this.dialogueBox = new DialogueBox(this, 750, 120)

    // 選択肢コンテナ初期化
    this.choiceContainer = this.add.container(400, 350)
    this.choiceContainer.setVisible(false)

    console.log('Scenario systems initialized')
  }

  /**
   * UI要素設定
   */
  private setupUI(): void {
    // タイトル
    this.add
      .text(400, 30, '📖 シナリオデモ - はじまりの物語', {
        fontSize: '20px',
        color: '#ffffff',
        stroke: '#000000',
        strokeThickness: 2,
      })
      .setOrigin(0.5)

    // 操作説明
    this.add
      .text(400, 570, 'スペースキー: 次へ | 選択肢: クリックまたは数字キー | ESC: タイトルに戻る', {
        fontSize: '14px',
        color: '#cccccc',
      })
      .setOrigin(0.5)
  }

  /**
   * 入力処理設定
   */
  private setupInput(): void {
    // スペースキーで次へ
    const spaceKey = this.input.keyboard!.addKey(Phaser.Input.Keyboard.KeyCodes.SPACE)
    spaceKey.on('down', () => {
      this.handleNextInput()
    })

    // クリックで次へ
    this.input.on('pointerdown', () => {
      this.handleNextInput()
    })

    // ESCキーでタイトルに戻る
    const escKey = this.input.keyboard!.addKey(Phaser.Input.Keyboard.KeyCodes.ESC)
    escKey.on('down', () => {
      console.log('ESC key pressed in ScenarioScene - returning to title')
      this.returnToTitle()
    })

    // 数字キーで選択肢選択
    for (let i = 1; i <= 9; i++) {
      const key = this.input.keyboard!.addKey(Phaser.Input.Keyboard.KeyCodes[`DIGIT_${i}` as keyof typeof Phaser.Input.Keyboard.KeyCodes])
      key.on('down', () => {
        if (this.choiceContainer.visible && this.choiceButtons.length >= i) {
          this.selectChoiceByIndex(i - 1)
        }
      })
    }
  }

  /**
   * シナリオ開始
   */
  private startScenario(): void {
    console.log('Starting scenario...')
    this.displayCurrentScene()
  }

  /**
   * 現在のシーンを表示
   */
  private displayCurrentScene(): void {
    if (this.isProcessing) return

    try {
      const currentScene = this.scenarioManager.getCurrentScene()
      console.log('Displaying scene:', currentScene.id)

      // ダイアログ表示
      this.dialogueBox.setSpeaker(this.getCharacterDisplayName(currentScene.character || 'narrator'))
      this.dialogueBox.setMessages([currentScene.text])
      this.dialogueBox.show()

      // 選択肢がある場合は表示
      if (this.scenarioManager.hasChoices()) {
        this.time.delayedCall(500, () => {
          this.showChoices()
        })
      }
    } catch (error) {
      console.error('Error displaying scene:', error)
      this.showCompletionMessage()
    }
  }

  /**
   * 選択肢を表示
   */
  private showChoices(): void {
    const choices = this.scenarioManager.getChoices()
    console.log('Showing choices:', choices.length)

    // 既存の選択肢ボタンをクリア
    this.clearChoiceButtons()

    // 新しい選択肢ボタンを作成
    choices.forEach((choice, index) => {
      const y = index * 60 - (choices.length - 1) * 30
      
      // 選択肢の背景
      const bg = this.add.rectangle(0, y, 400, 50, 0x4a90e2, 0.8)
      bg.setInteractive()
      bg.on('pointerdown', () => this.handleChoiceSelection(choice.id))
      bg.on('pointerover', () => bg.setFillStyle(0x5ba0f2))
      bg.on('pointerout', () => bg.setFillStyle(0x4a90e2))

      // 選択肢のテキスト
      const text = this.add.text(0, y, `${index + 1}. ${choice.text}`, {
        fontSize: '16px',
        color: '#ffffff',
        wordWrap: { width: 380 }
      })
      text.setOrigin(0.5)

      this.choiceContainer.add([bg, text])
      this.choiceButtons.push(text)
    })

    this.choiceContainer.setVisible(true)
  }

  /**
   * 選択肢ボタンをクリア
   */
  private clearChoiceButtons(): void {
    this.choiceContainer.removeAll(true)
    this.choiceButtons = []
  }

  /**
   * インデックスで選択肢を選択
   */
  private selectChoiceByIndex(index: number): void {
    if (index < this.choiceButtons.length) {
      const choices = this.scenarioManager.getChoices()
      this.handleChoiceSelection(choices[index].id)
    }
  }

  /**
   * 選択肢選択処理
   */
  private handleChoiceSelection(choiceId: string): void {
    if (this.isProcessing) return

    console.log('Choice selected:', choiceId)
    this.isProcessing = true

    // 選択肢を非表示
    this.choiceContainer.setVisible(false)

    // 選択実行
    try {
      this.scenarioManager.selectChoice(choiceId)
      
      // 少し待ってから次のシーンを表示
      this.time.delayedCall(300, () => {
        this.isProcessing = false
        this.displayCurrentScene()
      })
    } catch (error) {
      console.error('Error in choice selection:', error)
      this.isProcessing = false
      this.showCompletionMessage()
    }
  }

  /**
   * 入力処理
   */
  private handleNextInput(): void {
    if (this.isProcessing) return

    // 選択肢が表示中の場合は何もしない
    if (this.choiceContainer.visible) {
      return
    }

    // ダイアログが表示中の場合は非表示にして次へ
    if (this.dialogueBox.isVisible) {
      this.dialogueBox.hide()

      // 選択肢がない場合は次のシーンに進む
      if (!this.scenarioManager.hasChoices()) {
        this.time.delayedCall(300, () => {
          this.proceedToNextScene()
        })
      }
    }
  }

  /**
   * 次のシーンに進む
   */
  private proceedToNextScene(): void {
    try {
      this.scenarioManager.nextScene()
      this.displayCurrentScene()
    } catch (error) {
      console.log('Scenario completed or error:', error)
      this.showCompletionMessage()
    }
  }

  /**
   * キャラクター表示名取得
   */
  private getCharacterDisplayName(character: string): string {
    const characterNames: Record<string, string> = {
      narrator: 'ナレーター',
      wise_man: '賢者',
    }
    return characterNames[character] || character
  }

  /**
   * 完了メッセージ表示
   */
  private showCompletionMessage(): void {
    console.log('Showing completion message')
    
    this.dialogueBox.setSpeaker('システム')
    this.dialogueBox.setMessages([
      '🎉 シナリオデモが完了しました！',
      '',
      '✅ ScenarioManager: JSON形式シナリオデータ管理',
      '✅ 分岐フロー: 選択肢による物語の分岐',
      '✅ ストーリー進行: 順次的なシーン遷移',
      '',
      '3秒後にタイトルに戻ります。'
    ])
    this.dialogueBox.show()

    this.time.delayedCall(3000, () => {
      console.log('Auto return to title after completion')
      this.returnToTitle()
    })
  }

  /**
   * タイトルに戻る
   */
  private returnToTitle(): void {
    console.log('returnToTitle() called from ScenarioScene - starting fade out')
    this.cameras.main.fadeOut(500, 0, 0, 0)
    this.cameras.main.once('camerafadeoutcomplete', () => {
      console.log('Fade out complete - starting title scene')
      this.scene.start('title')
    })
  }
}
