import Phaser from 'phaser'
import { CharacterManager } from '../character/CharacterManager'

/**
 * ゲームメインシーン
 * キャラクター表示とダイアログシステムの統合
 */
export class GameScene extends Phaser.Scene {
  public characterManager!: CharacterManager
  private currentStep: number = 0
  private testSteps: string[] = [
    'Aliceを表示します（通常表情）',
    'Aliceの表情を嬉しそうに変更',
    'Aliceの表情を悲しそうに変更',
    'Aliceを非表示にします',
    'テストを完了。タイトルに戻ります',
  ]

  constructor() {
    super({ key: 'GameScene' })
  }

  preload(): void {
    // キャラクター画像の読み込み設定
    // 実際の画像ファイルがない場合はPhaser3デフォルトの矩形で代用
    this.setupPlaceholderAssets()
  }

  create(): void {
    // カメラフェードイン
    this.cameras.main.fadeIn(500, 0, 0, 0)

    // 背景
    this.add.rectangle(400, 300, 800, 600, 0x87ceeb) // 空色背景

    // キャラクター管理システム初期化
    this.characterManager = new CharacterManager(this)
    this.setupCharacters()

    // UI要素
    this.setupUI()

    // 入力処理
    this.setupInput()

    // テスト開始
    this.startCharacterTest()
  }

  /**
   * プレースホルダーアセットの設定
   * 実際の画像がない場合の代替表示
   */
  private setupPlaceholderAssets(): void {
    // テクスチャをプログラムで生成
    this.load.image(
      'alice-normal',
      'data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mNkYPhfDwAChwGA60e6kgAAAABJRU5ErkJggg=='
    )
    this.load.image(
      'alice-happy',
      'data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mNkYPhfDwAChwGA60e6kgAAAABJRU5ErkJggg=='
    )
    this.load.image(
      'alice-sad',
      'data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mNkYPhfDwAChwGA60e6kgAAAABJRU5ErkJggg=='
    )
  }

  /**
   * キャラクター設定
   */
  private setupCharacters(): void {
    this.characterManager.registerCharacter('alice', ['normal', 'happy', 'sad'])
  }

  /**
   * UI要素の設定
   */
  private setupUI(): void {
    // タイトル
    this.add
      .text(400, 50, 'キャラクターシステムテスト', {
        fontSize: '24px',
        color: '#ffffff',
        stroke: '#000000',
        strokeThickness: 2,
      })
      .setOrigin(0.5)

    // 説明テキスト
    this.add
      .text(400, 550, 'スペースキー または クリック で次のステップ', {
        fontSize: '16px',
        color: '#ffffff',
        stroke: '#000000',
        strokeThickness: 1,
      })
      .setOrigin(0.5)

    // 現在のステップ表示
    this.updateStepDisplay()
  }

  /**
   * 入力処理の設定
   */
  private setupInput(): void {
    // スペースキー
    const spaceKey = this.input.keyboard!.addKey(Phaser.Input.Keyboard.KeyCodes.SPACE)
    spaceKey.on('down', () => {
      this.nextStep()
    })

    // マウスクリック
    this.input.on('pointerdown', () => {
      this.nextStep()
    })

    // ESCキーでタイトルに戻る
    const escKey = this.input.keyboard!.addKey(Phaser.Input.Keyboard.KeyCodes.ESC)
    escKey.on('down', () => {
      this.scene.start('TitleScene')
    })
  }

  /**
   * キャラクターテストの開始
   */
  private startCharacterTest(): void {
    this.currentStep = 0
    this.updateStepDisplay()
  }

  /**
   * 次のステップに進む
   */
  private nextStep(): void {
    this.currentStep++

    switch (this.currentStep) {
      case 1:
        // Aliceを表示（通常表情）
        this.characterManager.showCharacterWithFadeIn('alice', 'normal', { x: 400, y: 450 })
        break

      case 2:
        // 表情を嬉しそうに変更
        this.characterManager.changeCharacterExpression('alice', 'happy')
        break

      case 3:
        // 表情を悲しそうに変更
        this.characterManager.changeCharacterExpression('alice', 'sad')
        break

      case 4:
        // Aliceを非表示
        this.characterManager.hideCharacterWithFadeOut('alice')
        break

      case 5:
        // テスト完了、タイトルに戻る
        this.cameras.main.fadeOut(500, 0, 0, 0)
        this.cameras.main.once('camerafadeoutcomplete', () => {
          this.scene.start('TitleScene')
        })
        return

      default:
        return
    }

    this.updateStepDisplay()
  }

  /**
   * ステップ表示の更新
   */
  private updateStepDisplay(): void {
    // 既存のステップテキストを削除
    this.children.getChildren().forEach((child) => {
      if (child.getData && child.getData('stepText')) {
        child.destroy()
      }
    })

    // 新しいステップテキストを追加
    const stepText =
      this.currentStep < this.testSteps.length ? this.testSteps[this.currentStep] : 'テスト完了'

    const text = this.add
      .text(400, 100, `ステップ ${this.currentStep + 1}: ${stepText}`, {
        fontSize: '18px',
        color: '#ffffff',
        stroke: '#000000',
        strokeThickness: 1,
        wordWrap: { width: 700 },
      })
      .setOrigin(0.5)

    text.setData('stepText', true)
  }
}
