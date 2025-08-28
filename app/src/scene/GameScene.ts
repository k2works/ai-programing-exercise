import Phaser from 'phaser'
import { CharacterManager } from '../character/CharacterManager'
import { DialogueBox } from '../dialogue/DialogueBox'
import { EffectManager } from '../effect/EffectManager'
import { BackgroundManager } from '../effect/BackgroundManager'

/**
 * 統合ゲームシーン
 * ストーリー、選択肢、エフェクトシステムを統合したメインゲームシーン
 */
export class GameScene extends Phaser.Scene {
	// コアシステム
  public characterManager!: CharacterManager
  public dialogueBox!: DialogueBox

	// エフェクトシステム
	private effectManager!: EffectManager
	private backgroundManager!: BackgroundManager

	// ゲーム状態
	private currentSegmentIndex: number = 0
	private isProcessing: boolean = false

  constructor() {
    super({ key: 'GameScene' })
  }

	preload(): void {
    this.setupPlaceholderAssets()
  }

  create(): void {
    // カメラフェードイン
    this.cameras.main.fadeIn(500, 0, 0, 0)

		// システム初期化
		this.initializeCoreSystem()
		this.initializeEffectSystems()
		this.setupUI()
    this.setupInput()

		// デモシナリオ開始を少し遅らせる
		this.time.delayedCall(100, () => {
			this.startDemoScenario()
		})
  }

  /**
   * プレースホルダーアセットの設定
   */
  private setupPlaceholderAssets(): void {
		// キャラクター画像を生成
		this.createCharacterTexture('alice-normal', 0x8b4513) // 茶色（通常）
		this.createCharacterTexture('alice-happy', 0xffd700) // 金色（嬉しそう）
		this.createCharacterTexture('alice-sad', 0x4169e1)   // 青色（悲しそう）

		// 背景画像を生成
		this.createBackgroundTexture('demo-background', 0x87ceeb) // 空色背景
		this.createBackgroundTexture('forest-background', 0x228b22) // 森の背景
  }

  /**
   * キャラクターテクスチャを作成
   */
	private createCharacterTexture(key: string, color: number): void {
		const graphics = this.add.graphics()
		graphics.fillStyle(color)
		graphics.fillRoundedRect(0, 0, 200, 300, 20)
		graphics.generateTexture(key, 200, 300)
		graphics.destroy()
	}

	/**
	 * 背景テクスチャを作成
	 */
	private createBackgroundTexture(key: string, color: number): void {
		const graphics = this.add.graphics()
		graphics.fillStyle(color)
		graphics.fillRect(0, 0, 800, 600)
		graphics.generateTexture(key, 800, 600)
		graphics.destroy()
	}

	/**
	 * コアシステム初期化
	 */
	private initializeCoreSystem(): void {
		// キャラクター管理
		this.characterManager = new CharacterManager(this)
    this.characterManager.registerCharacter('alice', ['normal', 'happy', 'sad'])

		// ダイアログシステム
		this.dialogueBox = new DialogueBox(this, 750, 120)
  }

  /**
   * エフェクトシステム初期化
   */
	private initializeEffectSystems(): void {
		console.log('Initializing effect systems...')
		console.log('this.add exists:', !!this.add)

		this.effectManager = new EffectManager(this)
		this.backgroundManager = new BackgroundManager(this)

		console.log('Effect systems initialized')
	}

  /**
   * UI要素設定
   */
  private setupUI(): void {
    // タイトル
    this.add
			.text(400, 30, '🎉 統合ゲーム体験 - すべてのシステムのデモ', {
				fontSize: '20px',
        color: '#ffffff',
        stroke: '#000000',
        strokeThickness: 2,
      })
      .setOrigin(0.5)

		// 操作説明
    this.add
			.text(400, 570, 'スペース: 次へ | ESC: タイトルに戻る', {
				fontSize: '14px',
        color: '#ffffff',
        stroke: '#000000',
        strokeThickness: 1,
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
			console.log('ESC key pressed - returning to title')
			this.returnToTitle()
    })
  }

  /**
   * シンプルなデモシナリオ開始
   */
	private startDemoScenario(): void {
		this.currentSegmentIndex = 0
		this.processNextDemo()
	}

	/**
	 * 次のデモステップを実行
	 */
	private processNextDemo(): void {
		switch (this.currentSegmentIndex) {
			case 0:
				this.showIntroduction()
				break
			case 1:
				this.demonstrateCharacterSystem()
				break
			case 2:
				this.demonstrateEffectSystem()
				break
			case 3:
				this.demonstrateBackgroundSystem()
				break
			case 4:
				this.showCompletion()
				break
			default:
				this.endDemo()
				break
		}
	}

	/**
	 * イントロダクション表示
	 */
	private showIntroduction(): void {
		console.log('showIntroduction called')
		console.log('dialogueBox exists:', !!this.dialogueBox)

		this.dialogueBox.setSpeaker('システム')
		this.dialogueBox.setMessages([
			'🎉 統合ゲーム体験へようこそ！',
			'',
			'この画面では以下のシステムが統合されています：',
			'✅ STORY-001: シナリオ管理システム',
			'✅ CHOICE-001: 選択肢システム',
			'✅ EFFECT-001: エフェクトシステム',
			'',
			'それぞれのシステムをデモします。'
		])
    this.dialogueBox.show()

		console.log('DialogueBox should be visible now')

		// イントロエフェクト実行
		this.time.delayedCall(200, () => {
			console.log('Attempting flash effect...')
			if (this.effectManager) {
				this.effectManager.flash('white', 500)
			} else {
				console.error('effectManager is not initialized')
			}
		})
  }

  /**
   * キャラクターシステムのデモ
   */
	private demonstrateCharacterSystem(): void {
		this.dialogueBox.setSpeaker('アリス')
		this.dialogueBox.setMessages([
			'こんにちは！私はアリスです。',
			'',
			'キャラクターシステムのデモを行います。',
			'表情変更や位置移動ができます。',
			'',
			'※これから表情が変わります'
		])
		this.dialogueBox.show()

		// アリスを表示
		this.characterManager.showCharacterWithFadeIn('alice', 'normal', { x: 300, y: 400 })

		// エフェクト付きで表情変更
		this.time.delayedCall(1000, () => {
			this.characterManager.changeCharacterExpression('alice', 'happy')
			this.effectManager.flash('yellow', 300)
		})

		this.time.delayedCall(2000, () => {
			this.characterManager.changeCharacterExpression('alice', 'sad')
			this.effectManager.shake('light')
		})

		this.time.delayedCall(3000, () => {
			this.characterManager.changeCharacterExpression('alice', 'normal')
		})
	}

	/**
	 * エフェクトシステムのデモ
	 */
	private demonstrateEffectSystem(): void {
		this.dialogueBox.setSpeaker('システム')
		this.dialogueBox.setMessages([
			'次にエフェクトシステムのデモです。',
			'',
			'画面フラッシュ、シェイク、フェードなど',
			'様々な視覚効果を使用できます。',
			'',
			'※これから連続エフェクトが始まります'
		])
		this.dialogueBox.show()

		// 連続エフェクト
		this.time.delayedCall(500, () => this.effectManager.flash('red', 200))
		this.time.delayedCall(1000, () => this.effectManager.flash('green', 200))
		this.time.delayedCall(1500, () => this.effectManager.flash('blue', 200))
		this.time.delayedCall(2000, () => this.effectManager.shake('medium'))
		this.time.delayedCall(2500, () => this.effectManager.flash('purple', 300))
	}

	/**
	 * 背景システムのデモ
	 */
	private demonstrateBackgroundSystem(): void {
		this.dialogueBox.setSpeaker('システム')
		this.dialogueBox.setMessages([
			'背景システムのデモです。',
			'',
			'背景の変更やエフェクトが可能です。',
			'',
			'※背景が森に変わります'
		])
		this.dialogueBox.show()

		// 背景変更（フェード付き）
		this.time.delayedCall(1000, () => {
			this.backgroundManager.setBackground('forest-background', 1500)
		})
  }

  /**
   * デモ完了表示
   */
	private showCompletion(): void {
		this.dialogueBox.setSpeaker('システム')
		this.dialogueBox.setMessages([
			'🎉 すべてのシステムデモが完了しました！',
			'',
			'✅ キャラクター表示・表情変更',
			'✅ 各種エフェクト（フラッシュ・シェイク・フェード）',
			'✅ 背景変更システム',
			'',
			'これでイテレーション1からの大幅な進歩を確認できます！',
			'',
			'次へ進むと終了します。'
		])
		this.dialogueBox.show()

		// 祝福エフェクト
		this.effectManager.flash('yellow', 1000)
	}

	/**
	 * デモ終了
	 */
	private endDemo(): void {
		this.dialogueBox.setSpeaker('システム')
		this.dialogueBox.setMessages([
			'🎊 統合デモが完了しました！',
			'',
			'STORY-001 + CHOICE-001 + EFFECT-001',
			'すべてのシステムが正常に動作しています。',
			'',
			'3秒後にタイトルに戻ります。'
		])
		this.dialogueBox.show()

		this.time.delayedCall(3000, () => {
			console.log('Demo timeout reached - returning to title')
			this.returnToTitle()
    })
	}

	/**
	 * 入力処理
	 */
	private handleNextInput(): void {
		if (this.isProcessing) return

		// ダイアログが表示中の場合は非表示にして次へ
		if (this.dialogueBox.isVisible) {
			this.dialogueBox.hide()
			this.currentSegmentIndex++

			// 少し待ってから次のセグメントを処理
			this.time.delayedCall(300, () => {
				this.processNextDemo()
      })
		}
	}

	/**
	 * タイトルに戻る
	 */
	private returnToTitle(): void {
		console.log('returnToTitle() called - starting fade out')

		// エフェクトをクリーンアップ
		if (this.effectManager) {
			this.effectManager.cleanup()
		}

		this.cameras.main.fadeOut(500, 0, 0, 0)
		this.cameras.main.once('camerafadeoutcomplete', () => {
			console.log('Fade out complete - starting title scene')
			this.scene.start('title')
		})
  }
}
