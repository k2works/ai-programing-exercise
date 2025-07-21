import { Game } from './game'
import { GameRenderer } from './renderer'
import { InputHandler } from './input'

class PuyoPuyoWebApp {
  private game: Game
  private renderer: GameRenderer
  private inputHandler: InputHandler
  private gameLoop: number | null = null
  private isRunning: boolean = false

  // UI要素
  private scoreDisplay: HTMLElement
  private chainDisplay: HTMLElement
  private statusDisplay: HTMLElement
  private startButton: HTMLElement
  private resetButton: HTMLElement

  constructor() {
    this.game = new Game()

    // キャンバス要素取得
    const canvas = document.getElementById('game-canvas') as HTMLCanvasElement
    const nextCanvas = document.getElementById(
      'next-canvas'
    ) as HTMLCanvasElement

    if (!canvas || !nextCanvas) {
      throw new Error('キャンバス要素が見つかりません')
    }

    const ctx = canvas.getContext('2d')
    const nextCtx = nextCanvas.getContext('2d')

    if (!ctx || !nextCtx) {
      throw new Error('描画コンテキストを取得できませんでした')
    }

    this.renderer = new GameRenderer(ctx, nextCtx, canvas, nextCanvas)
    this.inputHandler = new InputHandler(this.game)

    // UI要素取得
    this.scoreDisplay = document.getElementById('score-display')!
    this.chainDisplay = document.getElementById('chain-display')!
    this.statusDisplay = document.getElementById('status-display')!
    this.startButton = document.getElementById('start-button')!
    this.resetButton = document.getElementById('reset-button')!

    this.setupEventListeners()
    this.updateUI()
  }

  private setupEventListeners(): void {
    this.startButton.addEventListener('click', () => {
      this.startGame()
    })

    this.resetButton.addEventListener('click', () => {
      this.resetGame()
    })
  }

  startGame(): void {
    if (this.isRunning) return

    this.game.initialize()
    this.isRunning = true
    this.gameLoop = requestAnimationFrame(() => this.tick())
  }

  resetGame(): void {
    this.stopGame()
    this.game.reset()
    this.updateUI()
    this.renderer.render(this.game)
  }

  private stopGame(): void {
    this.isRunning = false
    if (this.gameLoop) {
      cancelAnimationFrame(this.gameLoop)
      this.gameLoop = null
    }
  }

  private tick(): void {
    if (!this.isRunning) return

    // ゲーム更新
    this.game.update()

    // 描画更新
    this.renderer.render(this.game)
    this.updateUI()

    // ゲームオーバー判定
    if (this.game.getMode() === 'gameOver') {
      this.handleGameOver()
      return
    }

    // 次フレーム
    this.gameLoop = requestAnimationFrame(() => this.tick())
  }

  private updateUI(): void {
    // リアルタイムUI更新
    this.scoreDisplay.textContent = this.game.getScore().toString()
    this.chainDisplay.textContent = this.game.getCombinationCount().toString()

    // 状態別表示とスタイリング
    const mode = this.game.getMode()
    let statusText = ''
    let statusClass = ''

    switch (mode) {
      case 'start':
      case 'newPuyo':
        statusText = 'Ready'
        statusClass = 'ready'
        break
      case 'playing':
        statusText = 'Playing'
        statusClass = 'playing'
        break
      case 'gameOver':
        statusText = 'Game Over'
        statusClass = 'game-over'
        break
      case 'erasing':
        statusText = 'Chain!'
        statusClass = 'erasing'
        break
      case 'fall':
      case 'checkFall':
        statusText = 'Falling'
        statusClass = 'falling'
        break
      default:
        statusText = mode
        break
    }

    this.statusDisplay.textContent = statusText
    this.statusDisplay.className = 'info-value'
    if (statusClass) {
      this.statusDisplay.classList.add(statusClass)
    }
  }

  private handleGameOver(): void {
    this.stopGame()
    this.updateUI()

    // ゲームオーバー処理
    setTimeout(() => {
      if (confirm('ゲームオーバー！\n再度プレイしますか？')) {
        this.startGame()
      }
    }, 500)
  }

  cleanup(): void {
    this.stopGame()
    this.inputHandler.cleanup()
  }
}

// アプリケーション初期化
document.addEventListener('DOMContentLoaded', () => {
  console.log('🎮 Puyo Puyo Game Starting...')

  try {
    const app = new PuyoPuyoWebApp()

    // ページ離脱時のクリーンアップ
    window.addEventListener('beforeunload', () => {
      app.cleanup()
    })
  } catch (error) {
    console.error('アプリケーションの初期化に失敗しました:', error)
    alert('ゲームの初期化に失敗しました。ページを再読み込みしてください。')
  }
})
