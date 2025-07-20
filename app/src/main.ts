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
  private startButton: HTMLButtonElement
  private resetButton: HTMLButtonElement

  constructor() {
    console.log('🎮 Puyo Puyo Game Starting...')
    
    // ゲームオブジェクトを初期化
    this.game = new Game()
    
    // キャンバス要素を取得
    const canvas = document.getElementById('game-canvas') as HTMLCanvasElement
    const nextCanvas = document.getElementById('next-canvas') as HTMLCanvasElement
    
    if (!canvas || !nextCanvas) {
      throw new Error('Canvas elements not found')
    }

    // レンダラーと入力ハンドラーを初期化
    this.renderer = new GameRenderer(canvas, nextCanvas)
    this.inputHandler = new InputHandler(this.game)

    // UI要素を取得
    this.scoreDisplay = document.getElementById('score-display')!
    this.chainDisplay = document.getElementById('chain-display')!
    this.statusDisplay = document.getElementById('status-display')!
    this.startButton = document.getElementById('start-button') as HTMLButtonElement
    this.resetButton = document.getElementById('reset-button') as HTMLButtonElement

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

    // ウィンドウが閉じられる時にクリーンアップ
    window.addEventListener('beforeunload', () => {
      this.cleanup()
    })
  }

  private startGame(): void {
    if (this.isRunning) {
      return
    }

    console.log('Starting new game...')
    
    this.game.initialize()
    this.isRunning = true
    
    this.startButton.disabled = true
    this.resetButton.disabled = false

    // ゲームループを開始
    this.gameLoop = requestAnimationFrame(() => this.tick())
  }

  private resetGame(): void {
    console.log('Resetting game...')
    
    this.stopGameLoop()
    this.game.reset()
    this.isRunning = false
    
    this.startButton.disabled = false
    this.resetButton.disabled = true
    
    this.updateUI()
    this.renderer.render(this.game)
  }

  private tick(): void {
    if (!this.isRunning) {
      return
    }

    // ゲーム状態を更新
    this.game.update()
    
    // 画面を更新
    this.renderer.render(this.game)
    this.updateUI()

    // ゲームオーバー判定
    if (this.game['mode'] === 'gameOver') {
      this.handleGameOver()
      return
    }

    // 次のフレームをスケジュール
    this.gameLoop = requestAnimationFrame(() => this.tick())
  }

  private updateUI(): void {
    // スコア表示
    this.scoreDisplay.textContent = this.game['score'].getScore().toString()
    
    // 連鎖数表示
    this.chainDisplay.textContent = this.game['combinationCount'].toString()
    
    // ゲーム状態表示
    const mode = this.game['mode']
    let statusText = ''
    let statusClass = ''

    switch (mode) {
      case 'start':
        statusText = 'Ready'
        break
      case 'playing':
        statusText = 'Playing'
        statusClass = 'playing'
        break
      case 'gameOver':
        statusText = 'Game Over'
        statusClass = 'game-over'
        break
      case 'checkErase':
      case 'erasing':
        statusText = 'Erasing...'
        statusClass = 'erasing'
        break
      case 'fall':
      case 'checkFall':
        statusText = 'Falling...'
        statusClass = 'falling'
        break
      default:
        statusText = mode
    }

    this.statusDisplay.textContent = statusText
    this.statusDisplay.className = 'info-value'
    if (statusClass) {
      this.statusDisplay.classList.add(statusClass)
    }
  }

  private handleGameOver(): void {
    console.log('Game Over!')
    
    this.stopGameLoop()
    this.isRunning = false
    
    this.startButton.disabled = false
    this.resetButton.disabled = false
    
    this.updateUI()
  }

  private stopGameLoop(): void {
    if (this.gameLoop) {
      cancelAnimationFrame(this.gameLoop)
      this.gameLoop = null
    }
  }

  private cleanup(): void {
    this.stopGameLoop()
    this.inputHandler.destroy()
  }
}

// DOM読み込み完了後にアプリケーションを開始
document.addEventListener('DOMContentLoaded', () => {
  try {
    new PuyoPuyoWebApp()
  } catch (error) {
    console.error('Failed to initialize Puyo Puyo Game:', error)
    alert('ゲームの初期化に失敗しました。ページを再読み込みしてください。')
  }
})
