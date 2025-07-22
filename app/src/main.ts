import { Game } from './game'
import { GameRenderer } from './renderer'
import { InputHandler } from './input'
import { PerformanceMonitor, DrawOptimizer } from './performance'
import { globalErrorHandler, ErrorLevel, handleError, handleRenderingError, handleGameStateError } from './errorhandler'
import { soundManager, SoundType, SoundPresets } from './audio'
import { SettingsUI } from './settingsUI'
import { settingsManager } from './settings'
import { statisticsManager } from './statistics'

class PuyoPuyoWebApp {
  private game: Game
  private renderer: GameRenderer
  private inputHandler: InputHandler
  private gameLoop: number | null = null
  private isRunning: boolean = false
  private performanceMonitor: PerformanceMonitor
  private drawOptimizer: DrawOptimizer
  private settingsUI: SettingsUI

  // UI要素
  private scoreDisplay: HTMLElement
  private chainDisplay: HTMLElement
  private statusDisplay: HTMLElement
  private startButton: HTMLElement
  private resetButton: HTMLElement
  private settingsButton: HTMLElement
  private performanceDisplay?: HTMLElement
  
  // 統計表示要素
  private statsGamesDisplay: HTMLElement
  private statsHighDisplay: HTMLElement
  private statsAvgDisplay: HTMLElement
  private statsTimeDisplay: HTMLElement

  constructor() {
    try {
      this.game = new Game()
      
      // サウンドシステム初期化
      this.initializeAudio()

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
    this.performanceMonitor = new PerformanceMonitor()
    this.drawOptimizer = new DrawOptimizer()
    this.settingsUI = new SettingsUI()

    // UI要素取得
    this.scoreDisplay = document.getElementById('score-display')!
    this.chainDisplay = document.getElementById('chain-display')!
    this.statusDisplay = document.getElementById('status-display')!
    this.startButton = document.getElementById('start-button')!
    this.resetButton = document.getElementById('reset-button')!
    this.settingsButton = document.getElementById('settings-button')!
    this.performanceDisplay = document.getElementById('performance-display') || undefined
    
    // 統計表示要素取得
    this.statsGamesDisplay = document.getElementById('stats-games')!
    this.statsHighDisplay = document.getElementById('stats-high')!
    this.statsAvgDisplay = document.getElementById('stats-avg')!
    this.statsTimeDisplay = document.getElementById('stats-time')!

      this.setupEventListeners()
      this.setupErrorHandlers()
      this.setupGameEventListeners()
      this.setupSettingsIntegration()
      this.updateUI()
    } catch (error) {
      handleError(error instanceof Error ? error : new Error(String(error)), 'Initialization')
      throw error
    }
  }

  private setupEventListeners(): void {
    this.startButton.addEventListener('click', () => {
      this.startGame()
    })

    this.resetButton.addEventListener('click', () => {
      this.resetGame()
    })

    this.settingsButton.addEventListener('click', () => {
      this.settingsUI.open()
    })
  }

  startGame(): void {
    if (this.isRunning) return

    try {
      this.game.initialize()
      this.isRunning = true
      
      // 統計セッション開始
      statisticsManager.startSession(1) // レベル1で開始（将来的には設定可能）
      
      // BGM開始
      soundManager.startBGM()
      
      this.gameLoop = requestAnimationFrame(() => this.tick())
    } catch (error) {
      handleGameStateError(`ゲーム開始エラー: ${error}`, 'GameStart')
      this.handleCriticalError(error)
    }
  }

  resetGame(): void {
    try {
      this.stopGame()
      this.game.reset()
      this.performanceMonitor.reset()
      this.drawOptimizer = new DrawOptimizer() // 描画状態もリセット
      this.renderer.clearAnimations() // アニメーションもクリア
      this.updateUI()
      this.renderer.render(this.game)
    } catch (error) {
      handleGameStateError(`ゲームリセットエラー: ${error}`, 'GameReset')
    }
  }

  private stopGame(): void {
    this.isRunning = false
    if (this.gameLoop) {
      cancelAnimationFrame(this.gameLoop)
      this.gameLoop = null
    }
    
    // 統計セッション終了
    if (statisticsManager.getCurrentSession()) {
      const finalScore = this.game.getScore()
      const isCompleted = this.game.getMode() !== 'gameOver'
      statisticsManager.endSession(finalScore, isCompleted)
    }
    
    // BGM停止
    soundManager.stopBGM()
  }

  private tick(): void {
    if (!this.isRunning) return

    try {
      // パフォーマンス記録開始
      this.performanceMonitor.recordFrame()

      // ゲーム更新
      this.game.update()
      
      // セッション統計の更新
      this.updateSessionStatistics()

      // 描画最適化判定
      const gameState = this.getGameStateHash()
      if (this.drawOptimizer.shouldRedraw(gameState) || this.drawOptimizer.hasDirtyRegions()) {
        try {
          this.renderer.render(this.game)
          this.drawOptimizer.clearDirtyRegions()
        } catch (renderError) {
          handleRenderingError(`描画エラー: ${renderError}`, 'GameLoop')
          // 描画エラーが発生した場合はゲームを停止
          this.stopGame()
          return
        }
      }

      this.updateUI()

      // ゲームオーバー判定
      if (this.game.getMode() === 'gameOver') {
        this.handleGameOver()
        return
      }

      // パフォーマンス情報を定期的にログ出力
      if (this.performanceMonitor.frameCount % 300 === 0) { // 5秒おき（60FPS基準）
        this.performanceMonitor.logPerformance()
      }

      // 次フレーム
      this.gameLoop = requestAnimationFrame(() => this.tick())
    } catch (error) {
      handleGameStateError(`ゲームループエラー: ${error}`, 'GameLoop')
      this.stopGame()
      this.handleCriticalError(error)
    }
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

    // パフォーマンス表示更新（オプション）
    if (this.performanceDisplay) {
      const fps = this.performanceMonitor.getAverageFPS()
      const status = this.performanceMonitor.isPerformanceGood() ? '✅' : '⚠️'
      this.performanceDisplay.textContent = `${fps.toFixed(1)} FPS ${status}`
    }
    
    // 統計表示更新
    this.updateStatisticsDisplay()
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
    
    // サウンド停止・リソース解放
    soundManager.destroy()
    
    // 最終パフォーマンスログ
    console.log('📊 Final Performance Report:')
    this.performanceMonitor.logPerformance()
  }

  private getGameStateHash(): string {
    // ゲーム状態をハッシュ化して描画の必要性を判定
    const state = {
      mode: this.game.getMode(),
      score: this.game.getScore(),
      chain: this.game.getCombinationCount()
    }
    return JSON.stringify(state)
  }

  private setupErrorHandlers(): void {
    // 重大エラー時の処理
    globalErrorHandler.onError(ErrorLevel.Critical, (error) => {
      this.stopGame()
      this.showErrorMessage(`重大なエラーが発生しました: ${error.message}`)
    })

    // エラー時の処理
    globalErrorHandler.onError(ErrorLevel.Error, (error) => {
      if (!error.recoverable) {
        this.stopGame()
        this.showErrorMessage(`エラーが発生しました: ${error.message}`)
      }
    })

    // 復旧アクションの登録
    globalErrorHandler.registerRecoveryAction({
      id: 'restart_game',
      description: 'ゲームを再開する',
      execute: () => {
        this.resetGame()
        this.startGame()
      }
    })

    globalErrorHandler.registerRecoveryAction({
      id: 'fallback_renderer',
      description: 'フォールバックレンダラーに切り替え',
      execute: () => {
        // 簡易レンダリングモードに切り替え
        console.log('フォールバックレンダラーに切り替えました')
      }
    })
  }

  private handleCriticalError(error: unknown): void {
    this.stopGame()
    
    const errorMessage = error instanceof Error ? error.message : String(error)
    const fullMessage = `重大なエラーが発生しました。\n\n${errorMessage}\n\nゲームを再開しますか？`
    
    setTimeout(() => {
      if (confirm(fullMessage)) {
        this.resetGame()
        this.startGame()
      }
    }, 100)
  }

  private showErrorMessage(message: string): void {
    // エラーメッセージをユーザーに表示
    const errorDiv = document.getElementById('error-message')
    if (errorDiv) {
      errorDiv.textContent = message
      errorDiv.style.display = 'block'
      
      // 5秒後に非表示
      setTimeout(() => {
        errorDiv.style.display = 'none'
      }, 5000)
    } else {
      // fallback
      console.error(message)
      alert(message)
    }
  }

  private async initializeAudio(): Promise<void> {
    try {
      await soundManager.initialize()
    } catch (error) {
      console.warn('サウンドシステムの初期化に失敗しました:', error)
    }
  }

  private setupGameEventListeners(): void {
    // ゲームイベントに応じたサウンド再生
    // 注意: この実装は簡略化されています。実際にはゲームクラスからイベントを受け取る設計が必要
    
    // ユーザージェスチャー後のAudioContext再開
    document.addEventListener('click', async () => {
      await soundManager.resumeAudioContext()
    }, { once: true })

    document.addEventListener('keydown', async () => {
      await soundManager.resumeAudioContext()
    }, { once: true })
  }

  // ゲームイベントハンドラー（将来の拡張用）
  onPuyoMove(): void {
    soundManager.playSound(SoundType.Move)
  }

  onPuyoRotate(): void {
    soundManager.playSound(SoundType.Rotate)
  }

  onPuyoLand(): void {
    soundManager.playSound(SoundType.Land)
  }

  onPuyoErase(count: number): void {
    soundManager.playSound(SoundType.Erase, { pitch: 1.0 + (count - 4) * 0.1 })
  }

  onChain(chainCount: number): void {
    soundManager.playSound(SoundType.Chain, { chain: chainCount })
  }

  onGameOver(): void {
    soundManager.playSound(SoundType.GameOver)
  }

  private setupSettingsIntegration(): void {
    // 設定変更の監視と適用
    settingsManager.onChange('masterVolume', (volume) => {
      soundManager.setMasterVolume(volume)
    })

    settingsManager.onChange('soundEnabled', (enabled) => {
      soundManager.setSoundEnabled(enabled)
    })

    settingsManager.onChange('bgmEnabled', (enabled) => {
      soundManager.setBGMEnabled(enabled)
      if (enabled && this.isRunning) {
        soundManager.startBGM()
      } else {
        soundManager.stopBGM()
      }
    })

    settingsManager.onChange('showFPS', (show) => {
      if (this.performanceDisplay) {
        this.performanceDisplay.style.display = show ? 'block' : 'none'
      }
    })

    settingsManager.onChange('pauseOnFocusLoss', (pauseOnFocus) => {
      if (pauseOnFocus) {
        window.addEventListener('blur', this.handleFocusLoss)
        window.addEventListener('focus', this.handleFocusGain)
      } else {
        window.removeEventListener('blur', this.handleFocusLoss)
        window.removeEventListener('focus', this.handleFocusGain)
      }
    })

    // 初期設定を適用
    const currentSettings = settingsManager.getSettings()
    soundManager.setMasterVolume(currentSettings.masterVolume)
    soundManager.setSoundEnabled(currentSettings.soundEnabled)
    soundManager.setBGMEnabled(currentSettings.bgmEnabled)
    
    if (this.performanceDisplay) {
      this.performanceDisplay.style.display = currentSettings.showFPS ? 'block' : 'none'
    }
  }

  private handleFocusLoss = () => {
    if (this.isRunning && settingsManager.get('pauseOnFocusLoss')) {
      this.stopGame()
    }
  }

  private handleFocusGain = () => {
    // フォーカス復帰時の処理（必要に応じて実装）
  }

  private updateSessionStatistics(): void {
    // セッション中の統計を定期的に更新
    if (statisticsManager.getCurrentSession()) {
      const currentScore = this.game.getScore()
      const combinationCount = this.game.getCombinationCount()
      
      statisticsManager.updateSessionStats({
        score: currentScore,
        chainsCount: combinationCount,
        longestChain: combinationCount // 簡略化（実際にはより詳細な追跡が必要）
      })
    }
  }

  private updateStatisticsDisplay(): void {
    const stats = statisticsManager.getStatistics()
    const summary = statisticsManager.getStatisticsSummary()
    
    this.statsGamesDisplay.textContent = stats.totalGamesPlayed.toString()
    this.statsHighDisplay.textContent = stats.highScore.toString()
    this.statsAvgDisplay.textContent = Math.round(stats.averageScore).toString()
    this.statsTimeDisplay.textContent = summary.totalPlayTime
  }

  private formatDuration(seconds: number): string {
    if (seconds < 60) {
      return `${seconds}秒`
    } else if (seconds < 3600) {
      const minutes = Math.floor(seconds / 60)
      return `${minutes}分`
    } else {
      const hours = Math.floor(seconds / 3600)
      const remainingMinutes = Math.floor((seconds % 3600) / 60)
      return remainingMinutes > 0 ? `${hours}時間${remainingMinutes}分` : `${hours}時間`
    }
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
