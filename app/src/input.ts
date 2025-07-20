import { Game } from './game'

export class InputHandler {
  private game: Game
  private keyPressed: Set<string> = new Set()
  private keyRepeatTimers: Map<string, number> = new Map()
  private readonly REPEAT_DELAY = 150 // キーリピートの遅延（ms）

  constructor(game: Game) {
    this.game = game
    this.setupEventListeners()
  }

  private setupEventListeners(): void {
    document.addEventListener('keydown', (event) => {
      this.handleKeyDown(event)
    })

    document.addEventListener('keyup', (event) => {
      this.handleKeyUp(event)
    })

    // フォーカスが外れた時にキー状態をリセット
    window.addEventListener('blur', () => {
      this.keyPressed.clear()
      this.clearAllTimers()
    })
  }

  private handleKeyDown(event: KeyboardEvent): void {
    const key = event.key

    // ゲームが動作中でない場合は操作を無視
    if (!this.isGameActive()) {
      return
    }

    // 既に押されているキーの場合はリピート処理
    if (this.keyPressed.has(key)) {
      return
    }

    this.keyPressed.add(key)

    // 即座に1回実行
    this.executeCommand(key)

    // 連続入力のためのタイマーを設定
    if (this.isRepeatableKey(key)) {
      const timer = window.setTimeout(() => {
        this.startKeyRepeat(key)
      }, this.REPEAT_DELAY)
      
      this.keyRepeatTimers.set(key, timer)
    }

    // ブラウザのデフォルト動作を防ぐ
    if (this.isGameKey(key)) {
      event.preventDefault()
    }
  }

  private handleKeyUp(event: KeyboardEvent): void {
    const key = event.key
    
    this.keyPressed.delete(key)
    
    // タイマーをクリア
    const timer = this.keyRepeatTimers.get(key)
    if (timer) {
      clearTimeout(timer)
      this.keyRepeatTimers.delete(key)
    }
  }

  private executeCommand(key: string): void {
    try {
      const player = this.game['player']
      
      switch (key) {
        case 'ArrowLeft':
          player.moveLeft()
          break
        case 'ArrowRight':
          player.moveRight()
          break
        case 'ArrowUp':
          player.rotateRight()
          break
        case 'ArrowDown':
          // 高速落下（連続入力可能）
          this.forcePlayerUpdate()
          break
        case ' ': // スペースキー：即座に落下
          this.dropPuyo()
          break
        case 'z':
        case 'Z':
          player.rotateLeft()
          break
        case 'x':
        case 'X':
          player.rotateRight()
          break
      }
    } catch (error) {
      console.warn('Player operation failed:', error)
    }
  }

  private startKeyRepeat(key: string): void {
    if (!this.keyPressed.has(key)) {
      return
    }

    this.executeCommand(key)

    // 次のリピートをスケジュール
    const timer = window.setTimeout(() => {
      this.startKeyRepeat(key)
    }, 50) // リピート間隔（ms）

    this.keyRepeatTimers.set(key, timer)
  }

  private forcePlayerUpdate(): void {
    // プレイヤーの更新を強制実行（高速落下）
    try {
      this.game['player'].update()
    } catch (error) {
      console.warn('Force player update failed:', error)
    }
  }

  private dropPuyo(): void {
    // ぷよを一気に落下させる
    try {
      const player = this.game['player']
      
      // 落下可能な限り下に移動
      let moved = true
      while (moved && !player.isPlaced()) {
        const currentY = player.getCurrentPair().getY()
        player.update() // 自動落下を1フレーム実行
        moved = player.getCurrentPair().getY() > currentY
      }
    } catch (error) {
      console.warn('Drop puyo failed:', error)
    }
  }

  private isGameActive(): boolean {
    const mode = this.game['mode']
    return mode === 'playing'
  }

  private isRepeatableKey(key: string): boolean {
    return ['ArrowLeft', 'ArrowRight', 'ArrowDown'].includes(key)
  }

  private isGameKey(key: string): boolean {
    return [
      'ArrowLeft', 'ArrowRight', 'ArrowUp', 'ArrowDown',
      ' ', 'z', 'Z', 'x', 'X'
    ].includes(key)
  }

  private clearAllTimers(): void {
    this.keyRepeatTimers.forEach(timer => clearTimeout(timer))
    this.keyRepeatTimers.clear()
  }

  public destroy(): void {
    this.clearAllTimers()
    this.keyPressed.clear()
  }
}