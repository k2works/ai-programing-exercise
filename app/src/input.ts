import { Game } from './game'

export class InputHandler {
  private keyPressed: Set<string> = new Set()
  private keyRepeatTimers: Map<string, number> = new Map()
  private readonly REPEAT_DELAY = 150
  private readonly REPEAT_INTERVAL = 50

  constructor(private game: Game) {
    this.setupEventListeners()
  }

  private setupEventListeners(): void {
    document.addEventListener('keydown', (event) => this.handleKeyDown(event))
    document.addEventListener('keyup', (event) => this.handleKeyUp(event))
  }

  private handleKeyDown(event: KeyboardEvent): void {
    const key = event.key

    if (!this.isGameActive()) return

    if (this.keyPressed.has(key)) return // 重複防止

    this.keyPressed.add(key)
    this.executeCommand(key) // 即座に実行

    // キーリピート設定
    if (this.isRepeatableKey(key)) {
      const timer = window.setTimeout(() => {
        this.startKeyRepeat(key)
      }, this.REPEAT_DELAY)

      this.keyRepeatTimers.set(key, timer)
    }

    // ブラウザのデフォルト動作を防止
    event.preventDefault()
  }

  private handleKeyUp(event: KeyboardEvent): void {
    const key = event.key

    this.keyPressed.delete(key)

    // キーリピートタイマーをクリア
    const timer = this.keyRepeatTimers.get(key)
    if (timer) {
      clearTimeout(timer)
      this.keyRepeatTimers.delete(key)
    }

    event.preventDefault()
  }

  private startKeyRepeat(key: string): void {
    const repeatTimer = window.setInterval(() => {
      if (!this.keyPressed.has(key)) {
        clearInterval(repeatTimer)
        return
      }

      if (!this.isGameActive()) {
        clearInterval(repeatTimer)
        return
      }

      this.executeCommand(key)
    }, this.REPEAT_INTERVAL)
  }

  private executeCommand(key: string): void {
    if (this.game.getMode() !== 'playing') return

    const player = this.game.getPlayerForRenderer()

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
        this.forcePlayerUpdate()
        break
      case ' ':
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
  }

  private forcePlayerUpdate(): void {
    const player = this.game.getPlayerForRenderer()
    player.update()
  }

  private dropPuyo(): void {
    const player = this.game.getPlayerForRenderer()

    // 一気落下処理
    let moved = true
    while (moved && !player.isPlaced()) {
      const currentY = player.getCurrentPair().getY()
      player.update()
      moved = player.getCurrentPair().getY() > currentY
    }
  }

  private isGameActive(): boolean {
    const mode = this.game.getMode()
    return mode === 'playing'
  }

  private isRepeatableKey(key: string): boolean {
    return ['ArrowLeft', 'ArrowRight', 'ArrowDown'].includes(key)
  }

  cleanup(): void {
    // イベントリスナーの削除
    document.removeEventListener('keydown', this.handleKeyDown)
    document.removeEventListener('keyup', this.handleKeyUp)

    // 全てのタイマーをクリア
    this.keyRepeatTimers.forEach((timer) => clearTimeout(timer))
    this.keyRepeatTimers.clear()
  }
}
