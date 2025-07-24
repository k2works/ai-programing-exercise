export class Game {
  private static readonly FIELD_WIDTH = 6
  private static readonly FIELD_HEIGHT = 13
  private static readonly CELL_SIZE = 30
  private static readonly FIELD_OFFSET_X = 10
  private static readonly FIELD_OFFSET_Y = 10

  private canvas: HTMLCanvasElement
  private context: CanvasRenderingContext2D
  private scoreDisplay: HTMLElement
  private isRunning = false
  private score = 0
  private gameState: 'ready' | 'playing' | 'gameover' = 'ready'
  private field: number[][]
  private nextPuyo: { color1: number; color2: number }

  constructor(canvas: HTMLCanvasElement, scoreDisplay: HTMLElement) {
    this.canvas = canvas
    this.scoreDisplay = scoreDisplay

    const context = canvas.getContext('2d')
    if (!context) {
      throw new Error('Could not get 2D context from canvas')
    }
    this.context = context

    // ゲームフィールドの初期化
    this.field = Array(Game.FIELD_HEIGHT)
      .fill(null)
      .map(() => Array(Game.FIELD_WIDTH).fill(0))

    // 次のぷよの初期化
    this.nextPuyo = { color1: 1, color2: 2 }
  }

  start(): void {
    if (this.isRunning) return

    this.isRunning = true
    this.initialize()
    this.gameLoop()
  }

  stop(): void {
    this.isRunning = false
  }

  private initialize(): void {
    this.score = 0
    this.updateScoreDisplay()
    this.clearCanvas()
  }

  private gameLoop(): void {
    if (!this.isRunning) return

    this.update()
    this.render()

    requestAnimationFrame(() => this.gameLoop())
  }

  private update(): void {
    // ゲームロジックの更新処理（後で実装）
  }

  private render(): void {
    this.clearCanvas()
    this.renderField()
  }

  private clearCanvas(): void {
    this.context.fillStyle = '#000000'
    this.context.fillRect(0, 0, this.canvas.width, this.canvas.height)
  }

  private updateScoreDisplay(): void {
    this.scoreDisplay.textContent = `スコア: ${this.score}`
  }

  // テスト用のpublicメソッド
  getScore(): number {
    return this.score
  }

  isGameRunning(): boolean {
    return this.isRunning
  }

  getGameState(): string {
    return this.gameState
  }

  getField(): number[][] {
    return this.field
  }

  getNextPuyo(): { color1: number; color2: number } {
    return this.nextPuyo
  }

  renderField(): void {
    const fieldWidth = Game.FIELD_WIDTH * Game.CELL_SIZE
    const fieldHeight = Game.FIELD_HEIGHT * Game.CELL_SIZE

    // フィールドの背景を描画
    this.context.fillStyle = '#f0f0f0'
    this.context.fillRect(Game.FIELD_OFFSET_X, Game.FIELD_OFFSET_Y, fieldWidth, fieldHeight)

    // フィールドの枠線を描画
    this.context.strokeStyle = '#000000'
    this.context.strokeRect(Game.FIELD_OFFSET_X, Game.FIELD_OFFSET_Y, fieldWidth, fieldHeight)
  }

  clearScreen(): void {
    this.context.clearRect(0, 0, this.canvas.width, this.canvas.height)
  }
}
