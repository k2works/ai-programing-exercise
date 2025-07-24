export class Game {
  private static readonly FIELD_WIDTH = 6
  private static readonly FIELD_HEIGHT = 13
  private static readonly CELL_SIZE = 30
  private static readonly FIELD_OFFSET_X = 10
  private static readonly FIELD_OFFSET_Y = 10
  private static readonly PUYO_COLORS = 4 // 1-4の色を使用
  private static readonly PUYO_COLOR_MAP = [
    '', // 0は使用しない
    '#FF0000', // 1: 赤
    '#00FF00', // 2: 緑
    '#0000FF', // 3: 青
    '#FFFF00', // 4: 黄
  ]

  private canvas: HTMLCanvasElement
  private context: CanvasRenderingContext2D
  private scoreDisplay: HTMLElement
  private isRunning = false
  private score = 0
  private gameState: 'ready' | 'playing' | 'gameover' = 'ready'
  private field: number[][]
  private nextPuyo: { color1: number; color2: number }
  private activePuyo: { x: number; y: number; color1: number; color2: number } | null = null

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
    this.nextPuyo = this.generateNewPuyoPair()
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

    // 最初の操作ぷよを生成
    this.spawnActivePuyo()
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
    this.renderActivePuyo()
    this.renderNextPuyo()
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

  generateNewPuyoPair(): { color1: number; color2: number } {
    return {
      color1: Math.floor(Math.random() * Game.PUYO_COLORS) + 1,
      color2: Math.floor(Math.random() * Game.PUYO_COLORS) + 1,
    }
  }

  spawnActivePuyo(): void {
    // 現在の次のぷよを操作ぷよとして配置
    this.activePuyo = {
      x: 2, // フィールド中央
      y: 0, // 上端
      color1: this.nextPuyo.color1,
      color2: this.nextPuyo.color2,
    }

    // 新しい次のぷよを生成
    this.nextPuyo = this.generateNewPuyoPair()
  }

  getActivePuyo(): { x: number; y: number; color1: number; color2: number } | null {
    return this.activePuyo
  }

  private getPuyoColor(colorNumber: number): string {
    return Game.PUYO_COLOR_MAP[colorNumber] || '#CCCCCC'
  }

  renderActivePuyo(): void {
    if (!this.activePuyo) return

    const x1 = Game.FIELD_OFFSET_X + this.activePuyo.x * Game.CELL_SIZE
    const y1 = Game.FIELD_OFFSET_Y + this.activePuyo.y * Game.CELL_SIZE
    const x2 = Game.FIELD_OFFSET_X + this.activePuyo.x * Game.CELL_SIZE
    const y2 = Game.FIELD_OFFSET_Y + (this.activePuyo.y + 1) * Game.CELL_SIZE

    // 1つ目のぷよを描画
    this.context.fillStyle = this.getPuyoColor(this.activePuyo.color1)
    this.context.fillRect(x1, y1, Game.CELL_SIZE - 2, Game.CELL_SIZE - 2)

    // 2つ目のぷよを描画（下に配置）
    this.context.fillStyle = this.getPuyoColor(this.activePuyo.color2)
    this.context.fillRect(x2, y2, Game.CELL_SIZE - 2, Game.CELL_SIZE - 2)
  }

  renderNextPuyo(): void {
    const nextX = Game.FIELD_OFFSET_X + (Game.FIELD_WIDTH + 1) * Game.CELL_SIZE
    const nextY = Game.FIELD_OFFSET_Y + Game.CELL_SIZE

    // 次のぷよ1を描画
    this.context.fillStyle = this.getPuyoColor(this.nextPuyo.color1)
    this.context.fillRect(nextX, nextY, Game.CELL_SIZE - 2, Game.CELL_SIZE - 2)

    // 次のぷよ2を描画（下に配置）
    this.context.fillStyle = this.getPuyoColor(this.nextPuyo.color2)
    this.context.fillRect(nextX, nextY + Game.CELL_SIZE, Game.CELL_SIZE - 2, Game.CELL_SIZE - 2)
  }
}
