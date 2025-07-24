export class Game {
  private canvas: HTMLCanvasElement
  private context: CanvasRenderingContext2D
  private scoreDisplay: HTMLElement
  private isRunning = false
  private score = 0

  constructor(canvas: HTMLCanvasElement, scoreDisplay: HTMLElement) {
    this.canvas = canvas
    this.scoreDisplay = scoreDisplay

    const context = canvas.getContext('2d')
    if (!context) {
      throw new Error('Could not get 2D context from canvas')
    }
    this.context = context
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
    // 描画処理（後で実装）
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
}
