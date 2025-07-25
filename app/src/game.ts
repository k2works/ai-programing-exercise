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
  private static readonly FALL_SPEED = 1 // 落下速度
  private static readonly FALL_INTERVAL = 30 // 落下間隔（フレーム数）

  private canvas: HTMLCanvasElement
  private context: CanvasRenderingContext2D
  private scoreDisplay: HTMLElement
  private isRunning = false
  private score = 0
  private gameState: 'ready' | 'playing' | 'gameover' = 'ready'
  private field: number[][]
  private nextPuyo: { color1: number; color2: number }
  private activePuyo: { x: number; y: number; color1: number; color2: number } | null = null
  private fallSpeed = Game.FALL_SPEED
  private fallTimer = 0 // 落下タイマー
  private fallInterval = Game.FALL_INTERVAL
  // 入力状態の管理
  private leftKeyPressed = false
  private rightKeyPressed = false
  private moveTimer = 0 // 移動タイマー
  private static readonly MOVE_INTERVAL = 8 // 移動間隔（フレーム数）

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
    this.setupInputHandlers()
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

    this.updateAndRender()

    requestAnimationFrame(() => this.gameLoop())
  }

  private update(): void {
    this.updateFalling()
    this.updateMovement()
  }

  // テスト用のpublicメソッド - 1フレーム分の更新と描画を実行
  updateAndRender(): void {
    this.update()
    this.render()
  }

  // renderメソッドをpublicにして外部からテスト可能にする
  render(): void {
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

    // フィールドに固定されたぷよを楕円形で描画
    for (let y = 0; y < Game.FIELD_HEIGHT; y++) {
      for (let x = 0; x < Game.FIELD_WIDTH; x++) {
        const puyoColor = this.field[y][x]
        if (puyoColor !== 0) {
          // ぷよが存在する場合は楕円形で描画
          const drawX = Game.FIELD_OFFSET_X + x * Game.CELL_SIZE
          const drawY = Game.FIELD_OFFSET_Y + y * Game.CELL_SIZE
          this.drawPuyo(drawX, drawY, this.getPuyoColor(puyoColor))
        }
      }
    }
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

  private drawPuyo(x: number, y: number, color: string): void {
    // ぷよを楕円形で描画
    const centerX = x + Game.CELL_SIZE / 2
    const centerY = y + Game.CELL_SIZE / 2
    const radiusX = (Game.CELL_SIZE - 4) / 2 // 若干小さくして見やすく
    const radiusY = (Game.CELL_SIZE - 4) / 2

    this.context.fillStyle = color
    this.context.beginPath()
    this.context.ellipse(centerX, centerY, radiusX, radiusY, 0, 0, 2 * Math.PI)
    this.context.fill()

    // 輪郭線を追加
    this.context.strokeStyle = '#333333'
    this.context.lineWidth = 1
    this.context.stroke()
  }

  renderActivePuyo(): void {
    if (!this.activePuyo) return

    const x1 = Game.FIELD_OFFSET_X + this.activePuyo.x * Game.CELL_SIZE
    const y1 = Game.FIELD_OFFSET_Y + this.activePuyo.y * Game.CELL_SIZE
    const x2 = Game.FIELD_OFFSET_X + this.activePuyo.x * Game.CELL_SIZE
    const y2 = Game.FIELD_OFFSET_Y + (this.activePuyo.y + 1) * Game.CELL_SIZE

    // 1つ目のぷよを楕円形で描画
    this.drawPuyo(x1, y1, this.getPuyoColor(this.activePuyo.color1))

    // 2つ目のぷよを楕円形で描画（下に配置）
    this.drawPuyo(x2, y2, this.getPuyoColor(this.activePuyo.color2))
  }

  renderNextPuyo(): void {
    const nextX = Game.FIELD_OFFSET_X + (Game.FIELD_WIDTH + 1) * Game.CELL_SIZE
    const nextY = Game.FIELD_OFFSET_Y + Game.CELL_SIZE

    // 次のぷよ1を楕円形で描画
    this.drawPuyo(nextX, nextY, this.getPuyoColor(this.nextPuyo.color1))

    // 次のぷよ2を楕円形で描画（下に配置）
    this.drawPuyo(nextX, nextY + Game.CELL_SIZE, this.getPuyoColor(this.nextPuyo.color2))
  }

  updateFalling(): void {
    if (!this.activePuyo) return

    this.fallTimer++
    if (this.fallTimer >= this.fallInterval) {
      this.fallTimer = 0

      // 落下可能かチェック
      if (this.canFall()) {
        this.activePuyo.y += this.fallSpeed
      } else {
        // 着地処理を実行
        this.processLanding()
      }
    }
  }

  private canFall(): boolean {
    if (!this.activePuyo) return false

    // フィールドの底部に到達した場合（ぷよは2つ分の高さなので、+2で判定）
    if (this.activePuyo.y + 2 >= Game.FIELD_HEIGHT) {
      return false
    }

    // canMoveDown()と同じ衝突判定を使用
    return this.canMoveDown()
  }

  getFallSpeed(): number {
    return this.fallSpeed
  }

  // プレイヤー入力の検出
  handleKeyDown(event: KeyboardEvent): void {
    switch (event.key) {
      case 'ArrowLeft':
        this.leftKeyPressed = true
        break
      case 'ArrowRight':
        this.rightKeyPressed = true
        break
    }
  }

  handleKeyUp(event: KeyboardEvent): void {
    switch (event.key) {
      case 'ArrowLeft':
        this.leftKeyPressed = false
        break
      case 'ArrowRight':
        this.rightKeyPressed = false
        break
    }
  }

  isLeftKeyPressed(): boolean {
    return this.leftKeyPressed
  }

  isRightKeyPressed(): boolean {
    return this.rightKeyPressed
  }

  setupInputHandlers(): void {
    addEventListener('keydown', this.handleKeyDown.bind(this))
    addEventListener('keyup', this.handleKeyUp.bind(this))
  }

  resetInputState(): void {
    this.leftKeyPressed = false
    this.rightKeyPressed = false
  }

  updateMovement(): void {
    if (!this.activePuyo) return

    // 左右のキーが同時に押されている場合は移動しない
    if (this.leftKeyPressed && this.rightKeyPressed) {
      return
    }

    // キーが押されている場合のみタイマーを進める
    if (this.leftKeyPressed || this.rightKeyPressed) {
      this.moveTimer++

      if (this.moveTimer >= Game.MOVE_INTERVAL) {
        this.moveTimer = 0

        if (this.leftKeyPressed && this.canMoveLeft()) {
          this.activePuyo.x -= 1
        } else if (this.rightKeyPressed && this.canMoveRight()) {
          this.activePuyo.x += 1
        }
      }
    } else {
      // キーが押されていないときはタイマーをリセット
      this.moveTimer = 0
    }
  }

  clearActivePuyo(): void {
    this.activePuyo = null
  }

  canMoveLeft(): boolean {
    return this.canMoveToPosition(-1)
  }

  canMoveRight(): boolean {
    return this.canMoveToPosition(1)
  }

  canMoveDown(): boolean {
    if (!this.activePuyo) return false

    const targetX = this.activePuyo.x
    const targetY = this.activePuyo.y + 1

    // フィールドの境界チェック（底面判定）
    if (targetY + 1 >= Game.FIELD_HEIGHT) {
      return false
    }

    // 他のぷよとの衝突チェック
    return this.isPuyoPositionEmpty(targetX, targetY)
  }

  private canMoveToPosition(deltaX: number): boolean {
    if (!this.activePuyo) return false

    const targetX = this.activePuyo.x + deltaX
    const targetY = this.activePuyo.y

    // フィールドの境界チェック
    if (targetX < 0 || targetX >= Game.FIELD_WIDTH) {
      return false
    }

    // 他のぷよとの衝突チェック
    return this.isPuyoPositionEmpty(targetX, targetY)
  }

  private isPuyoPositionEmpty(x: number, y: number): boolean {
    // 操作ぷよの1つ目との衝突チェック
    if (this.field[y] && this.field[y][x] !== 0) {
      return false
    }

    // 操作ぷよの2つ目との衝突チェック
    if (this.field[y + 1] && this.field[y + 1][x] !== 0) {
      return false
    }

    return true
  }

  landActivePuyo(): void {
    if (!this.activePuyo) return

    // 操作ぷよをフィールドに固定
    this.field[this.activePuyo.y][this.activePuyo.x] = this.activePuyo.color1
    this.field[this.activePuyo.y + 1][this.activePuyo.x] = this.activePuyo.color2

    // 操作ぷよをクリア
    this.activePuyo = null
  }

  processLanding(): void {
    if (!this.activePuyo) return

    // 着地処理
    this.landActivePuyo()

    // 新しい操作ぷよを生成
    this.spawnActivePuyo()
  }
}
