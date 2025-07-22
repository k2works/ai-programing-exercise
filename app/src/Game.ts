import { Stage } from './Stage'
import { PuyoPair } from './Puyo'
import { Config } from './Config'
import { Player } from './Player'

// ゲームの状態を管理するメインクラス
export class Game {
  private stage: Stage
  private currentPuyo: PuyoPair | null = null
  private running = false
  private canvas: HTMLCanvasElement
  private ctx: CanvasRenderingContext2D
  private player: Player
  private frameCount = 0

  constructor(canvas: HTMLCanvasElement) {
    this.canvas = canvas
    const context = canvas.getContext('2d')
    if (!context) {
      throw new Error('Canvas context not available')
    }
    this.ctx = context
    this.stage = new Stage()
    this.player = new Player(this.stage)
  }

  start(): void {
    this.running = true
    this.stage = new Stage()
    this.player = new Player(this.stage)
    this.currentPuyo = this.generateNewPuyo()
    this.frameCount = 0
    this.render()
  }

  isRunning(): boolean {
    return this.running
  }

  getStage(): Stage {
    return this.stage
  }

  getCurrentPuyo(): PuyoPair | null {
    return this.currentPuyo
  }

  // ゲームの状態を更新（フレーム毎に呼ばれる）
  update(): void {
    if (!this.running || !this.currentPuyo) {
      return
    }

    this.frameCount++

    // 一定フレーム毎に自動落下（テスト時は即座に落下）
    const dropInterval = Config.GAME_SPEED
    if (this.frameCount % dropInterval === 0 || dropInterval >= 60) {
      const droppedPuyo = this.player.dropPuyoDown(this.currentPuyo)
      
      // 落下できた場合
      if (droppedPuyo.main.y > this.currentPuyo.main.y) {
        this.currentPuyo = droppedPuyo
      } else {
        // 着地した場合（今は何もしない、後で固定処理を実装）
        // this.fixCurrentPuyo()
        // this.currentPuyo = this.generateNewPuyo()
      }
    }

    this.render()
  }

  // キーボード入力を処理
  handleInput(key: string): void {
    if (!this.running || !this.currentPuyo) {
      return
    }

    switch (key) {
      case 'ArrowLeft':
      case 'KeyA':
        this.currentPuyo = this.player.movePuyoLeft(this.currentPuyo)
        break
      case 'ArrowRight':
      case 'KeyD':
        this.currentPuyo = this.player.movePuyoRight(this.currentPuyo)
        break
      case 'ArrowDown':
      case 'KeyS':
        this.currentPuyo = this.player.dropPuyoDown(this.currentPuyo)
        break
    }

    this.render()
  }

  private generateNewPuyo(): PuyoPair {
    const startX = Math.floor(Config.STAGE_WIDTH / 2)
    const startY = 1
    const mainColor = Math.floor(Math.random() * (Config.COLORS.length - 1)) + 1
    const subColor = Math.floor(Math.random() * (Config.COLORS.length - 1)) + 1
    return new PuyoPair(startX, startY, mainColor, subColor)
  }

  private render(): void {
    // 画面をクリア
    this.ctx.clearRect(0, 0, this.canvas.width, this.canvas.height)

    // ステージを描画
    this.renderStage()

    // 現在のぷよを描画
    if (this.currentPuyo) {
      this.renderPuyo(this.currentPuyo.main)
      this.renderPuyo(this.currentPuyo.sub)
    }
  }

  private renderStage(): void {
    // ステージの枠線を描画
    this.ctx.strokeStyle = '#ffffff'
    this.ctx.strokeRect(
      0,
      0,
      Config.STAGE_WIDTH * Config.PUYO_SIZE,
      Config.STAGE_HEIGHT * Config.PUYO_SIZE
    )
  }

  private renderPuyo(puyo: { x: number; y: number; color: number }): void {
    const pixelX = puyo.x * Config.PUYO_SIZE
    const pixelY = puyo.y * Config.PUYO_SIZE

    this.ctx.fillStyle = Config.COLORS[puyo.color]
    this.ctx.fillRect(
      pixelX,
      pixelY,
      Config.PUYO_SIZE,
      Config.PUYO_SIZE
    )

    // 枠線を描画
    this.ctx.strokeStyle = '#ffffff'
    this.ctx.strokeRect(
      pixelX,
      pixelY,
      Config.PUYO_SIZE,
      Config.PUYO_SIZE
    )
  }
}