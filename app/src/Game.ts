import { Stage } from './Stage'
import { PuyoPair } from './Puyo'
import { Config } from './Config'

// ゲームの状態を管理するメインクラス
export class Game {
  private stage: Stage
  private currentPuyo: PuyoPair | null = null
  private running = false
  private canvas: HTMLCanvasElement
  private ctx: CanvasRenderingContext2D

  constructor(canvas: HTMLCanvasElement) {
    this.canvas = canvas
    const context = canvas.getContext('2d')
    if (!context) {
      throw new Error('Canvas context not available')
    }
    this.ctx = context
    this.stage = new Stage()
  }

  start(): void {
    this.running = true
    this.stage = new Stage()
    this.currentPuyo = this.generateNewPuyo()
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