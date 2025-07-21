import { Game } from './game'
import { Stage } from './stage'
import { Player } from './player'
import { PuyoColor } from './puyo'
import { Config } from './config'

export class GameRenderer {
  private cellSize: number = 32
  private colors = {
    [PuyoColor.Empty]: '#000000',
    [PuyoColor.Red]: '#ff4444',
    [PuyoColor.Blue]: '#4444ff',
    [PuyoColor.Green]: '#44ff44',
    [PuyoColor.Yellow]: '#ffff44',
  }

  constructor(
    private ctx: CanvasRenderingContext2D,
    private nextCtx: CanvasRenderingContext2D,
    private canvas: HTMLCanvasElement,
    private nextCanvas: HTMLCanvasElement
  ) {}

  render(game: Game): void {
    // メインキャンバスクリア
    this.ctx.fillStyle = '#000000'
    this.ctx.fillRect(0, 0, this.canvas.width, this.canvas.height)

    // 各レイヤー描画
    this.renderStage(game.getStageForRenderer())
    this.renderCurrentPair(game.getPlayerForRenderer())
    this.renderGrid(game.getConfigForRenderer())
    this.renderNextPuyo(game.getPlayerForRenderer())
  }

  private renderStage(stage: Stage): void {
    for (let x = 0; x < stage.getWidth(); x++) {
      for (let y = 0; y < stage.getHeight(); y++) {
        const puyo = stage.getPuyo(x, y)
        this.renderPuyo(x, y, puyo.getColor())
      }
    }
  }

  private renderCurrentPair(player: Player): void {
    try {
      const currentPair = player.getCurrentPair()
      const mainX = currentPair.getX()
      const mainY = currentPair.getY()
      const [subX, subY] = currentPair.getSubPosition()

      // 落下中ぷよはハイライト表示
      this.renderPuyo(mainX, mainY, currentPair.getMainColor(), true)
      this.renderPuyo(subX, subY, currentPair.getSubColor(), true)
    } catch {
      // currentPairがない場合は何も描画しない
    }
  }

  private renderNextPuyo(player: Player): void {
    // 次ぷよキャンバスクリア
    this.nextCtx.fillStyle = '#000000'
    this.nextCtx.fillRect(0, 0, this.nextCanvas.width, this.nextCanvas.height)

    try {
      const nextPair = player.getNextPair()
      const size = this.nextCanvas.width / 2

      // メインぷよ（上）
      this.nextCtx.fillStyle = this.colors[nextPair.getMainColor()]
      this.nextCtx.fillRect(size / 2, 0, size, size)

      // サブぷよ（下）
      this.nextCtx.fillStyle = this.colors[nextPair.getSubColor()]
      this.nextCtx.fillRect(size / 2, size, size, size)
    } catch {
      // nextPairがない場合は何も描画しない
    }
  }

  private renderGrid(config: Config): void {
    this.ctx.strokeStyle = '#333333'
    this.ctx.lineWidth = 1

    // 縦線
    for (let x = 0; x <= config.stageWidth; x++) {
      const pixelX = x * this.cellSize
      this.ctx.beginPath()
      this.ctx.moveTo(pixelX, 0)
      this.ctx.lineTo(pixelX, config.stageHeight * this.cellSize)
      this.ctx.stroke()
    }

    // 横線
    for (let y = 0; y <= config.stageHeight; y++) {
      const pixelY = y * this.cellSize
      this.ctx.beginPath()
      this.ctx.moveTo(0, pixelY)
      this.ctx.lineTo(config.stageWidth * this.cellSize, pixelY)
      this.ctx.stroke()
    }
  }

  private renderPuyo(
    x: number,
    y: number,
    color: PuyoColor,
    highlight: boolean = false
  ): void {
    if (color === PuyoColor.Empty) return

    const pixelX = x * this.cellSize
    const pixelY = y * this.cellSize

    let baseColor = this.colors[color]

    if (highlight) {
      baseColor = this.brightenColor(baseColor, 0.3)
    }

    // メイン描画
    this.ctx.fillStyle = baseColor
    this.ctx.fillRect(pixelX, pixelY, this.cellSize, this.cellSize)

    // 境界線
    this.ctx.strokeStyle = '#222222'
    this.ctx.lineWidth = 1
    this.ctx.strokeRect(pixelX, pixelY, this.cellSize, this.cellSize)

    // 3D効果
    this.ctx.fillStyle = this.brightenColor(baseColor, 0.4)
    this.ctx.fillRect(
      pixelX + 2,
      pixelY + 2,
      this.cellSize - 8,
      this.cellSize - 8
    )
  }

  private brightenColor(color: string, factor: number): string {
    // 簡単な色の明度調整
    const hex = color.replace('#', '')
    const r = Math.min(
      255,
      parseInt(hex.substr(0, 2), 16) + Math.floor(255 * factor)
    )
    const g = Math.min(
      255,
      parseInt(hex.substr(2, 2), 16) + Math.floor(255 * factor)
    )
    const b = Math.min(
      255,
      parseInt(hex.substr(4, 2), 16) + Math.floor(255 * factor)
    )

    return `#${r.toString(16).padStart(2, '0')}${g.toString(16).padStart(2, '0')}${b.toString(16).padStart(2, '0')}`
  }
}
