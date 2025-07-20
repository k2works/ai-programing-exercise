import { Game } from './game'
import { Stage } from './stage'
import { Player } from './player'
import { PuyoColor } from './puyo'

export class GameRenderer {
  private canvas: HTMLCanvasElement
  private ctx: CanvasRenderingContext2D
  private nextCanvas: HTMLCanvasElement
  private nextCtx: CanvasRenderingContext2D
  private cellSize: number = 32

  // ぷよの色定義
  private readonly colors = {
    [PuyoColor.Empty]: '#000000',
    [PuyoColor.Red]: '#ff4444',
    [PuyoColor.Blue]: '#4444ff',
    [PuyoColor.Green]: '#44ff44',
    [PuyoColor.Yellow]: '#ffff44'
  }

  constructor(canvas: HTMLCanvasElement, nextCanvas: HTMLCanvasElement) {
    this.canvas = canvas
    this.nextCanvas = nextCanvas
    
    const ctx = canvas.getContext('2d')
    const nextCtx = nextCanvas.getContext('2d')
    
    if (!ctx || !nextCtx) {
      throw new Error('Failed to get canvas context')
    }
    
    this.ctx = ctx
    this.nextCtx = nextCtx
  }

  render(game: Game): void {
    // メインキャンバスをクリア
    this.ctx.fillStyle = '#000000'
    this.ctx.fillRect(0, 0, this.canvas.width, this.canvas.height)

    // ステージを描画
    this.renderStage(game['stage'])
    
    // 現在落下中のぷよペアを描画
    this.renderCurrentPair(game['player'])
    
    // 次のぷよを描画
    this.renderNextPuyo(game['player'])

    // グリッドラインを描画
    this.renderGrid()
  }

  private renderStage(stage: Stage): void {
    for (let x = 0; x < stage.getWidth(); x++) {
      for (let y = 0; y < stage.getHeight(); y++) {
        const puyo = stage.getPuyo(x, y)
        if (!puyo.isEmpty()) {
          this.renderPuyo(x, y, puyo.getColor())
        }
      }
    }
  }

  private renderCurrentPair(player: Player): void {
    try {
      const currentPair = player.getCurrentPair()
      const mainX = currentPair.getX()
      const mainY = currentPair.getY()
      const [subX, subY] = currentPair.getSubPosition()

      // メインぷよを描画（少し明るく）
      this.renderPuyo(mainX, mainY, currentPair.getMainColor(), true)
      
      // サブぷよを描画（少し明るく）
      this.renderPuyo(subX, subY, currentPair.getSubColor(), true)
    } catch (error) {
      // プレイヤーが初期化されていない場合は何もしない
    }
  }

  private renderNextPuyo(player: Player): void {
    // 次のぷよ用キャンバスをクリア
    this.nextCtx.fillStyle = '#000000'
    this.nextCtx.fillRect(0, 0, this.nextCanvas.width, this.nextCanvas.height)

    try {
      const nextPair = player.getNextPair()
      
      // 次のぷよを小さなキャンバスに描画
      const smallCellSize = 16
      this.renderPuyoOnCanvas(this.nextCtx, 1, 0.5, nextPair.getMainColor(), smallCellSize)
      this.renderPuyoOnCanvas(this.nextCtx, 1, 2.5, nextPair.getSubColor(), smallCellSize)
    } catch (error) {
      // プレイヤーが初期化されていない場合は何もしない
    }
  }

  private renderPuyo(x: number, y: number, color: PuyoColor, highlight: boolean = false): void {
    if (color === PuyoColor.Empty) return

    const pixelX = x * this.cellSize
    const pixelY = y * this.cellSize

    // 基本色
    let baseColor = this.colors[color]
    
    if (highlight) {
      // ハイライト効果（少し明るく）
      baseColor = this.brightenColor(baseColor, 0.3)
    }

    this.ctx.fillStyle = baseColor
    this.ctx.fillRect(pixelX, pixelY, this.cellSize, this.cellSize)

    // 境界線を描画
    this.ctx.strokeStyle = '#222222'
    this.ctx.lineWidth = 1
    this.ctx.strokeRect(pixelX, pixelY, this.cellSize, this.cellSize)

    // ぷよの輪郭を描画（3D効果）
    this.ctx.fillStyle = this.brightenColor(baseColor, 0.4)
    this.ctx.fillRect(pixelX + 2, pixelY + 2, this.cellSize - 8, this.cellSize - 8)
  }

  private renderPuyoOnCanvas(ctx: CanvasRenderingContext2D, x: number, y: number, color: PuyoColor, size: number): void {
    if (color === PuyoColor.Empty) return

    const pixelX = x * size
    const pixelY = y * size

    ctx.fillStyle = this.colors[color]
    ctx.fillRect(pixelX, pixelY, size, size)

    ctx.strokeStyle = '#222222'
    ctx.lineWidth = 1
    ctx.strokeRect(pixelX, pixelY, size, size)
  }

  private renderGrid(): void {
    this.ctx.strokeStyle = '#333333'
    this.ctx.lineWidth = 0.5

    // 縦線
    for (let x = 0; x <= 6; x++) {
      const pixelX = x * this.cellSize
      this.ctx.beginPath()
      this.ctx.moveTo(pixelX, 0)
      this.ctx.lineTo(pixelX, this.canvas.height)
      this.ctx.stroke()
    }

    // 横線
    for (let y = 0; y <= 13; y++) {
      const pixelY = y * this.cellSize
      this.ctx.beginPath()
      this.ctx.moveTo(0, pixelY)
      this.ctx.lineTo(this.canvas.width, pixelY)
      this.ctx.stroke()
    }
  }

  private brightenColor(hexColor: string, factor: number): string {
    // #rrggbb形式の色を明るくする
    const hex = hexColor.replace('#', '')
    const r = Math.min(255, Math.floor(parseInt(hex.substr(0, 2), 16) * (1 + factor)))
    const g = Math.min(255, Math.floor(parseInt(hex.substr(2, 2), 16) * (1 + factor)))
    const b = Math.min(255, Math.floor(parseInt(hex.substr(4, 2), 16) * (1 + factor)))
    
    return `#${r.toString(16).padStart(2, '0')}${g.toString(16).padStart(2, '0')}${b.toString(16).padStart(2, '0')}`
  }
}