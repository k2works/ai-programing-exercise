import { Config } from './config'
import { PuyoImage } from './puyoimage'
import { Puyo, PuyoColor } from './puyo'

export class Stage {
  private config: Config
  private puyoImage: PuyoImage
  private grid: Puyo[][]

  constructor(config: Config, puyoImage: PuyoImage) {
    this.config = config
    this.puyoImage = puyoImage
    this.grid = []
  }

  initialize(): void {
    this.grid = []
    for (let x = 0; x < this.config.stageWidth; x++) {
      this.grid[x] = []
      for (let y = 0; y < this.config.stageHeight; y++) {
        this.grid[x][y] = new Puyo(PuyoColor.Empty, x, y)
      }
    }
  }

  getWidth(): number {
    return this.config.stageWidth
  }

  getHeight(): number {
    return this.config.stageHeight
  }

  getPuyo(x: number, y: number): Puyo {
    this.validatePosition(x, y)
    return this.grid[x][y]
  }

  setPuyo(x: number, y: number, puyo: Puyo): void {
    this.validatePosition(x, y)
    this.grid[x][y] = puyo
  }

  isEmpty(x: number, y: number): boolean {
    this.validatePosition(x, y)
    return this.grid[x][y].isEmpty()
  }

  checkFall(): boolean {
    for (let x = 0; x < this.config.stageWidth; x++) {
      for (let y = 0; y < this.config.stageHeight - 1; y++) {
        if (!this.isEmpty(x, y) && this.isEmpty(x, y + 1)) {
          return true
        }
      }
    }
    return false
  }

  applyFall(): void {
    for (let x = 0; x < this.config.stageWidth; x++) {
      for (let y = this.config.stageHeight - 2; y >= 0; y--) {
        if (!this.isEmpty(x, y) && this.isEmpty(x, y + 1)) {
          const puyo = this.getPuyo(x, y)
          this.setPuyo(x, y + 1, new Puyo(puyo.getColor(), x, y + 1))
          this.setPuyo(x, y, new Puyo(PuyoColor.Empty, x, y))
        }
      }
    }
  }

  private validatePosition(x: number, y: number): void {
    if (x < 0 || x >= this.config.stageWidth || y < 0 || y >= this.config.stageHeight) {
      throw new Error(`座標が範囲外です: (${x}, ${y})`)
    }
  }
}
