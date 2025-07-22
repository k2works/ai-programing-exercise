import { Config } from './Config'

// ゲームボードを管理するクラス
export class Stage {
  private grid: number[][]

  constructor() {
    this.grid = this.initializeGrid()
  }

  private initializeGrid(): number[][] {
    const grid: number[][] = []
    for (let y = 0; y < Config.STAGE_HEIGHT; y++) {
      grid[y] = []
      for (let x = 0; x < Config.STAGE_WIDTH; x++) {
        grid[y][x] = 0 // 0 = 空
      }
    }
    return grid
  }

  isEmpty(): boolean {
    for (let y = 0; y < Config.STAGE_HEIGHT; y++) {
      for (let x = 0; x < Config.STAGE_WIDTH; x++) {
        if (this.grid[y][x] !== 0) {
          return false
        }
      }
    }
    return true
  }

  getWidth(): number {
    return Config.STAGE_WIDTH
  }

  getHeight(): number {
    return Config.STAGE_HEIGHT
  }

  isValidPosition(x: number, y: number): boolean {
    return x >= 0 && x < Config.STAGE_WIDTH && y >= 0 && y < Config.STAGE_HEIGHT
  }

  getCell(x: number, y: number): number {
    if (!this.isValidPosition(x, y)) {
      return -1 // 無効な位置
    }
    return this.grid[y][x]
  }
}