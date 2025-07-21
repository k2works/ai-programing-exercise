import { Config } from './config'
import { Puyo, PuyoColor } from './puyo'
import { PuyoImage } from './puyoimage'

export class Stage {
  private grid: Puyo[][]

  constructor(
    private config: Config,
    private puyoImage: PuyoImage
  ) {
    this.grid = []
    this.initialize()
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

  private validatePosition(x: number, y: number): void {
    if (
      x < 0 ||
      x >= this.config.stageWidth ||
      y < 0 ||
      y >= this.config.stageHeight
    ) {
      throw new Error(`座標が範囲外です: (${x}, ${y})`)
    }
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
    if (!this.isValidPosition(x, y)) {
      return false
    }
    return this.getPuyo(x, y).isEmpty()
  }

  isValidPosition(x: number, y: number): boolean {
    return (
      x >= 0 &&
      x < this.config.stageWidth &&
      y >= 0 &&
      y < this.config.stageHeight
    )
  }

  getWidth(): number {
    return this.config.stageWidth
  }

  getHeight(): number {
    return this.config.stageHeight
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

  findErasableGroups(): [number, number][][] {
    const visited = new Set<string>()
    const erasableGroups: [number, number][][] = []

    for (let x = 0; x < this.config.stageWidth; x++) {
      for (let y = 0; y < this.config.stageHeight; y++) {
        const key = `${x},${y}`
        if (visited.has(key) || this.isEmpty(x, y)) {
          continue
        }

        const group = this.findConnectedGroup(x, y, visited)
        if (group.length >= 4) {
          erasableGroups.push(group)
        }
      }
    }

    return erasableGroups
  }

  private findConnectedGroup(
    startX: number,
    startY: number,
    visited: Set<string>
  ): [number, number][] {
    const targetColor = this.getPuyo(startX, startY).getColor()
    const group: [number, number][] = []
    const queue: [number, number][] = [[startX, startY]]

    while (queue.length > 0) {
      const [x, y] = queue.shift()!
      const key = `${x},${y}`

      if (visited.has(key)) continue

      visited.add(key)
      group.push([x, y])

      const directions = [
        [0, -1], // 上
        [0, 1], // 下
        [-1, 0], // 左
        [1, 0], // 右
      ]

      directions.forEach(([dx, dy]) => {
        const newX = x + dx
        const newY = y + dy

        if (
          this.isValidPosition(newX, newY) &&
          !visited.has(`${newX},${newY}`) &&
          !this.isEmpty(newX, newY) &&
          this.getPuyo(newX, newY).getColor() === targetColor
        ) {
          queue.push([newX, newY])
        }
      })
    }

    return group
  }

  erasePuyos(groups: [number, number][][]): number {
    let erasedCount = 0
    for (const group of groups) {
      for (const [x, y] of group) {
        this.setPuyo(x, y, new Puyo(PuyoColor.Empty, x, y))
        erasedCount++
      }
    }
    return erasedCount
  }
}
