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

  setCell(x: number, y: number, value: number): void {
    if (!this.isValidPosition(x, y)) {
      return // 無効な位置には設定しない
    }
    this.grid[y][x] = value
  }

  // 同色ぷよの連結グループを検出する
  findConnectedGroups(): Array<Array<{x: number, y: number, color: number}>> {
    const visited: boolean[][] = Array(Config.STAGE_HEIGHT).fill(null).map(() => Array(Config.STAGE_WIDTH).fill(false))
    const groups: Array<Array<{x: number, y: number, color: number}>> = []

    for (let y = 0; y < Config.STAGE_HEIGHT; y++) {
      for (let x = 0; x < Config.STAGE_WIDTH; x++) {
        const color = this.getCell(x, y)
        
        // 空のセル、無効な値、または既に訪問済みの場合はスキップ
        if (color <= 0 || visited[y][x]) {
          continue
        }

        // 連結グループを探索（DFS）
        const group: Array<{x: number, y: number, color: number}> = []
        this.dfsConnectedGroup(x, y, color, visited, group)

        if (group.length > 0) {
          groups.push(group)
        }
      }
    }

    return groups
  }

  // 深度優先探索で同色の連結ぷよを探す
  private dfsConnectedGroup(
    x: number, 
    y: number, 
    targetColor: number, 
    visited: boolean[][], 
    group: Array<{x: number, y: number, color: number}>
  ): void {
    // 範囲外、異なる色、既に訪問済み、無効な値の場合は終了
    const cellValue = this.getCell(x, y)
    if (!this.isValidPosition(x, y) || visited[y][x] || cellValue !== targetColor || cellValue <= 0) {
      return
    }

    // 訪問済みにマークして、グループに追加
    visited[y][x] = true
    group.push({x, y, color: targetColor})

    // 隣接する4方向を探索
    const directions = [
      {dx: 0, dy: -1}, // 上
      {dx: 1, dy: 0},  // 右
      {dx: 0, dy: 1},  // 下
      {dx: -1, dy: 0}  // 左
    ]

    for (const {dx, dy} of directions) {
      this.dfsConnectedGroup(x + dx, y + dy, targetColor, visited, group)
    }
  }

  // 消去可能な連結グループ（4個以上）を取得する
  findEliminatableGroups(): Array<Array<{x: number, y: number, color: number}>> {
    const allGroups = this.findConnectedGroups()
    return allGroups.filter(group => group.length >= 4)
  }

  // ぷよを消去する
  eliminatePuyo(): number {
    const eliminatableGroups = this.findEliminatableGroups()
    let totalEliminated = 0

    // 各消去対象グループのぷよを削除
    for (const group of eliminatableGroups) {
      for (const puyo of group) {
        this.setCell(puyo.x, puyo.y, 0)
        totalEliminated++
      }
    }

    return totalEliminated
  }

  // 重力を適用する（消去後の落下処理）
  applyGravity(): void {
    // 各列で下から上へスキャンして詰める
    for (let x = 0; x < Config.STAGE_WIDTH; x++) {
      let writeY = Config.STAGE_HEIGHT - 1 // 書き込み位置（下から）

      // 下から上へスキャン
      for (let readY = Config.STAGE_HEIGHT - 1; readY >= 0; readY--) {
        const cell = this.getCell(x, readY)
        
        if (cell !== 0) { // 空でないセル
          if (writeY !== readY) {
            // 下の位置に移動
            this.setCell(x, writeY, cell)
            this.setCell(x, readY, 0)
          }
          writeY-- // 次の書き込み位置
        }
      }
    }
  }
}