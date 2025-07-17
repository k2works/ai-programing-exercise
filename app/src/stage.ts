import { Config } from './config'
import { PuyoImage } from './puyoimage'

export interface EraseInfo {
	erasePuyoCount: number
	eraseInfo: {
		x: number
		y: number
		type: number
	}[]
}

export class Stage {
  private readonly config: Config
  private readonly puyoImage: PuyoImage
  board: number[][] = []
  stageElement: HTMLElement
  scoreElement: HTMLElement
  private eraseFlag: boolean[][] = []

  constructor(config: Config, puyoImage: PuyoImage) {
    this.config = config
    this.puyoImage = puyoImage

    // ステージの初期化
    this.board = Array(this.config.stageRows)
      .fill(null)
      .map(() => Array(this.config.stageCols).fill(0))

    // 消去フラグの初期化
    this.eraseFlag = Array(this.config.stageRows)
      .fill(null)
      .map(() => Array(this.config.stageCols).fill(false))

    // ステージ要素の準備
    const stageElement = document.getElementById('stage')
    if (!stageElement) {
      throw new Error('stage element not found')
    }
    this.stageElement = stageElement

    // ステージの設定
    this.stageElement.style.width = `${this.config.stageCols * this.config.puyoImageWidth}px`
    this.stageElement.style.height = `${this.config.stageRows * this.config.puyoImageHeight}px`
    this.stageElement.style.backgroundColor = this.config.stageBackgroundColor
    this.stageElement.style.position = 'relative'

    // スコア要素の準備
    const scoreElement = document.getElementById('score')
    if (!scoreElement) {
      throw new Error('score element not found')
    }
    this.scoreElement = scoreElement

    // スコアの設定
    this.scoreElement.style.backgroundColor = this.config.scoreBackgroundColor
    this.scoreElement.style.width = `${this.config.stageCols * this.config.puyoImageWidth}px`
    this.scoreElement.style.height = `${this.config.fontHeight}px`
    this.scoreElement.style.position = 'relative'
  }

  draw(): void {
    // まずステージをクリア
    while (this.stageElement.firstChild) {
      this.stageElement.removeChild(this.stageElement.firstChild)
    }

    // ぷよを描画
    for (let y = 0; y < this.config.stageRows; y++) {
      for (let x = 0; x < this.config.stageCols; x++) {
        if (this.board[y][x] !== 0) {
          this.puyoImage.draw(this, x, y, this.board[y][x])
        }
      }
    }
  }

  checkErase(): { eraseCount: number; colorCount: number } {
    // 消去フラグを初期化
    this.eraseFlag = Array(this.config.stageRows)
      .fill(null)
      .map(() => Array(this.config.stageCols).fill(false))

    let totalEraseCount = 0
    let colorCount = 0

    // 各ぷよについて消去判定を行う
    for (let y = 0; y < this.config.stageRows; y++) {
      for (let x = 0; x < this.config.stageCols; x++) {
        if (this.board[y][x] !== 0 && !this.eraseFlag[y][x]) {
          const connectedPuyos = this.findConnectedPuyos(x, y, this.board[y][x])
          
          if (connectedPuyos.length >= 4) {
            // 4つ以上つながっている場合は消去対象
            colorCount++
            totalEraseCount += connectedPuyos.length
            
            // 消去フラグを立てる
            for (const puyo of connectedPuyos) {
              this.eraseFlag[puyo.y][puyo.x] = true
            }
          }
        }
      }
    }

    return { eraseCount: totalEraseCount, colorCount }
  }

  private findConnectedPuyos(startX: number, startY: number, color: number): Array<{x: number, y: number}> {
    const visited = Array(this.config.stageRows)
      .fill(null)
      .map(() => Array(this.config.stageCols).fill(false))
    
    const connectedPuyos: Array<{x: number, y: number}> = []
    const stack = [{ x: startX, y: startY }]

    while (stack.length > 0) {
      const current = stack.pop()!
      const { x, y } = current

      // 範囲外チェック
      if (x < 0 || x >= this.config.stageCols || y < 0 || y >= this.config.stageRows) {
        continue
      }

      // 既に訪問済みまたは異なる色の場合はスキップ
      if (visited[y][x] || this.board[y][x] !== color) {
        continue
      }

      // 訪問済みとしてマーク
      visited[y][x] = true
      connectedPuyos.push({ x, y })

      // 4方向の隣接するセルをスタックに追加
      stack.push({ x: x + 1, y })  // 右
      stack.push({ x: x - 1, y })  // 左
      stack.push({ x, y: y + 1 })  // 下
      stack.push({ x, y: y - 1 })  // 上
    }

    return connectedPuyos
  }

  eraseBoards(): void {
    // 消去フラグが立っているぷよを消去
    for (let y = 0; y < this.config.stageRows; y++) {
      for (let x = 0; x < this.config.stageCols; x++) {
        if (this.eraseFlag[y][x]) {
          this.board[y][x] = 0
        }
      }
    }
  }

  checkFall(): boolean {
		let hasFallen = false

		// 下から上に向かってチェック
		for (let y = this.config.stageRows - 2; y >= 0; y--) {
			for (let x = 0; x < this.config.stageCols; x++) {
				if (this.board[y][x] !== 0 && this.board[y + 1][x] === 0) {
					// 下が空いているので落下可能
					hasFallen = true
				}
			}
		}

		return hasFallen
  }

  fall(): void {
		// 下から上に向かって処理
		for (let y = this.config.stageRows - 2; y >= 0; y--) {
			for (let x = 0; x < this.config.stageCols; x++) {
				if (this.board[y][x] !== 0 && this.board[y + 1][x] === 0) {
					// ぷよを一段下に移動
					this.board[y + 1][x] = this.board[y][x]
					this.board[y][x] = 0
				}
			}
		}
  }

	setPuyo(x: number, y: number, type: number): void {
		// ぷよをボードに設定
		this.board[y][x] = type
	}

	getPuyo(x: number, y: number): number {
		// ボード外の場合は0（空）を返す
		if (x < 0 || x >= this.config.stageCols || y < 0 || y >= this.config.stageRows) {
			return 0
		}
		return this.board[y][x]
	}

	checkEraseIteration5(): EraseInfo {
		// 消去情報
		const eraseInfo: EraseInfo = {
			erasePuyoCount: 0,
			eraseInfo: []
		}

		// 一時的なチェック用ボード
		const checked: boolean[][] = []
		for (let y = 0; y < this.config.stageRows; y++) {
			checked[y] = []
			for (let x = 0; x < this.config.stageCols; x++) {
				checked[y][x] = false
			}
		}

		// 全マスをチェック
		for (let y = 0; y < this.config.stageRows; y++) {
			for (let x = 0; x < this.config.stageCols; x++) {
				// ぷよがあり、まだチェックしていない場合
				if (this.board[y][x] !== 0 && !checked[y][x]) {
					// 接続しているぷよを探索
					const puyoType = this.board[y][x]
					const connected: { x: number, y: number }[] = []
					this.searchConnectedPuyoIteration5(x, y, puyoType, checked, connected)

					// 4つ以上つながっている場合は消去対象
					if (connected.length >= 4) {
						for (const puyo of connected) {
							eraseInfo.eraseInfo.push({
								x: puyo.x,
								y: puyo.y,
								type: puyoType
							})
						}
						eraseInfo.erasePuyoCount += connected.length
					}
				}
			}
		}

		return eraseInfo
	}

	private searchConnectedPuyoIteration5(
		startX: number,
		startY: number,
		puyoType: number,
		checked: boolean[][],
		connected: { x: number, y: number }[]
	): void {
		// 探索済みにする
		checked[startY][startX] = true
		connected.push({ x: startX, y: startY })

		// 4方向を探索
		const directions = [
			{ dx: 1, dy: 0 },  // 右
			{ dx: -1, dy: 0 }, // 左
			{ dx: 0, dy: 1 },  // 下
			{ dx: 0, dy: -1 }  // 上
		]

		for (const direction of directions) {
			const nextX = startX + direction.dx
			const nextY = startY + direction.dy

			// ボード内かつ同じ色のぷよがあり、まだチェックしていない場合
			if (
				nextX >= 0 && nextX < this.config.stageCols &&
				nextY >= 0 && nextY < this.config.stageRows &&
				this.board[nextY][nextX] === puyoType &&
				!checked[nextY][nextX]
			) {
				// 再帰的に探索
				this.searchConnectedPuyoIteration5(nextX, nextY, puyoType, checked, connected)
			}
		}
	}

	eraseBoardsIteration5(eraseInfo: { x: number, y: number, type: number }[]): void {
		// 消去対象のぷよを消去
		for (const info of eraseInfo) {
			this.board[info.y][info.x] = 0
		}
	}

	fallIteration5(): void {
		// 下から上に向かって処理
		for (let y = this.config.stageRows - 2; y >= 0; y--) {
			for (let x = 0; x < this.config.stageCols; x++) {
				if (this.board[y][x] !== 0) {
					// 現在のぷよの下が空いている場合、落下させる
					let fallY = y
					while (fallY + 1 < this.config.stageRows && this.board[fallY + 1][x] === 0) {
						this.board[fallY + 1][x] = this.board[fallY][x]
						this.board[fallY][x] = 0
						fallY++
					}
				}
			}
		}
	}
}
