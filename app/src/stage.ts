import { Config } from './config'
import { PuyoImage } from './puyoimage'

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
}
