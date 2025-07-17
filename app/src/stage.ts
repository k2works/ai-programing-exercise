import { Config } from './config'
import { PuyoImage } from './puyoimage'

export class Stage {
  private readonly config: Config
  private readonly puyoImage: PuyoImage
  board: number[][] = []
  stageElement: HTMLElement
  scoreElement: HTMLElement

  constructor(config: Config, puyoImage: PuyoImage) {
    this.config = config
    this.puyoImage = puyoImage

    // ステージの初期化
    this.board = Array(this.config.stageRows)
      .fill(null)
      .map(() => Array(this.config.stageCols).fill(0))

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
    // 消去判定の実装（簡略版）
    return { eraseCount: 0, colorCount: 0 }
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
