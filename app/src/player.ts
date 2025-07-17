import { Config } from './config'
import { Stage } from './stage'
import { PuyoImage } from './puyoimage'
import type { GameMode } from './game'

export interface PuyoStatus {
  x: number
  y: number
  left: number
  top: number
  dx: number
  dy: number
  rotation: number
}

export interface KeyStatus {
  left: boolean
  right: boolean
  down: boolean
  up: boolean
}

export interface TouchPoint {
  x: number
  y: number
}

export class Player {
  private readonly config: Config
  private stage: Stage | undefined
  private score: any
  centerPuyo: number = 0
  movablePuyo: number = 0
  private centerPuyoElement: HTMLImageElement | HTMLCanvasElement | null | undefined
  private movablePuyoElement: HTMLImageElement | HTMLCanvasElement | null | undefined
  puyoStatus: PuyoStatus = { x: 0, y: 0, left: 0, top: 0, dx: 0, dy: 0, rotation: 0 }
  groundFrame: number = 0
  keyStatus: KeyStatus = { left: false, right: false, down: false, up: false }
	private previousKeyStatus: KeyStatus = { left: false, right: false, down: false, up: false }
	touchPoint: TouchPoint = { x: 0, y: 0 }

  constructor(config: Config) {
    this.config = config

    // キーボードイベントの設定
    document.addEventListener('keydown', this.onKeyDown.bind(this))
    document.addEventListener('keyup', this.onKeyUp.bind(this))
  }

  private onKeyDown(event: KeyboardEvent): void {
    switch (event.keyCode) {
      case 37: // 左キー
        this.keyStatus.left = true
        break
      case 39: // 右キー
        this.keyStatus.right = true
        break
      case 40: // 下キー
        this.keyStatus.down = true
        break
      case 38: // 上キー
        this.keyStatus.up = true
        break
    }
  }

  private onKeyUp(event: KeyboardEvent): void {
    switch (event.keyCode) {
      case 37: // 左キー
        this.keyStatus.left = false
        break
      case 39: // 右キー
        this.keyStatus.right = false
        break
      case 40: // 下キー
        this.keyStatus.down = false
        break
      case 38: // 上キー
        this.keyStatus.up = false
        break
    }
  }

  createNewPuyo(stage: Stage, puyoImage: PuyoImage, score: any): GameMode {
    this.stage = stage
    this.score = score

    // ぷよぷよが置けるかどうか、１番上の段の左から３つ目を確認する
    if (stage.board[0][2] !== 0) {
      // ゲームオーバー
      return 'gameOver'
    }

    // 新しいぷよを作成
    this.centerPuyo = Math.floor(Math.random() * 4) + 1
    this.movablePuyo = Math.floor(Math.random() * 4) + 1

    // ぷよの初期位置を設定
    this.puyoStatus = {
      x: 2,
      y: 0,
      left: 2 * this.config.puyoImageWidth,
      top: 0,
      dx: 0,
      dy: 0,
      rotation: 0
    }

    // ぷよを描画
    this.centerPuyoElement = puyoImage.draw(
      stage,
      this.puyoStatus.x,
      this.puyoStatus.y,
      this.centerPuyo,
      this.puyoStatus.dx,
      this.puyoStatus.dy
    )

    // 回転するぷよの位置を計算
    let movablePuyoX = this.puyoStatus.x
    let movablePuyoY = this.puyoStatus.y - 1

    this.movablePuyoElement = puyoImage.draw(
      stage,
      movablePuyoX,
      movablePuyoY,
      this.movablePuyo,
      this.puyoStatus.dx,
      this.puyoStatus.dy
    )

    this.groundFrame = 0

    return 'playing'
  }

	public playing(_frame: number): GameMode {
    if (this.stage === undefined) throw new Error('stage is undefined')

		// 左右移動の処理（連続入力を許可）
		if (this.keyStatus.left && this.canMove(-1)) {
			this.move(-1)
		}
		if (this.keyStatus.right && this.canMove(1)) {
			this.move(1)
		}

		// 回転の処理（一回だけ）
		if (this.keyStatus.up && !this.previousKeyStatus.up && this.canRotate()) {
			this.rotate()
		}

		// 前フレームのキー状態を記録
		this.previousKeyStatus = { ...this.keyStatus }

    // まず自由落下を確認する
    if (this.falling(this.keyStatus.down)) {
      return 'playing'
    }

    // 地面についた時の処理
    if (this.groundFrame > 0) {
      this.groundFrame--
      if (this.groundFrame === 0) {
        this.fix()
        return 'checkFall'
      }
    }

    return 'playing'
  }

  private falling(isDownPressed?: boolean): boolean {
    if (this.stage === undefined) throw new Error('stage is undefined')

    // 現状の場所の下にブロックがあるかどうか確認する
		const x = this.puyoStatus.x
		const y = this.puyoStatus.y

    // 中心ぷよの確認
    if (
      y + 1 >= this.config.stageRows ||
      this.stage.board[y + 1][x] !== 0
    ) {
      // 下にブロックがある、もしくは一番下まで来た場合は、地面に着いたとみなす
      if (this.groundFrame === 0) {
        this.groundFrame = 5
      }
      return false
    }

    // 落下速度の計算
		const fallSpeed = isDownPressed
      ? this.config.playerDownSpeed
      : this.config.playerFallSpeed

    this.puyoStatus.dy += fallSpeed

    if (this.puyoStatus.dy >= this.config.puyoImageHeight) {
      // 1マス分落下
      this.puyoStatus.y++
      this.puyoStatus.dy = 0
      this.draw()
    }

    return true
  }

  private fix(): void {
    if (this.stage === undefined) throw new Error('stage is undefined')

    // 現在のぷよをステージ上に配置する
    const y = this.puyoStatus.y
    const x = this.puyoStatus.x
    const rotation = this.puyoStatus.rotation

    // 中心ぷよを固定
    this.stage.board[y][x] = this.centerPuyo

    // 動くぷよの位置を計算
    let movablePuyoX = x
    let movablePuyoY = y - 1

    if (rotation === 90) {
      movablePuyoX = x - 1
      movablePuyoY = y
    } else if (rotation === 180) {
      movablePuyoX = x
      movablePuyoY = y + 1
    } else if (rotation === 270) {
      movablePuyoX = x + 1
      movablePuyoY = y
    }

    // 動くぷよを固定
    if (
      movablePuyoY >= 0 &&
      movablePuyoY < this.config.stageRows &&
      movablePuyoX >= 0 &&
      movablePuyoX < this.config.stageCols
    ) {
      this.stage.board[movablePuyoY][movablePuyoX] = this.movablePuyo
    }

    // 画面から削除
    if (this.centerPuyoElement && this.centerPuyoElement.parentNode) {
      this.centerPuyoElement.parentNode.removeChild(this.centerPuyoElement)
    }
    if (this.movablePuyoElement && this.movablePuyoElement.parentNode) {
      this.movablePuyoElement.parentNode.removeChild(this.movablePuyoElement)
    }
  }

  private draw(): void {
    // ぷよの描画更新
    if (this.centerPuyoElement) {
      this.centerPuyoElement.style.left = `${this.puyoStatus.left + this.puyoStatus.dx}px`
      this.centerPuyoElement.style.top = `${this.puyoStatus.top + this.puyoStatus.dy}px`
    }

    if (this.movablePuyoElement) {
      // 動くぷよの位置を計算
      let movablePuyoLeft = this.puyoStatus.left
      let movablePuyoTop = this.puyoStatus.top - this.config.puyoImageHeight

      if (this.puyoStatus.rotation === 90) {
        movablePuyoLeft = this.puyoStatus.left - this.config.puyoImageWidth
        movablePuyoTop = this.puyoStatus.top
      } else if (this.puyoStatus.rotation === 180) {
        movablePuyoLeft = this.puyoStatus.left
        movablePuyoTop = this.puyoStatus.top + this.config.puyoImageHeight
      } else if (this.puyoStatus.rotation === 270) {
        movablePuyoLeft = this.puyoStatus.left + this.config.puyoImageWidth
        movablePuyoTop = this.puyoStatus.top
      }

      this.movablePuyoElement.style.left = `${movablePuyoLeft + this.puyoStatus.dx}px`
      this.movablePuyoElement.style.top = `${movablePuyoTop + this.puyoStatus.dy}px`
    }
  }

	moving(_frame: number): boolean {
    // 移動処理の実装（簡略版）
    return false
  }

	rotating(_frame: number): boolean {
    // 回転処理の実装（簡略版）
    return false
  }

	private canMove(direction: number): boolean {
		if (this.stage === undefined) return false

		const newX = this.puyoStatus.x + direction
		const y = this.puyoStatus.y
		const rotation = this.puyoStatus.rotation

		// ステージの境界チェック
		if (newX < 0 || newX >= this.config.stageCols) {
			return false
		}

		// 中心ぷよの衝突チェック
		if (this.stage.board[y][newX] !== 0) {
			return false
		}

		// 動くぷよの衝突チェック
		let movablePuyoX = newX
		let movablePuyoY = y - 1

		if (rotation === 90) {
			movablePuyoX = newX - 1
			movablePuyoY = y
		} else if (rotation === 180) {
			movablePuyoX = newX
			movablePuyoY = y + 1
		} else if (rotation === 270) {
			movablePuyoX = newX + 1
			movablePuyoY = y
		}

		// 動くぷよの位置チェック
		if (
			movablePuyoX < 0 ||
			movablePuyoX >= this.config.stageCols ||
			movablePuyoY >= this.config.stageRows ||
			(movablePuyoY >= 0 && this.stage.board[movablePuyoY][movablePuyoX] !== 0)
		) {
			return false
		}

		return true
	}

	private move(direction: number): void {
		this.puyoStatus.x += direction
		this.puyoStatus.left = this.puyoStatus.x * this.config.puyoImageWidth
		this.draw()
	}

	private canRotate(): boolean {
		if (this.stage === undefined) return false

		const x = this.puyoStatus.x
		const y = this.puyoStatus.y
		const newRotation = (this.puyoStatus.rotation + 90) % 360

		// 新しい回転位置での動くぷよの位置を計算
		let movablePuyoX = x
		let movablePuyoY = y - 1

		if (newRotation === 90) {
			movablePuyoX = x - 1
			movablePuyoY = y
		} else if (newRotation === 180) {
			movablePuyoX = x
			movablePuyoY = y + 1
		} else if (newRotation === 270) {
			movablePuyoX = x + 1
			movablePuyoY = y
		}

		// 境界チェック
		if (
			movablePuyoX < 0 ||
			movablePuyoX >= this.config.stageCols ||
			movablePuyoY >= this.config.stageRows
		) {
			return false
		}

		// 衝突チェック（上側は除外）
		if (movablePuyoY >= 0 && this.stage.board[movablePuyoY][movablePuyoX] !== 0) {
			return false
		}

		return true
	}

	private rotate(): void {
		this.puyoStatus.rotation = (this.puyoStatus.rotation + 90) % 360
		this.draw()
	}
}
