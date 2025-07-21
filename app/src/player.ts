import { Config } from './config'
import { Stage } from './stage'
import { PuyoPair } from './puyopair'
import { PuyoImage } from './puyoimage'
import { Puyo } from './puyo'

export class Player {
  private currentPair: PuyoPair | null = null
  private nextPair: PuyoPair | null = null
  private fallTimer: number = 0
  private placed: boolean = false

  constructor(
    private config: Config,
    private stage: Stage,
    private puyoImage: PuyoImage
  ) {}

  initialize(): void {
    this.currentPair = this.createRandomPair()
    this.nextPair = this.createRandomPair()
    this.fallTimer = 0
    this.placed = false
  }

  private createRandomPair(): PuyoPair {
    const mainColor = this.puyoImage.createRandomPuyoColor()
    const subColor = this.puyoImage.createRandomPuyoColor()
    const startX = Math.floor(this.config.stageWidth / 2)
    const startY = 1
    return new PuyoPair(mainColor, subColor, startX, startY)
  }

  getCurrentPair(): PuyoPair {
    if (!this.currentPair) {
      throw new Error('現在のぷよペアがありません')
    }
    return this.currentPair
  }

  getNextPair(): PuyoPair {
    if (!this.nextPair) {
      throw new Error('次のぷよペアがありません')
    }
    return this.nextPair
  }

  isPlaced(): boolean {
    return this.placed
  }

  moveLeft(): boolean {
    if (!this.currentPair || this.placed) return false

    const newX = this.currentPair.getX() - 1
    if (this.canMoveTo(newX, this.currentPair.getY())) {
      this.currentPair.setPosition(newX, this.currentPair.getY())
      return true
    }
    return false
  }

  moveRight(): boolean {
    if (!this.currentPair || this.placed) return false

    const newX = this.currentPair.getX() + 1
    if (this.canMoveTo(newX, this.currentPair.getY())) {
      this.currentPair.setPosition(newX, this.currentPair.getY())
      return true
    }
    return false
  }

  rotateRight(): boolean {
    if (!this.currentPair || this.placed) return false

    this.currentPair.rotateRight()

    if (!this.canMoveTo(this.currentPair.getX(), this.currentPair.getY())) {
      // 回転できない場合は元に戻す
      this.currentPair.rotateLeft()
      return false
    }
    return true
  }

  rotateLeft(): boolean {
    if (!this.currentPair || this.placed) return false

    this.currentPair.rotateLeft()

    if (!this.canMoveTo(this.currentPair.getX(), this.currentPair.getY())) {
      // 回転できない場合は元に戻す
      this.currentPair.rotateRight()
      return false
    }
    return true
  }

  private canMoveTo(x: number, y: number): boolean {
    if (!this.currentPair) return false

    // メインぷよの範囲チェック
    if (
      x < 0 ||
      x >= this.config.stageWidth ||
      y < 0 ||
      y >= this.config.stageHeight
    ) {
      return false
    }

    if (!this.stage.isEmpty(x, y)) {
      return false
    }

    // サブぷよの衝突判定
    const tempPair = new PuyoPair(
      this.currentPair.getMainColor(),
      this.currentPair.getSubColor(),
      x,
      y
    )
    // 回転状態をコピー
    for (let i = 0; i < this.currentPair.getRotation(); i++) {
      tempPair.rotateRight()
    }

    const [subX, subY] = tempPair.getSubPosition()

    return this.stage.isEmpty(subX, subY) && this.isValidPosition(subX, subY)
  }

  private isValidPosition(x: number, y: number): boolean {
    return (
      x >= 0 &&
      x < this.config.stageWidth &&
      y >= 0 &&
      y < this.config.stageHeight
    )
  }

  update(): void {
    if (!this.currentPair || this.placed) return

    this.fallTimer++
    if (this.fallTimer >= this.config.fallSpeed) {
      this.fallTimer = 0

      const newY = this.currentPair.getY() + 1
      if (this.canMoveTo(this.currentPair.getX(), newY)) {
        this.currentPair.setPosition(this.currentPair.getX(), newY)
      } else {
        this.placed = true
      }
    }
  }

  placePuyo(): void {
    if (!this.currentPair) return

    // メインぷよを配置
    const mainX = this.currentPair.getX()
    const mainY = this.currentPair.getY()
    this.stage.setPuyo(
      mainX,
      mainY,
      new Puyo(this.currentPair.getMainColor(), mainX, mainY)
    )

    // サブぷよを配置
    const [subX, subY] = this.currentPair.getSubPosition()
    this.stage.setPuyo(
      subX,
      subY,
      new Puyo(this.currentPair.getSubColor(), subX, subY)
    )
  }

  newPair(): void {
    this.currentPair = this.nextPair
    this.nextPair = this.createRandomPair()
    this.fallTimer = 0
    this.placed = false
  }
}
