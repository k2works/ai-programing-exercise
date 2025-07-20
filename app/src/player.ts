import { Config } from './config'
import { Stage } from './stage'
import { PuyoImage } from './puyoimage'
import { PuyoPair } from './puyopair'
import { PuyoColor } from './puyo'

export class Player {
  private config: Config
  private stage: Stage
  private puyoImage: PuyoImage
  private currentPair: PuyoPair | null = null
  private nextPair: PuyoPair | null = null
  private fallTimer: number = 0
  private placed: boolean = false

  constructor(config: Config, stage: Stage, puyoImage: PuyoImage) {
    this.config = config
    this.stage = stage
    this.puyoImage = puyoImage
  }

  initialize(): void {
    this.currentPair = this.createRandomPair()
    this.nextPair = this.createRandomPair()
    this.fallTimer = 0
    this.placed = false
    
    // 現在のペアを初期位置に配置
    this.currentPair.setPosition(Math.floor(this.config.stageWidth / 2), 1)
  }

  getCurrentPair(): PuyoPair {
    if (!this.currentPair) {
      throw new Error('現在のぷよペアが初期化されていません')
    }
    return this.currentPair
  }

  getNextPair(): PuyoPair {
    if (!this.nextPair) {
      throw new Error('次のぷよペアが初期化されていません')
    }
    return this.nextPair
  }

  moveLeft(): boolean {
    if (!this.currentPair) return false
    
    const newX = this.currentPair.getX() - 1
    if (this.canMoveTo(newX, this.currentPair.getY())) {
      this.currentPair.setPosition(newX, this.currentPair.getY())
      return true
    }
    return false
  }

  moveRight(): boolean {
    if (!this.currentPair) return false
    
    const newX = this.currentPair.getX() + 1
    if (this.canMoveTo(newX, this.currentPair.getY())) {
      this.currentPair.setPosition(newX, this.currentPair.getY())
      return true
    }
    return false
  }

  rotateRight(): boolean {
    if (!this.currentPair) return false
    
    const originalRotation = this.currentPair.getRotation()
    this.currentPair.rotateRight()
    
    if (this.canMoveTo(this.currentPair.getX(), this.currentPair.getY())) {
      return true
    } else {
      // 回転できない場合は元に戻す
      this.currentPair.rotateLeft()
      return false
    }
  }

  rotateLeft(): boolean {
    if (!this.currentPair) return false
    
    const originalRotation = this.currentPair.getRotation()
    this.currentPair.rotateLeft()
    
    if (this.canMoveTo(this.currentPair.getX(), this.currentPair.getY())) {
      return true
    } else {
      // 回転できない場合は元に戻す
      this.currentPair.rotateRight()
      return false
    }
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

  isPlaced(): boolean {
    return this.placed
  }

  newPair(): void {
    this.currentPair = this.nextPair
    this.nextPair = this.createRandomPair()
    this.placed = false
    this.fallTimer = 0
    
    if (this.currentPair) {
      this.currentPair.setPosition(Math.floor(this.config.stageWidth / 2), 1)
    }
  }

  private canMoveTo(x: number, y: number): boolean {
    if (!this.currentPair) return false
    
    // メインぷよの位置チェック
    if (x < 0 || x >= this.config.stageWidth || 
        y < 0 || y >= this.config.stageHeight) {
      return false
    }
    
    if (!this.stage.isEmpty(x, y)) {
      return false
    }
    
    // サブぷよの位置チェック
    const tempPair = new PuyoPair(
      this.currentPair.getMainColor(),
      this.currentPair.getSubColor(),
      x, y
    )
    tempPair['rotation'] = this.currentPair.getRotation()
    
    const [subX, subY] = tempPair.getSubPosition()
    
    if (subX < 0 || subX >= this.config.stageWidth || 
        subY < 0 || subY >= this.config.stageHeight) {
      return false
    }
    
    if (!this.stage.isEmpty(subX, subY)) {
      return false
    }
    
    return true
  }

  private createRandomPair(): PuyoPair {
    const colors = [PuyoColor.Red, PuyoColor.Blue, PuyoColor.Green, PuyoColor.Yellow]
    const mainColor = colors[Math.floor(Math.random() * colors.length)]
    const subColor = colors[Math.floor(Math.random() * colors.length)]
    
    return new PuyoPair(mainColor, subColor, 0, 0)
  }
}
