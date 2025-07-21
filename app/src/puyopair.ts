import { PuyoColor } from './puyo'

export enum PairRotation {
  Up = 0,
  Right = 1,
  Down = 2,
  Left = 3,
}

export class PuyoPair {
  private rotation: PairRotation = PairRotation.Up

  constructor(
    private mainColor: PuyoColor,
    private subColor: PuyoColor,
    private x: number,
    private y: number
  ) {}

  getMainColor(): PuyoColor {
    return this.mainColor
  }

  getSubColor(): PuyoColor {
    return this.subColor
  }

  getX(): number {
    return this.x
  }

  getY(): number {
    return this.y
  }

  getRotation(): PairRotation {
    return this.rotation
  }

  setPosition(x: number, y: number): void {
    this.x = x
    this.y = y
  }

  rotateRight(): void {
    this.rotation = (this.rotation + 1) % 4
  }

  rotateLeft(): void {
    this.rotation = (this.rotation + 3) % 4
  }

  getSubPosition(): [number, number] {
    switch (this.rotation) {
      case PairRotation.Up:
        return [this.x, this.y - 1]
      case PairRotation.Right:
        return [this.x + 1, this.y]
      case PairRotation.Down:
        return [this.x, this.y + 1]
      case PairRotation.Left:
        return [this.x - 1, this.y]
      default:
        return [this.x, this.y - 1]
    }
  }
}
