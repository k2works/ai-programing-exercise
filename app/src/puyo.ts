export enum PuyoColor {
  Empty = 0,
  Red = 1,
  Blue = 2,
  Green = 3,
  Yellow = 4
}

export class Puyo {
  private color: PuyoColor
  private x: number
  private y: number

  constructor(color: PuyoColor, x: number, y: number) {
    this.color = color
    this.x = x
    this.y = y
  }

  getColor(): PuyoColor {
    return this.color
  }

  getX(): number {
    return this.x
  }

  getY(): number {
    return this.y
  }

  setPosition(x: number, y: number): void {
    this.x = x
    this.y = y
  }

  isEmpty(): boolean {
    return this.color === PuyoColor.Empty
  }
}