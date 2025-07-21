export enum PuyoColor {
  Empty = 0,
  Red = 1,
  Blue = 2,
  Green = 3,
  Yellow = 4,
}

export class Puyo {
  constructor(
    private color: PuyoColor,
    private x: number,
    private y: number
  ) {}

  getColor(): PuyoColor {
    return this.color
  }

  getX(): number {
    return this.x
  }

  getY(): number {
    return this.y
  }

  setColor(color: PuyoColor): void {
    this.color = color
  }

  setPosition(x: number, y: number): void {
    this.x = x
    this.y = y
  }

  isEmpty(): boolean {
    return this.color === PuyoColor.Empty
  }
}
