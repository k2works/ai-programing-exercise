import { PuyoColor } from './puyo'

export class PuyoImage {
  createRandomPuyoColor(): PuyoColor {
    const colors = [
      PuyoColor.Red,
      PuyoColor.Blue,
      PuyoColor.Green,
      PuyoColor.Yellow,
    ]
    return colors[Math.floor(Math.random() * colors.length)]
  }
}
