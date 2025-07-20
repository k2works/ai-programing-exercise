import { Config } from './config'
import { PuyoImage } from './puyoimage'

export class Stage {
  private config: Config
  private puyoImage: PuyoImage

  constructor(config: Config, puyoImage: PuyoImage) {
    this.config = config
    this.puyoImage = puyoImage
  }
}
