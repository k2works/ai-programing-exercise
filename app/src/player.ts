import { Config } from './config'
import { Stage } from './stage'
import { PuyoImage } from './puyoimage'

export class Player {
  private config: Config
  private stage: Stage
  private puyoImage: PuyoImage

  constructor(config: Config, stage: Stage, puyoImage: PuyoImage) {
    this.config = config
    this.stage = stage
    this.puyoImage = puyoImage
  }
}
