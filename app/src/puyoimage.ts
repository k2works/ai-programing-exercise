import { Config } from './config'
import type { Stage } from './stage'

export class PuyoImage {
  private readonly config: Config
  puyoImages: (HTMLImageElement | HTMLCanvasElement)[] = []
  batankyuImage: HTMLImageElement | HTMLCanvasElement | undefined
  private gameOverFrame: number = 0

  constructor(config: Config) {
    this.config = config

    // ぷよ画像の準備
    for (let i = 1; i <= 5; i++) {
      const puyoImage = document.getElementById(`puyo${i}`) as HTMLImageElement | HTMLCanvasElement
      if (puyoImage) {
        puyoImage.width = this.config.puyoImageWidth
        puyoImage.height = this.config.puyoImageHeight
        puyoImage.style.position = 'absolute'
        this.puyoImages.push(puyoImage)
      }
    }

    // ばたんきゅー画像の準備
    const batankyuImage = document.getElementById('batankyu') as HTMLImageElement | HTMLCanvasElement
    if (batankyuImage) {
      batankyuImage.width = this.config.stageCols * this.config.puyoImageWidth
      batankyuImage.height = this.config.stageRows * this.config.puyoImageHeight
      batankyuImage.style.position = 'absolute'
      this.batankyuImage = batankyuImage
    }
  }

  draw(
    stage: Stage,
    x: number,
    y: number,
    puyoType: number,
    dx?: number,
    dy?: number
  ): HTMLImageElement | HTMLCanvasElement | null {
    if (puyoType === 0) return null

    const puyoImage = this.puyoImages[puyoType - 1]?.cloneNode(true) as HTMLImageElement | HTMLCanvasElement
    if (!puyoImage) return null

    puyoImage.style.left = `${(dx || 0) + x * this.config.puyoImageWidth}px`
    puyoImage.style.top = `${(dy || 0) + y * this.config.puyoImageHeight}px`

    stage.stageElement.appendChild(puyoImage)
    return puyoImage
  }

  gameOver(stage: Stage): boolean {
    this.gameOverFrame++

    if (this.gameOverFrame === 1 && this.batankyuImage) {
      // ばたんきゅー画像を表示
      this.batankyuImage.style.left = '0px'
      this.batankyuImage.style.top = '0px'
      stage.stageElement.appendChild(this.batankyuImage)
    }

    if (this.gameOverFrame > 120) {
      // 2秒後にゲーム終了
      return true
    }

    return false
  }
}
