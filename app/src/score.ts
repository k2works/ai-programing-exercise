import { Config } from './config'
import { Stage } from './stage'

export class Score {
  public score: number = 0
  public fontLength: number = 0
  public fontTemplateList: (HTMLImageElement | HTMLCanvasElement)[] = []
  private readonly config: Config
  private readonly stage: Stage

  static readonly rensaBonus: number[] = [
    0, 16, 64, 160, 320, 560, 896, 1344, 1920, 2624, 3584, 4832, 6336, 8128,
    10240, 12672, 15424, 18496, 21888, 25600, 29632, 33984, 38656, 43648,
    48960, 54592, 60544, 66816, 73408, 80320, 87552, 95104, 102976, 111168,
    119680, 128512, 137664, 147136, 156928, 167040, 177472, 188224, 199296,
    210688, 222400, 234432, 246784, 259456, 272448, 285760, 299392, 313344,
    327616, 342208, 357120, 372352, 387904, 403776, 419968, 436480, 453312,
    470464, 487936, 505728, 523840, 542272, 561024, 580096, 599488, 619200,
    639232, 659584, 680256
  ]

  static readonly pieceBonus: number[] = [0, 0, 0, 0, 2, 3, 4, 5, 6, 7, 10, 10]
  static readonly colorBonus: number[] = [0, 0, 3, 6, 12, 24]

  constructor(config: Config, stage: Stage) {
    this.config = config
    this.stage = stage

    let fontWidth = 0
    for (let i = 0; i < 10; i++) {
      const fontImage = document.getElementById(`font${i}`) as HTMLImageElement | HTMLCanvasElement
      if (fontImage && fontWidth === 0) {
        fontWidth = (fontImage.width / fontImage.height) * this.config.fontHeight
      }
      if (fontImage) {
        fontImage.height = this.config.fontHeight
        fontImage.width = fontWidth
        this.fontTemplateList.push(fontImage)
      }
    }

    this.fontLength = Math.floor(
      (this.config.stageCols * this.config.puyoImageWidth) / this.fontTemplateList[0].width
    )

    // テストではイメージサイズの幅がマイナスになりフォント長が無限大になるので調整する
    if (this.fontLength === Infinity) {
      this.fontLength = 9
    }

    this.showScore()
  }

  showScore(): void {
    let score = this.score
    const scoreElement = this.stage.scoreElement
    // まず最初に、scoreElementの中身を空っぽにする
    while (scoreElement.firstChild) {
      scoreElement.removeChild(scoreElement.firstChild)
    }
    // スコアを下の桁から埋めていく
    for (let i = 0; i < this.fontLength; i++) {
      // 10で割ったあまりを求めて、一番下の桁を取り出す
      const number = score % 10
      // 一番うしろに追加するのではなく、一番前に追加することで、スコアの並びを数字と同じようにする
      scoreElement.insertBefore(
        this.fontTemplateList[number].cloneNode(true),
        scoreElement.firstChild
      )
      // 10で割って次の桁の準備をしておく
      score = Math.floor(score / 10)
    }
  }

  addScore(eraseCount: number, colorCount: number, rensaCount: number): void {
    const baseScore = eraseCount * 10
    const rensaBonus = Score.rensaBonus[Math.min(rensaCount, Score.rensaBonus.length - 1)]
    const pieceBonus = Score.pieceBonus[Math.min(eraseCount, Score.pieceBonus.length - 1)]
    const colorBonus = Score.colorBonus[Math.min(colorCount, Score.colorBonus.length - 1)]

    const totalBonus = rensaBonus + pieceBonus + colorBonus
    const additionalScore = baseScore * Math.max(totalBonus, 1)

    this.score += additionalScore
    this.showScore()
  }
}
