import { Config } from './config'
import { Stage } from './stage'
import { Player } from './player'
import { Score } from './score'
import { PuyoImage } from './puyoimage'

export type GameMode =
  | 'start'
  | 'checkFall'
  | 'fall'
  | 'checkErase'
  | 'erasing'
  | 'newPuyo'
  | 'playing'
  | 'gameOver'

export class Game {
  private mode: GameMode = 'start'
  private frame: number = 0
  private combinationCount: number = 0
  private erasableGroups: [number, number][][] | null = null

  private config: Config
  private stage: Stage
  private player: Player
  private score: Score
  private puyoImage: PuyoImage

  constructor() {
    this.config = new Config()
    this.puyoImage = new PuyoImage()
    this.stage = new Stage(this.config, this.puyoImage)
    this.player = new Player(this.config, this.stage, this.puyoImage)
    this.score = new Score()
  }

  initialize(): void {
    this.stage.initialize()
    this.player.initialize()
    this.score.reset()
    this.mode = 'newPuyo'
    this.frame = 0
    this.combinationCount = 0
    this.erasableGroups = null
  }

  update(): void {
    this.frame++

    switch (this.mode) {
      case 'start':
        this.updateStart()
        break
      case 'checkFall':
        this.updateCheckFall()
        break
      case 'fall':
        this.updateFall()
        break
      case 'checkErase':
        this.updateCheckErase()
        break
      case 'erasing':
        this.updateErasing()
        break
      case 'newPuyo':
        this.updateNewPuyo()
        break
      case 'playing':
        this.updatePlaying()
        break
      case 'gameOver':
        // ゲームオーバー時は何もしない
        break
    }
  }

  private updateStart(): void {
    this.mode = 'newPuyo'
  }

  private updateCheckFall(): void {
    if (this.stage.checkFall()) {
      this.mode = 'fall'
    } else {
      this.mode = 'checkErase'
    }
  }

  private updateFall(): void {
    this.stage.applyFall()
    this.mode = 'checkFall'
  }

  private updateCheckErase(): void {
    this.erasableGroups = this.stage.findErasableGroups()
    if (this.erasableGroups.length > 0) {
      this.mode = 'erasing'
    } else {
      this.combinationCount = 0
      this.mode = 'newPuyo'
    }
  }

  private updateErasing(): void {
    if (this.erasableGroups) {
      const erasedCount = this.stage.erasePuyos(this.erasableGroups)
      this.combinationCount++

      const baseScore = erasedCount * 10
      const chainMultiplier = this.calculateChainMultiplier(
        this.combinationCount
      )
      this.score.addScore(baseScore * chainMultiplier)

      this.erasableGroups = null
    }
    this.mode = 'checkFall'
  }

  private updateNewPuyo(): void {
    if (this.canPlaceNewPuyo()) {
      this.player.newPair()
      this.mode = 'playing'
    } else {
      this.mode = 'gameOver'
    }
  }

  private updatePlaying(): void {
    this.player.update()
    if (this.player.isPlaced()) {
      this.player.placePuyo()
      this.mode = 'checkFall'
    }
  }

  private canPlaceNewPuyo(): boolean {
    const currentPair = this.player.getCurrentPair()
    const mainX = currentPair.getX()
    const mainY = currentPair.getY()
    const [subX, subY] = currentPair.getSubPosition()

    return this.stage.isEmpty(mainX, mainY) && this.stage.isEmpty(subX, subY)
  }

  private calculateChainMultiplier(chainCount: number): number {
    if (chainCount <= 1) return 1
    return Math.pow(2, chainCount - 1)
  }

  reset(): void {
    this.initialize()
  }

  // 外部からのアクセス用メソッド（描画・入力処理用）
  getMode(): GameMode {
    return this.mode
  }

  getScore(): number {
    return this.score.getScore()
  }

  getCombinationCount(): number {
    return this.combinationCount
  }

  getFrame(): number {
    return this.frame
  }

  // Friendアクセス用（描画・入力システム用）
  getStageForRenderer(): Stage {
    return this.stage
  }

  getPlayerForRenderer(): Player {
    return this.player
  }

  getConfigForRenderer(): Config {
    return this.config
  }
}
