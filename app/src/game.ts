import { Config } from './config'
import { PuyoImage } from './puyoimage'
import { Stage } from './stage'
import { Player } from './player'
import { Score } from './score'

export type GameMode = 'start' | 'checkFall' | 'fall' | 'checkErase' | 'erasing' | 'newPuyo' | 'playing' | 'moving' | 'rotating' | 'gameOver'

export class Game {
  private mode: GameMode = 'start'
  private frame: number = 0
  private combinationCount: number = 0
  private config: Config | undefined
  private puyoImage: PuyoImage | undefined
  private stage: Stage | undefined
  private player: Player | undefined
  private score: Score | undefined

  constructor() {
    // コンストラクタでは何もしない
  }

  initialize(): void {
    // 各コンポーネントの初期化
    this.config = new Config()
    this.puyoImage = new PuyoImage(this.config)
    this.stage = new Stage(this.config, this.puyoImage)
    this.player = new Player(this.config)
    this.score = new Score(this.config, this.stage)

    // ゲームモードを設定
    this.mode = 'start'
    this.frame = 0
    this.combinationCount = 0

    // 最初のぷよを作成
    if (this.player && this.stage && this.puyoImage && this.score) {
      this.mode = this.player.createNewPuyo(this.stage, this.puyoImage, this.score)
    }
  }

  loop(): void {
    if (!this.stage || !this.puyoImage || !this.player || !this.score) {
      requestAnimationFrame(this.loop.bind(this))
      return
    }

    switch (this.mode) {
      case 'start':
        // ゲーム開始処理
        this.mode = 'newPuyo'
        break

      case 'newPuyo':
        // 新しいぷよを作成
        this.mode = this.player.createNewPuyo(this.stage, this.puyoImage, this.score)
        break

      case 'playing':
        // プレイ中の処理
        this.mode = this.player.playing(this.frame)
        break

      case 'moving':
        // 移動中の処理
        if (!this.player.moving(this.frame)) {
          this.mode = 'playing'
        }
        break

      case 'rotating':
        // 回転中の処理
        if (!this.player.rotating(this.frame)) {
          this.mode = 'playing'
        }
        break

      case 'checkFall':
        // 落下確認
        this.stage.draw()
        if (this.stage.checkFall()) {
          this.mode = 'fall'
        } else {
          this.mode = 'checkErase'
        }
        break

      case 'fall':
        // 落下処理
        this.stage.fall()
        this.mode = 'checkFall'
        break

      case 'checkErase':
        // 消去確認
        const eraseResult = this.stage.checkErase()
        if (eraseResult.eraseCount > 0) {
          this.combinationCount++
          this.score.addScore(eraseResult.eraseCount, eraseResult.colorCount, this.combinationCount)
          this.mode = 'erasing'
        } else {
          this.combinationCount = 0
          this.mode = 'newPuyo'
        }
        break

      case 'erasing':
        // 消去処理
        this.stage.eraseBoards()
        this.mode = 'checkFall'
        break

      case 'gameOver':
        // ゲームオーバー処理
        if (this.puyoImage.gameOver(this.stage)) {
          // ゲーム終了
          console.log('Game Over!')
        }
        break
    }

    this.frame++
    requestAnimationFrame(this.loop.bind(this))
  }

  start(): void {
    this.initialize()
    this.loop()
  }
}
