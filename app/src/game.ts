import { Config } from './config'
import { PuyoImage } from './puyoimage'
import { Stage } from './stage'
import { Player } from './player'
import { Score } from './score'
import { Puyo } from './puyo'

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
  private config: Config
  private puyoImage: PuyoImage
  private stage: Stage
  private player: Player
  private score: Score

  constructor() {
    // コンストラクタでは何もしない
  }

  initialize(): void {
    // 各コンポーネントの初期化
    this.config = new Config()
    this.puyoImage = new PuyoImage(this.config)
    this.stage = new Stage(this.config, this.puyoImage)
    this.stage.initialize()
    this.player = new Player(this.config, this.stage, this.puyoImage)
    this.player.initialize()
    this.score = new Score()

    // ゲームモードを設定
    this.mode = 'start'
    this.frame = 0
    this.combinationCount = 0
  }

  update(): void {
    this.frame++

    switch (this.mode) {
      case 'start':
        this.updateStart()
        break
      case 'newPuyo':
        this.updateNewPuyo()
        break
      case 'playing':
        this.updatePlaying()
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
      case 'gameOver':
        this.updateGameOver()
        break
    }
  }

  private updateStart(): void {
    this.mode = 'newPuyo'
  }

  private updateNewPuyo(): void {
    // 新しいぷよペアを生成
    this.player.newPair()
    this.mode = 'playing'
  }

  private updatePlaying(): void {
    // プレイヤーの自動落下処理
    this.player.update()
    
    // プレイヤーのぷよが配置されたかチェック
    if (this.player.isPlaced()) {
      this.placePuyoPair()
      this.mode = 'checkFall'
    }
  }

  private updateCheckFall(): void {
    if (this.stage.checkFall()) {
      this.mode = 'fall'
    } else {
      this.mode = 'newPuyo'
    }
  }

  private updateFall(): void {
    this.stage.applyFall()
    this.mode = 'checkFall'
  }

  private updateCheckErase(): void {
    // 消去チェックの処理
    // 後のイテレーションで実装
  }

  private updateErasing(): void {
    // 消去処理
    // 後のイテレーションで実装
  }

  private updateGameOver(): void {
    // ゲームオーバー処理
    // 後のイテレーションで実装
  }

  private placePuyoPair(): void {
    const currentPair = this.player.getCurrentPair()
    const mainX = currentPair.getX()
    const mainY = currentPair.getY()
    const [subX, subY] = currentPair.getSubPosition()

    // ステージにぷよを配置
    this.stage.setPuyo(mainX, mainY, new Puyo(currentPair.getMainColor(), mainX, mainY))
    this.stage.setPuyo(subX, subY, new Puyo(currentPair.getSubColor(), subX, subY))
  }
}
