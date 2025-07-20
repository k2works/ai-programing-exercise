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
  private config!: Config
  private puyoImage!: PuyoImage
  private stage!: Stage
  private player!: Player
  private score!: Score
  private erasableGroups: [number, number][][] | null = null

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
    
    // ゲームオーバー判定：新しいぷよが配置できるかチェック
    if (!this.canPlaceNewPuyo()) {
      this.mode = 'gameOver'
      return
    }
    
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
      this.mode = 'checkErase'
    }
  }

  private updateFall(): void {
    this.stage.applyFall()
    this.mode = 'checkFall'
  }

  private updateCheckErase(): void {
    const erasableGroups = this.stage.findErasableGroups()
    if (erasableGroups.length > 0) {
      this.erasableGroups = erasableGroups
      this.mode = 'erasing'
    } else {
      // 連鎖終了
      this.combinationCount = 0
      this.mode = 'newPuyo'
    }
  }

  private updateErasing(): void {
    if (this.erasableGroups) {
      const erasedCount = this.stage.erasePuyos(this.erasableGroups)
      this.combinationCount++
      
      // 連鎖倍率を計算してスコア加算
      const baseScore = erasedCount * 10
      const chainMultiplier = this.calculateChainMultiplier(this.combinationCount)
      this.score.addScore(baseScore * chainMultiplier)
      
      this.erasableGroups = null
    }
    this.mode = 'checkFall'
  }

  private updateGameOver(): void {
    // ゲームオーバー状態では何もしない
  }

  private canPlaceNewPuyo(): boolean {
    const currentPair = this.player.getCurrentPair()
    const mainX = currentPair.getX()
    const mainY = currentPair.getY()
    const [subX, subY] = currentPair.getSubPosition()

    // メインぷよとサブぷよの位置が空いているかチェック
    return this.stage.isEmpty(mainX, mainY) && this.stage.isEmpty(subX, subY)
  }

  private calculateChainMultiplier(chainCount: number): number {
    // 連鎖倍率: 1連鎖=1倍、2連鎖=2倍、3連鎖=4倍、4連鎖=8倍...
    if (chainCount <= 1) return 1
    return Math.pow(2, chainCount - 1)
  }

  reset(): void {
    this.mode = 'start'
    this.frame = 0
    this.combinationCount = 0
    this.erasableGroups = null
    
    // 各コンポーネントを再初期化
    this.stage.initialize()
    this.player.initialize()
    this.score = new Score()
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
